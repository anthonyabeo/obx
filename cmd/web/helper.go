package web

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"time"

	zlog "github.com/rs/zerolog/log"

	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/support/cache"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/directive"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

var (
	assetManifest     map[string]string
	assetManifestOnce sync.Once
	assetManifestErr  error
)

// loadAssetManifest reads static/manifest.json (if present) from the embedded
// static FS and returns a mapping of placeholder keys → replacement paths.
func loadAssetManifest() (map[string]string, error) {
	assetManifestOnce.Do(func() {
		data, err := staticFS.ReadFile("static/manifest.json")
		if err != nil {
			// manifest is optional
			assetManifest = nil
			assetManifestErr = nil
			return
		}
		m := make(map[string]string)
		if err := json.Unmarshal(data, &m); err != nil {
			assetManifestErr = err
			return
		}
		assetManifest = m
	})
	return assetManifest, assetManifestErr
}

// replacePlaceholders substitutes %KEY% placeholders in HTML with values from
// the asset manifest (if available). If no manifest is present the HTML is
// returned unchanged.
func replacePlaceholders(html string) string {
	m, _ := loadAssetManifest()
	if m == nil {
		return html
	}
	for k, v := range m {
		ph := "%" + k + "%"
		html = strings.ReplaceAll(html, ph, v)
	}
	return html
}

// statusRecorder wraps http.ResponseWriter to capture the response status code.
type statusRecorder struct {
	http.ResponseWriter
	status int
}

func (r *statusRecorder) WriteHeader(code int) {
	r.status = code
	r.ResponseWriter.WriteHeader(code)
}

func writeJSON(w http.ResponseWriter, status int, v any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	if err := json.NewEncoder(w).Encode(v); err != nil {
		zlog.Error().Err(err).Msg("writeJSON")
	}
}

// validateJWT validates a simple HS256 (HMAC-SHA256) JWT bearer token using the
// provided secret. It performs signature verification and optional exp claim
// expiry check. Returns true when token is valid.
func validateJWT(token string, secret string) bool {
	// token format: header.payload.signature (base64url)
	parts := strings.Split(token, ".")
	if len(parts) != 3 {
		return false
	}
	signing := parts[0] + "." + parts[1]
	sig, err := base64.RawURLEncoding.DecodeString(parts[2])
	if err != nil {
		// try standard URLEncoding with padding
		sig, err = base64.URLEncoding.DecodeString(parts[2])
		if err != nil {
			return false
		}
	}
	mac := hmac.New(sha256.New, []byte(secret))
	mac.Write([]byte(signing))
	expected := mac.Sum(nil)
	if !hmac.Equal(sig, expected) {
		return false
	}
	// parse payload and check exp if present
	payloadB, err := base64.RawURLEncoding.DecodeString(parts[1])
	if err != nil {
		payloadB, err = base64.URLEncoding.DecodeString(parts[1])
		if err != nil {
			return false
		}
	}
	var claims map[string]any
	if err := json.Unmarshal(payloadB, &claims); err != nil {
		return false
	}
	if expv, ok := claims["exp"]; ok {
		switch t := expv.(type) {
		case float64:
			if int64(t) < time.Now().Unix() {
				return false
			}
		case int64:
			if t < time.Now().Unix() {
				return false
			}
		case string:
			// try parse as integer string
			if iv, err := strconv.ParseInt(t, 10, 64); err == nil {
				if iv < time.Now().Unix() {
					return false
				}
			}
		}
	}
	return true
}

// ── shared helpers ────────────────────────────────────────────────────────────

// injectHostPlatformDirectives sets POSIX/WINDOWS/DARWIN/LINUX based on the
// OS the server is currently running on.
func injectHostPlatformDirectives(ctx *compiler.Context) {
	switch runtime.GOOS {
	case "windows":
		ctx.SetDirective("WINDOWS", true)
		ctx.SetDirective("POSIX", false)
		ctx.SetDirective("LINUX", false)
		ctx.SetDirective("DARWIN", false)
	case "darwin":
		ctx.SetDirective("POSIX", true)
		ctx.SetDirective("DARWIN", true)
		ctx.SetDirective("LINUX", false)
		ctx.SetDirective("WINDOWS", false)
	default:
		ctx.SetDirective("POSIX", true)
		ctx.SetDirective("LINUX", true)
		ctx.SetDirective("DARWIN", false)
		ctx.SetDirective("WINDOWS", false)
	}
}

// prepareStdlibUnits discovers stdlib units, loads any precompiled .obxi
// bundles, and parses the remaining headers into the provided OberonX program.
// It returns the map of preloaded minir modules (keyed by module name) so
// callers that need lowering can merge them without re-lowering.
func prepareStdlibUnits(ctx *compiler.Context, obx *ast.OberonX, entry, userFilename, userSource string) (map[string]*minir.Module, error) {
	roots, cleanup, err := buildRoots(userFilename, userSource)
	if err != nil {
		return nil, err
	}
	if len(roots) == 0 {
		return nil, nil
	}
	if cleanup != nil {
		defer cleanup()
	}

	// Build resolver and compute sorted stdlib headers reachable from entry.
	sorted, err := buildSortedStdlibHeaders(roots, ctx, entry)
	if err != nil {
		// Be tolerant in environments where a stdlib root was configured but
		// is not present on disk (e.g. some test sandboxes). Log and continue
		// without stdlib rather than failing the entire request.
		zlog.Debug().Err(err).Msg("web: stdlib discovery failed — continuing without stdlib")
		return nil, nil
	}

	// Print sorted stdlib file list for debugging/visibility.
	for i, hdr := range sorted {
		zlog.Info().Str("file", filepath.Base(hdr.File)).Int("idx", i).Msg("web: stdlib sorted")
	}

	// Load precompiled .obxi bundles where available; collect which names
	// were satisfied so we can skip parsing them.
	loadedNames, preBundles := loadStdlibBundles(sorted, ctx)

	// Parse only the headers that were not covered by a bundle.
	toParse := filterToParse(sorted, loadedNames)
	if len(toParse) > 0 {
		if err := parseProgramFiles(toParse, ctx, obx); err != nil {
			return nil, err
		}
	}

	return preBundles, nil
}

// loadStdlibBundles iterates over the sorted headers and, for each one, tries
// to load a precompiled .obxi bundle from:
//  1. a sibling file next to the .obx source (e.g. Math.obxi beside Math.obx)
//  2. a cache/ subdirectory next to the .obx source (e.g. cache/Math.obxi)
//
// On success the bundle's sema scope is injected into ctx.Env and the minir
// module is added to preBundles. The module name is added to loadedNames so
// callers can skip reparsing that header.
func loadStdlibBundles(sorted []project.Header, ctx *compiler.Context) (loadedNames map[string]bool, preBundles map[string]*minir.Module) {
	loadedNames = make(map[string]bool)
	preBundles = make(map[string]*minir.Module)
	for _, h := range sorted {
		obxPath := h.File

		// 1. sibling .obxi next to the source file
		obxiPath := obxPath[:len(obxPath)-len(".obx")] + ".obxi"
		if _, err := os.Stat(obxiPath); err == nil {
			if b, err := cache.LoadBundle(obxiPath, nil); err == nil {
				ctx.Env.AddModuleScope(b.ModuleName, b.Scope)
				preBundles[b.ModuleName] = b.Module
				loadedNames[h.Key.Name()] = true
				zlog.Info().Str("module", b.ModuleName).Str("path", obxiPath).
					Msg("web: precompiled stdlib module loaded")
				continue
			}
		}

		// 2. cache/<name>.obxi in the same directory
		cachePath := filepath.Join(
			filepath.Dir(obxPath), "cache",
			filepath.Base(obxPath[:len(obxPath)-len(".obx")]+".obxi"),
		)
		if _, err := os.Stat(cachePath); err == nil {
			if b, err := cache.LoadBundle(cachePath, nil); err == nil {
				ctx.Env.AddModuleScope(b.ModuleName, b.Scope)
				preBundles[b.ModuleName] = b.Module
				loadedNames[h.Key.Name()] = true
				zlog.Info().Str("module", b.ModuleName).Str("path", cachePath).
					Msg("web: precompiled stdlib module loaded from cache dir")
				continue
			}
		}

		// 3. global stdlib cache: <stdlibRoot>/cache/<relative/path/to/module>.obxi
		// This mirrors the CLI precompiler which emits bundles into the top-level
		// stdlib cache directory (e.g. stdlib/cache/posix/Stdio.obxi). Try this
		// location as a last resort when the sibling/cache-local paths fail.
		if stdlibRoot := findStdlibRoot(); stdlibRoot != "" {
			if rel, err := filepath.Rel(stdlibRoot, obxPath); err == nil {
				globalCachePath := filepath.Join(stdlibRoot, "cache", strings.TrimSuffix(rel, ".obx")+".obxi")
				if _, err := os.Stat(globalCachePath); err == nil {
					if b, err := cache.LoadBundle(globalCachePath, nil); err == nil {
						ctx.Env.AddModuleScope(b.ModuleName, b.Scope)
						preBundles[b.ModuleName] = b.Module
						loadedNames[h.Key.Name()] = true
						zlog.Info().Str("module", b.ModuleName).Str("path", globalCachePath).
							Msg("web: precompiled stdlib module loaded from global cache")
						continue
					}
				}
			}
		}
	}
	return
}

// filterToParse returns the subset of sorted headers whose module name does
// not appear in loadedNames (i.e. headers that still need to be parsed).
func filterToParse(sorted []project.Header, loadedNames map[string]bool) []project.Header {
	out := make([]project.Header, 0, len(sorted))
	for _, h := range sorted {
		if !loadedNames[h.Key.Name()] {
			out = append(out, h)
		}
	}
	return out
}

// buildRoots prepares an ordered list of roots where the resolver should
// search for modules. If userSource is provided, a temporary root is
// created (returned as cleanup) and placed before the stdlib root so the
// uploaded file overrides stdlib modules with the same name.
func buildRoots(userFilename, userSource string) (roots []string, cleanup func(), err error) {
	stdlibRoot := findStdlibRoot()
	if userSource != "" {
		td, cl, e := createTempRootWithUserFile(userFilename, userSource)
		if e != nil {
			return nil, nil, e
		}
		roots = append(roots, td)
		cleanup = cl
	}
	if stdlibRoot != "" {
		roots = append(roots, stdlibRoot)
	}
	return roots, cleanup, nil
}

// findStdlibRoot returns the stdlib root either from project.ResolveStdlibRoot
// or by walking upwards from cwd looking for a `stdlib` directory.
func findStdlibRoot() string {
	stdlibRoot := project.ResolveStdlibRoot(project.Manifest{})
	if stdlibRoot != "" {
		return stdlibRoot
	}
	if wd, err := os.Getwd(); err == nil {
		for dir := wd; ; dir = filepath.Dir(dir) {
			candidate := filepath.Join(dir, "stdlib")
			if info, err := os.Stat(candidate); err == nil && info.IsDir() {
				return candidate
			}
			parent := filepath.Dir(dir)
			if parent == dir {
				break
			}
		}
	}
	return ""
}

// createTempRootWithUserFile creates a temporary directory and writes the
// uploaded user source into it using filename (adds .obx if missing). It
// returns the temp dir path and a cleanup func to remove it.
func createTempRootWithUserFile(userFilename, userSource string) (string, func(), error) {
	td, err := os.MkdirTemp("", "obx-web-*")
	if err != nil {
		return "", nil, fmt.Errorf("create temp dir: %w", err)
	}
	// cleanup function
	cleanup := func() {
		if err := os.RemoveAll(td); err != nil {
			zlog.Error().Err(err).Str("dir", td).Msg("failed to remove temp dir")
		}
	}

	if filepath.Ext(userFilename) != ".obx" {
		userFilename = userFilename + ".obx"
	}
	target := filepath.Join(td, userFilename)
	if err := os.WriteFile(target, []byte(userSource), 0644); err != nil {
		cleanup()
		return "", nil, fmt.Errorf("write temp file: %w", err)
	}
	return td, cleanup, nil
}

// buildSortedStdlibHeaders creates a resolver from roots, discovers headers,
// builds an import graph, topo-sorts it and returns the reachable headers
// filtered from the given entry.
func buildSortedStdlibHeaders(roots []string, ctx *compiler.Context, entry string) ([]project.Header, error) {
	r := project.NewResolver(roots...)
	headers, err := r.DiscoverAllWithResolver(directive.ResolverFromContext(ctx))
	if err != nil {
		return nil, fmt.Errorf("stdlib discover: %w", err)
	}
	graph, err := project.BuildImportGraph(headers)
	if err != nil {
		return nil, fmt.Errorf("stdlib import graph: %w", err)
	}
	sorted, err := project.TopoSort(graph)
	if err != nil {
		return nil, fmt.Errorf("stdlib topo sort: %w", err)
	}
	sorted, err = project.ReachableFrom(sorted, graph, entry)
	if err != nil {
		return nil, err
	}
	return sorted, nil
}

// parseProgramFiles reads, parses and conditionally adds parsed units
// into the provided OberonX program. Files that produce parse diagnostics are
// skipped to avoid adding partially-parsed units.
func parseProgramFiles(sorted []project.Header, ctx *compiler.Context, obx *ast.OberonX) error {
	for _, header := range sorted {
		data, err := os.ReadFile(header.File)
		if err != nil {
			return fmt.Errorf("read %s: %w", header.File, err)
		}
		fileName := filepath.Base(header.File)
		content := data[header.StartPos:header.EndPos]

		p := parser.NewParser(ctx, fileName, content)
		unit := p.Parse()

		// If parsing produced diagnostics, flush them and return an error so
		// callers (e.g. the web handler) can report the failure instead of
		// silently continuing with partially-parsed units.
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			return fmt.Errorf("parse errors in %s", header.File)
		}

		obx.AddUnit(unit)
	}
	return nil
}

// deriveEntryFromFilename returns the module entry name from a filename
// e.g. "Main.obx" -> "Main". Falls back to "Main" when empty.
func deriveEntryFromFilename(fn string) string {
	if fn == "" {
		return "Main"
	}
	base := filepath.Base(fn)
	stem := strings.TrimSuffix(base, filepath.Ext(base))
	if stem == "" {
		return "Main"
	}
	return stem
}

// applyPipeline keeps web lowering behavior aligned with the CLI
// build pipeline: merge cached stdlib modules, deduplicate extern declarations,
// run the default optimization level, then verify the final program.
func applyPipeline(lowered *minir.Program, preBundles map[string]*minir.Module) []minir.VerifyError {
	if lowered == nil {
		return nil
	}
	mergePrecompiledMinirModulesWeb(lowered, preBundles)
	dedupMinirExternalsWeb(lowered)

	pm := miniropt.NewPassManager()
	pm.ConfigureFromLevel(2)
	pm.RunOnProgram(lowered)

	return minir.VerifyProgram(lowered)
}

func mergePrecompiledMinirModulesWeb(lowered *minir.Program, preBundles map[string]*minir.Module) {
	if lowered == nil || len(preBundles) == 0 {
		return
	}
	exist := make(map[string]bool, len(lowered.Modules))
	for _, m := range lowered.Modules {
		if m != nil {
			exist[m.Name] = true
		}
	}
	for name, mod := range preBundles {
		if mod == nil || exist[name] {
			continue
		}
		mod.IsEntry = false
		lowered.Modules = append(lowered.Modules, mod)
	}
}

func dedupMinirExternalsWeb(lowered *minir.Program) {
	if lowered == nil {
		return
	}
	uniq := make(map[string]*minir.ExternalFunc)
	for _, m := range lowered.Modules {
		if m == nil {
			continue
		}
		newExts := make([]*minir.ExternalFunc, 0, len(m.Externals))
		seen := make(map[string]bool)
		for _, e := range m.Externals {
			if e == nil {
				continue
			}
			key := externalFuncKeyWeb(e)
			if seen[key] {
				continue
			}
			seen[key] = true
			if ex, ok := uniq[key]; ok {
				if ex.Attrs == nil && e.Attrs != nil {
					ex.Attrs = e.Attrs
				}
				newExts = append(newExts, ex)
			} else {
				uniq[key] = e
				newExts = append(newExts, e)
			}
		}
		m.Externals = newExts
	}
}

func externalFuncKeyWeb(e *minir.ExternalFunc) string {
	cname := e.Name
	dll := ""
	vari := false
	if e.Attrs != nil {
		if e.Attrs.CName != "" {
			cname = e.Attrs.CName
		}
		dll = e.Attrs.DLLName
		vari = e.Attrs.Variadic
	}
	sig := ""
	if e.Sig != nil {
		parts := make([]string, 0, len(e.Sig.Params)+1)
		for _, p := range e.Sig.Params {
			if p == nil {
				parts = append(parts, "<nil>")
			} else {
				parts = append(parts, p.String())
			}
		}
		if vari {
			parts = append(parts, "...")
		}
		res := "void"
		if e.Sig.Result != nil {
			res = e.Sig.Result.String()
		}
		sig = res + "(" + strings.Join(parts, ",") + ")"
	}
	return cname + "|" + dll + "|" + sig
}
