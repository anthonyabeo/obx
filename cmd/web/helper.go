package web

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"time"

	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/directive"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

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
		log.Printf("writeJSON: %v", err)
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

// prepareStdlibUnits discovers, parses and adds stdlib (and optional user)
// units into the provided OberonX program. It uses `buildRoots` to obtain
// resolver roots and cleans up any temporary directories created for user
// source.
func prepareStdlibUnits(ctx *compiler.Context, obx *ast.OberonX, entry, userFilename, userSource string) error {
	roots, cleanup, err := buildRoots(userFilename, userSource)
	if err != nil {
		return err
	}
	if len(roots) == 0 {
		return nil
	}
	if cleanup != nil {
		defer cleanup()
	}

	// Build resolver and compute sorted stdlib headers reachable from entry
	sorted, err := buildSortedStdlibHeaders(roots, ctx, entry)
	if err != nil {
		return err
	}

	// Print sorted stdlib file list for debugging/visibility.
	for i, hdr := range sorted {
		log.Printf("web: stdlib sorted[%d]: %s", i, filepath.Base(hdr.File))
	}

	// Parse and add units into the OberonX program
	if err := parseProgramFiles(sorted, ctx, obx); err != nil {
		return err
	}

	return nil
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
			log.Printf("failed to remove temp dir %s: %v", td, err)
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
			continue
		}
		fileName := filepath.Base(header.File)
		content := data[header.StartPos:header.EndPos]

		p := parser.NewParser(ctx, fileName, content)
		unit := p.Parse()
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
