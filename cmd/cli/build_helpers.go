package cli

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"unicode"

	zlog "github.com/rs/zerolog/log"

	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/minir"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/support/cache"
	"github.com/anthonyabeo/obx/src/support/compiler"
)

type buildToolchain struct {
	assembler     string
	assemblerArgs []string
	linker        string
	linkerArgs    []string
}

func canonicalBuildTarget(targetName string) string {
	return strings.ToLower(strings.TrimSpace(targetName))
}

func buildToolchainFor(mach target.Target) (buildToolchain, error) {
	switch canonicalBuildTarget(mach.Name()) {
	case target.RV64IMAFDName:
		return buildToolchain{
			assembler: "riscv64-linux-gnu-gcc",
			linker:    "riscv64-linux-gnu-gcc",
		}, nil
	case target.Arm64Name, target.Arm64AppleMacosName, target.AArch64AppleDarwinName:
		return buildToolchain{
			assembler:     "clang",
			assemblerArgs: []string{"-arch", "arm64"},
			linker:        "clang",
			linkerArgs:    []string{"-arch", "arm64"},
		}, nil
	default:
		return buildToolchain{}, fmt.Errorf("target %q is not supported by obx build yet", mach.Name())
	}
}

func defaultExecutableBase(projectName string) string {
	name := normalizeExecutableBase(projectName)
	if name == "" {
		name = "obx"
	}
	return name
}

func normalizeExecutableBase(name string) string {
	name = strings.TrimSpace(name)
	if name == "" {
		return "obx"
	}

	var b strings.Builder
	lastUnderscore := false
	for _, r := range name {
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r):
			b.WriteRune(r)
			lastUnderscore = false
		case r == '-' || r == '_' || r == '.' || unicode.IsSpace(r):
			if !lastUnderscore {
				b.WriteRune('_')
				lastUnderscore = true
			}
		default:
			if !lastUnderscore {
				b.WriteRune('_')
				lastUnderscore = true
			}
		}
	}

	out := strings.Trim(b.String(), "_")
	if out == "" {
		return "obx"
	}
	return out
}

func defaultExecutableName(manifest project.Manifest, targetName string) string {
	base := defaultExecutableBase(manifest.Name)
	if strings.Contains(canonicalBuildTarget(targetName), "windows") && !strings.HasSuffix(strings.ToLower(base), ".exe") {
		return base + ".exe"
	}
	return base
}

func resolveOutputPath(projectDir string, manifest project.Manifest, targetName, explicit string) string {
	if strings.TrimSpace(explicit) == "" {
		return filepath.Join(projectDir, "build", defaultExecutableName(manifest, targetName))
	}

	if filepath.IsAbs(explicit) {
		return explicit
	}

	if strings.ContainsRune(explicit, filepath.Separator) {
		return filepath.Join(projectDir, explicit)
	}

	return filepath.Join(projectDir, "build", explicit)
}

// tryLoadBundle attempts to load a single .obxi bundle from path.
// srcData is the raw source bytes used for staleness validation — if the
// stored SHA-256 prefix does not match, cache.ErrStale is returned by
// LoadBundle and the function returns false so the caller falls back to
// re-parsing the source.
func tryLoadBundle(
	path string,
	srcData []byte,
	ctx *compiler.Context,
	preBundles map[string]*minir.Module,
	loadedNames map[string]bool,
	h project.Header,
) bool {
	if _, err := os.Stat(path); err != nil {
		return false
	}
	b, err := cache.LoadBundle(path, srcData)
	if err != nil {
		if errors.Is(err, cache.ErrStale) {
			zlog.Warn().
				Str("module", h.Key.Name()).
				Str("path", path).
				Msg("build: precompiled bundle is stale (source changed); falling back to parse")
		}
		return false
	}
	ctx.Env.AddModuleScope(b.ModuleName, b.Scope)
	preBundles[b.ModuleName] = b.Module
	loadedNames[h.Key.Name()] = true
	zlog.Info().
		Str("module", b.ModuleName).
		Str("path", path).
		Msg("build: precompiled module loaded; will skip parsing")
	return true
}

// loadPrecompiledBundles looks for sibling/cache .obxi bundles and returns the
// pre-lowered minir modules plus the names we should skip during parsing.
//
// Probe order for each module header:
//  1. Sibling .obxi  — same directory as the .obx file (e.g. stdlib/IO.obxi).
//  2. Stdlib cache   — stdlibRoot/cache[/<subdir>]/Name.obxi, matching the
//     layout written by "obx precompile-stdlib". This is the primary location
//     for FFI subdirectory bundles (e.g. stdlibRoot/cache/posix/Stdio.obxi).
//  3. Legacy project cache — <dir>/cache/Name.obxi, for user-project caches
//     created outside the stdlib tree.
//
// Each probe passes the source file content to LoadBundle so that the embedded
// SHA-256 hash is validated. A stale bundle is skipped (logged as a warning),
// and the module falls back to normal parse + sema.
func loadPrecompiledBundles(
	ctx *compiler.Context,
	sorted []project.Header,
	stdlibRoot string,
) (map[string]*minir.Module, map[string]bool) {
	preBundles := make(map[string]*minir.Module)
	loadedNames := make(map[string]bool)

	for _, h := range sorted {
		obxPath := h.File
		// Read source once; used for staleness validation across all probes.
		srcData, _ := os.ReadFile(obxPath)
		baseName := filepath.Base(obxPath[:len(obxPath)-len(".obx")])

		// ── Probe 1: sibling .obxi ────────────────────────────────────────
		obxiPath := obxPath[:len(obxPath)-len(".obx")] + ".obxi"
		if tryLoadBundle(obxiPath, srcData, ctx, preBundles, loadedNames, h) {
			continue
		}

		// ── Probe 2: stdlibRoot/cache[/<subdir>]/Name.obxi ────────────────
		// Matches the output layout of "obx precompile-stdlib":
		//   stdlib/IO.obx        → stdlib/cache/IO.obxi
		//   stdlib/posix/Stdio.obx → stdlib/cache/posix/Stdio.obxi
		if stdlibRoot != "" {
			rel, err := filepath.Rel(stdlibRoot, filepath.Dir(obxPath))
			if err == nil && !strings.HasPrefix(rel, "..") {
				var cp string
				if rel == "." {
					cp = filepath.Join(stdlibRoot, "cache", baseName+".obxi")
				} else {
					cp = filepath.Join(stdlibRoot, "cache", rel, baseName+".obxi")
				}
				if tryLoadBundle(cp, srcData, ctx, preBundles, loadedNames, h) {
					continue
				}
			}
		}

		// ── Probe 3: legacy <dir>/cache/Name.obxi ─────────────────────────
		legacyCache := filepath.Join(filepath.Dir(obxPath), "cache", baseName+".obxi")
		tryLoadBundle(legacyCache, srcData, ctx, preBundles, loadedNames, h)
	}
	return preBundles, loadedNames
}

// splitParsedHeaders returns the headers that still need parsing and the
// module names skipped because a precompiled bundle was already loaded.
func splitParsedHeaders(sorted []project.Header, loadedNames map[string]bool) ([]project.Header, []string) {
	var toParse []project.Header
	var skipped []string
	for _, h := range sorted {
		if loadedNames[h.Key.Name()] {
			skipped = append(skipped, h.Key.Name())
			continue
		}
		toParse = append(toParse, h)
	}
	return toParse, skipped
}

// mergePrecompiledMinirModules appends missing precompiled minir modules to the
// lowered program so later passes and codegen can see them.
func mergePrecompiledMinirModules(lowered *minir.Program, preBundles map[string]*minir.Module) {
	if len(preBundles) == 0 {
		return
	}
	exist := make(map[string]bool)
	for _, m := range lowered.Modules {
		exist[m.Name] = true
	}
	for name, mod := range preBundles {
		if exist[name] {
			continue
		}
		// Precompiled modules are never the program entry point; clear the flag
		// so their __init_<Name> functions are NOT renamed to _main.
		mod.IsEntry = false
		lowered.Modules = append(lowered.Modules, mod)
	}
}

// dedupMinirExternals canonicalises extern declarations across the lowered
// program so repeated references share the same external function object.
func dedupMinirExternals(lowered *minir.Program) {
	uniq := make(map[string]*minir.ExternalFunc)
	for _, m := range lowered.Modules {
		newExts := make([]*minir.ExternalFunc, 0, len(m.Externals))
		seen := make(map[string]bool)
		for _, e := range m.Externals {
			key := externalFuncKey(e)
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

func externalFuncKey(e *minir.ExternalFunc) string {
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

func backendTargetFor(name string) (target.Target, error) {
	switch name {
	case target.RV64IMAFDName, target.RISCVAliasName, target.RISCV64AliasName:
		return target.Lookup(target.RV64IMAFDName)
	case target.Arm64AppleMacosName, target.Arm64Name, target.AArch64AppleDarwinName:
		return target.Lookup(target.Arm64Name)
	}
	return target.Lookup(name)
}
