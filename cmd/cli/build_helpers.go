package cli

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"unicode"

	zlog "github.com/rs/zerolog/log"

	"github.com/anthonyabeo/obx/src/codegen/target"
	"github.com/anthonyabeo/obx/src/ir/minir"
	"github.com/anthonyabeo/obx/src/project"
	minircache "github.com/anthonyabeo/obx/src/support/cache"
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

func buildToolchainFor(mach target.Machine) (buildToolchain, error) {
	switch canonicalBuildTarget(mach.Name()) {
	case "rv64imafd":
		return buildToolchain{
			assembler: "riscv64-linux-gnu-gcc",
			linker:    "riscv64-linux-gnu-gcc",
		}, nil
	case "arm64-apple-macos", "aarch64-apple-darwin":
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

// loadPrecompiledBundles looks for sibling/cache .obxi bundles and returns the
// pre-lowered minir modules plus the names we should skip during parsing.
func loadPrecompiledBundles(ctx *compiler.Context, sorted []project.Header) (map[string]*minir.Module, map[string]bool) {
	preBundles := make(map[string]*minir.Module)
	loadedNames := make(map[string]bool)
	for _, h := range sorted {
		// Attempt to load precompiled .obxi bundles for discovered modules.
		obxPath := h.File
		obxiPath := obxPath[:len(obxPath)-len(".obx")] + ".obxi"
		if _, err := os.Stat(obxiPath); err == nil {
			if b, err := minircache.LoadBundle(obxiPath, nil); err == nil {
				ctx.Env.AddModuleScope(b.ModuleName, b.Scope)
				preBundles[b.ModuleName] = b.Module
				loadedNames[h.Key.Name()] = true
				zlog.Info().Str("module", b.ModuleName).Str("path", obxiPath).Msg("build: precompiled module loaded; will skip parsing")
				continue
			}
		}
		cachePath := filepath.Join(filepath.Dir(obxPath), "cache", filepath.Base(obxPath[:len(obxPath)-len(".obx")]+".obxi"))
		if _, err := os.Stat(cachePath); err == nil {
			if b, err := minircache.LoadBundle(cachePath, nil); err == nil {
				ctx.Env.AddModuleScope(b.ModuleName, b.Scope)
				preBundles[b.ModuleName] = b.Module
				loadedNames[h.Key.Name()] = true
				zlog.Info().Str("module", b.ModuleName).Str("path", cachePath).Msg("build: precompiled module loaded; will skip parsing")
			}
		}
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
