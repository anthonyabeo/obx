package cli

// precompile.go — "obx precompile-stdlib" command.
//
// Compiles every .obx file under stdlibRoot into a .obxi bundle so
// downstream builds can skip reparsing and re-lowering the stdlib.
//
// # Two-phase approach
//
// Phase 1 — FFI subdirectories (posix/, win32/, …)
//
//	Each immediate subdirectory of stdlibRoot that contains .obx files is
//	compiled in its own fresh compiler context.  This avoids scope name
//	collisions: both posix/Stdio.obx and win32/Stdio.obx declare
//	"DEFINITION Stdio", but they live in separate compilation namespaces.
//	Bundles are written to stdlib/cache/<subdir>/<Name>.obxi.
//
// Phase 2 — Pure Oberon+ source modules (IO.obx, Math.obx, …)
//
//	Only the .obx files directly under stdlibRoot (non-recursive) are
//	compiled here.  The Phase-1 scopes for the host platform are injected
//	into the context before compilation so that
//	  `import S := posix.Stdio`
//	resolves correctly.  sema.VisitImport resolves by the bare module name
//	("Stdio"), so Phase-1 scopes are registered under both the qualified
//	key ("posix.Stdio") and the bare name ("Stdio") for the active platform.
//	Bundles are written to stdlib/cache/<Name>.obxi.
//
// After this command the legacy stdlib/cache/*.def stubs are no longer
// required (they were hand-maintained duplicates of the posix/*.obx files).

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	zlog "github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/support/cache"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/directive"
)

var precompileCmd = &cobra.Command{
	Use:   "precompile-stdlib",
	Short: "Precompile all stdlib .obx files into .obxi bundles",
	Run:   runPrecompile,
}

// ffiBundle holds the precompiled artifacts for one FFI module (DEFINITION file).
type ffiBundle struct {
	scope  *ast.LexicalScope
	module *minir.Module
}

func init() {} // registered by cli.go / Run().

// ── entry point ───────────────────────────────────────────────────────────────

func runPrecompile(_ *cobra.Command, _ []string) {
	stdlibRoot := os.Getenv("OBX_STDLIB")
	if stdlibRoot == "" {
		stdlibRoot = project.ResolveStdlibRoot(project.Manifest{})
	}
	if stdlibRoot == "" {
		log.Fatalf("cannot determine stdlib root; set OBX_STDLIB or provide manifest")
	}
	cacheDir := filepath.Join(stdlibRoot, "cache")
	if err := os.MkdirAll(cacheDir, 0o755); err != nil {
		log.Fatalf("create cache dir %s: %v", cacheDir, err)
	}

	// ── Phase 1: FFI subdirectories ───────────────────────────────────────
	// Collect: qualifiedKey ("posix.Stdio") → scope + module, for Phase 2.
	phase1 := make(map[string]ffiBundle) // key = "subdir.Name"

	subdirs, err := subdirectoriesOf(stdlibRoot)
	if err != nil {
		log.Fatalf("list subdirs of %s: %v", stdlibRoot, err)
	}
	for _, subdir := range subdirs {
		name := filepath.Base(subdir)
		if name == "cache" {
			continue // skip output directory
		}
		compiled := compileSubdir(subdir, cacheDir, name)
		for modName, e := range compiled {
			phase1[name+"."+modName] = e
		}
	}

	// ── Phase 2: root MODULE files ────────────────────────────────────────
	compileRootModules(stdlibRoot, cacheDir, phase1)
}

// ── Phase 1 helper ─────��──────────────────────────────────────────────────────

// compileSubdir compiles every .obx file directly inside subdir (all of them
// are DEFINITION files — FFI bindings) in a fresh compiler context, saves
// bundles to cacheDir/subdirName/<Name>.obxi, and returns the produced
// scope+module map keyed by module name (bare, e.g. "Stdio").
func compileSubdir(
	subdir, cacheDir, subdirName string,
) map[string]ffiBundle {
	result := make(map[string]ffiBundle)

	entries, err := os.ReadDir(subdir)
	if err != nil {
		zlog.Error().Err(err).Str("dir", subdir).Msg("read subdir")
		return result
	}
	var obxFiles []string
	for _, e := range entries {
		if !e.IsDir() && filepath.Ext(e.Name()) == ".obx" {
			obxFiles = append(obxFiles, filepath.Join(subdir, e.Name()))
		}
	}
	if len(obxFiles) == 0 {
		return result
	}

	ctx, _ := newContext(100)
	injectPlatformDirectives(ctx, platformForSubdir(subdirName))
	res := directive.ResolverFromContext(ctx)

	// Scan headers (using subdir as the root so keys are bare: "Stdio" not "posix.Stdio").
	var headers []project.Header
	for _, f := range obxFiles {
		h, err := project.ScanHeaderWithResolver(subdir, f, res)
		if err != nil {
			zlog.Error().Err(err).Str("file", f).Msg("scan header")
			continue
		}
		headers = append(headers, h)
	}
	if len(headers) == 0 {
		return result
	}

	// Topo-sort within the subdir.
	graph, err := project.BuildImportGraph(headers)
	if err != nil {
		zlog.Error().Err(err).Str("subdir", subdir).Msg("import graph")
		return result
	}
	sorted, err := project.TopoSort(graph)
	if err != nil {
		zlog.Error().Err(err).Str("subdir", subdir).Msg("topo sort")
		return result
	}

	obx := ast.NewOberonX()
	if ok := parseModules(sorted, ctx, obx); !ok {
		log.Printf("precompile: parse errors in %s — skipping subdir", subdir)
		return result
	}

	s := sema.NewSema(ctx, obx)
	s.Validate()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		log.Printf("precompile: sema errors in %s — skipping subdir", subdir)
		return result
	}

	hirProg := desugar.NewGenerator(obx, ctx).Generate()
	mirProg := minir.New(ctx).Lower(hirProg)

	// ── 6. Run optimization passes ─────────────────────────────────────
	pm := miniropt.NewPassManager()

	// Configure passes based on CLI flags
	if buildArgs.EnablePasses != "" {
		// Explicit pass list overrides -O level
		if err := pm.ConfigureFromPassList(buildArgs.EnablePasses); err != nil {
			log.Fatalf("build: invalid pass list: %v", err)
		}
	} else {
		// Use passes for the selected optimization level
		pm.ConfigureFromLevel(buildArgs.OptLevel)
	}

	// Apply disable-passes filter
	if buildArgs.DisablePasses != "" {
		if err := pm.DisablePasses(buildArgs.DisablePasses); err != nil {
			log.Fatalf("build: invalid disable-passes list: %v", err)
		}
	}

	// Enable verbose output if requested
	if buildArgs.Verbose {
		pm.SetVerbose(true)
		pm.SetLogWriter(os.Stdout)
	}

	// Run passes on the entire program
	totalChanges := pm.RunOnProgram(mirProg)
	if buildArgs.Verbose {
		stats := pm.Stats()
		fmt.Printf("Passes applied: %d total changes across %d pass invocations\n", totalChanges, len(stats))
	}

	// Save bundles and collect results.
	outSubdir := filepath.Join(cacheDir, subdirName)
	if err := os.MkdirAll(outSubdir, 0o755); err != nil {
		zlog.Error().Err(err).Str("dir", outSubdir).Msg("create cache subdir")
		return result
	}

	// Build a map header.Key.Name() → source file path for hash computation.
	modToFile := make(map[string]string)
	for _, h := range sorted {
		modToFile[h.Key.Name()] = h.File
	}

	for _, mod := range mirProg.Modules {
		scope := ctx.Env.ModuleScope(mod.Name)
		if scope == nil {
			zlog.Warn().Str("module", mod.Name).Msg("precompile: no sema scope; skipping")
			continue
		}
		srcContent := readFileOrName(modToFile[mod.Name], mod.Name)
		outPath := filepath.Join(outSubdir, mod.Name+".obxi")
		if err := cache.SaveBundle(outPath, mod.Name, srcContent, scope, mod); err != nil {
			zlog.Error().Err(err).Str("module", mod.Name).Msg("save bundle")
			continue
		}
		fmt.Printf("wrote %s  (externals=%d globals=%d)\n",
			outPath, len(mod.Externals), len(mod.Globals))
		result[mod.Name] = ffiBundle{scope: scope, module: mod}
	}
	return result
}

// ── Phase 2 helper ────────────────────────────────────────────────────────────

// compileRootModules compiles all .obx files directly in stdlibRoot (one level
// only — intentionally non-recursive so FFI subdirectory files are excluded).
// phase1 provides pre-compiled FFI scopes and modules to inject before sema.
func compileRootModules(
	stdlibRoot, cacheDir string,
	phase1 map[string]ffiBundle,
) {
	ctx, _ := newContext(100)
	injectPlatformDirectives(ctx, runtime.GOOS+"/"+runtime.GOARCH)

	// Inject Phase-1 FFI scopes.
	// sema.VisitImport resolves by the bare module name (last segment of the
	// import path), so register each scope under:
	//   • the bare name alone      ("Stdio")      — for unqualified lookups
	//   • the qualified key        ("posix.Stdio") — for defensive lookup
	// On a POSIX host we only inject "posix.*" under the bare name so that
	// win32.Stdio does not accidentally shadow posix.Stdio via "Stdio".
	platformPrefix := hostPlatformPrefix() // "posix" or "win32"
	for qualKey, e := range phase1 {
		ctx.Env.AddModuleScope(qualKey, e.scope) // always register qualified
		parts := strings.SplitN(qualKey, ".", 2)
		if len(parts) == 2 && parts[0] == platformPrefix {
			ctx.Env.AddModuleScope(parts[1], e.scope) // bare name for host platform only
		}
	}

	// Discover ALL .obx files under stdlibRoot (recursive) so that
	// BuildImportGraph can resolve cross-directory imports (e.g. IO → posix.Stdio).
	// We then filter to parse only the root-level MODULE files; the subdir
	// DEFINITION files are already compiled and their scopes are in ctx.Env.
	allSorted, _, err := resolveModules(ctx, stdlibRoot)
	if err != nil {
		log.Fatalf("resolve stdlib modules: %v", err)
	}

	// Keep only headers whose .obx file sits directly in stdlibRoot (depth 1).
	var rootHeaders []project.Header
	for _, h := range allSorted {
		if filepath.Dir(h.File) == stdlibRoot {
			rootHeaders = append(rootHeaders, h)
		}
	}
	if len(rootHeaders) == 0 {
		fmt.Println("no root-level .obx files found in", stdlibRoot)
		return
	}

	obx := ast.NewOberonX()
	if ok := parseModules(rootHeaders, ctx, obx); !ok {
		log.Fatalf("precompile: parse errors in root stdlib modules")
	}

	s := sema.NewSema(ctx, obx)
	s.Validate()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		log.Fatalf("sema errors in root stdlib modules")
	}

	hirProg := desugar.NewGenerator(obx, ctx).Generate()

	// Register Phase-1 pure DEFINITION module names (DLLName=="") as "no-init"
	// modules. FFI DEFINITION modules (DLLName != "") have a backend-synthesised
	// __init_X function and must NOT be skipped — their init populates module
	// globals (e.g. Stdio$stdin/stdout/stderr) before importers read them.
	noInit := make(map[string]bool)
	for _, e := range phase1 {
		if e.module.DLLName == "" {
			noInit[e.module.Name] = true
		}
	}
	lowerer := minir.New(ctx)
	lowerer.SetNoInitModules(noInit)
	mirProg := lowerer.Lower(hirProg)

	// ── 6. Run optimization passes ─────────────────────────────────────
	pm := miniropt.NewPassManager()

	// Configure passes based on CLI flags
	if buildArgs.EnablePasses != "" {
		// Explicit pass list overrides -O level
		if err := pm.ConfigureFromPassList(buildArgs.EnablePasses); err != nil {
			log.Fatalf("build: invalid pass list: %v", err)
		}
	} else {
		// Use passes for the selected optimization level
		pm.ConfigureFromLevel(buildArgs.OptLevel)
	}

	// Apply disable-passes filter
	if buildArgs.DisablePasses != "" {
		if err := pm.DisablePasses(buildArgs.DisablePasses); err != nil {
			log.Fatalf("build: invalid disable-passes list: %v", err)
		}
	}

	// Enable verbose output if requested
	if buildArgs.Verbose {
		pm.SetVerbose(true)
		pm.SetLogWriter(os.Stdout)
	}

	// Run passes on the entire program
	totalChanges := pm.RunOnProgram(mirProg)
	if buildArgs.Verbose {
		stats := pm.Stats()
		fmt.Printf("Passes applied: %d total changes across %d pass invocations\n", totalChanges, len(stats))
	}

	modToFile := make(map[string]string)
	for _, h := range rootHeaders {
		modToFile[h.Key.Name()] = h.File
	}

	for _, mod := range mirProg.Modules {
		scope := ctx.Env.ModuleScope(mod.Name)
		if scope == nil {
			zlog.Warn().Str("module", mod.Name).Msg("precompile: no sema scope; skipping")
			continue
		}
		srcContent := readFileOrName(modToFile[mod.Name], mod.Name)
		outPath := filepath.Join(cacheDir, mod.Name+".obxi")
		if err := cache.SaveBundle(outPath, mod.Name, srcContent, scope, mod); err != nil {
			zlog.Error().Err(err).Str("module", mod.Name).Msg("save bundle")
			continue
		}
		fmt.Printf("wrote %s  (funcs=%d globals=%d externals=%d)\n",
			outPath, len(mod.Functions), len(mod.Globals), len(mod.Externals))
	}
}

// ── utilities ─────────────────────────────────────────────────────────────────

// subdirectoriesOf returns the absolute paths of all immediate subdirectories
// of dir (one level deep, non-recursive), excluding the "cache" output dir.
// Both "posix" and "win32" are included regardless of the host OS so that
// precompile-stdlib always produces bundles for every supported platform.
func subdirectoriesOf(dir string) ([]string, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil, err
	}

	var out []string
	for _, e := range entries {
		if e.IsDir() && e.Name() != "cache" {
			out = append(out, filepath.Join(dir, e.Name()))
		}
	}

	return out, nil
}

// hostPlatformPrefix returns the stdlib subdirectory name for the host OS.
func hostPlatformPrefix() string {
	switch runtime.GOOS {
	case "windows":
		return "win32"
	default:
		return "posix" // darwin, linux, freebsd, …
	}
}

// platformForSubdir maps a stdlib FFI subdirectory name to the target platform
// string understood by injectPlatformDirectives.  This lets each FFI layer be
// compiled with the correct directive context regardless of the host OS.
func platformForSubdir(subdirName string) string {
	switch strings.ToLower(subdirName) {
	case "win32":
		return "windows"
	default:
		return "linux" // posix: covers linux, darwin, etc. for DEFINITION files
	}
}

// readFileOrName reads path if non-empty and returns the content; otherwise
// returns the module name as a trivial byte slice (used as the hash seed).
func readFileOrName(path, fallback string) []byte {
	if path != "" {
		if data, err := os.ReadFile(path); err == nil {
			return data
		}
	}
	return []byte(fallback)
}
