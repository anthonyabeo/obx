package cli

// stdlib_build_test.go — integration test for the full stdlib → user-program
// compilation pipeline.
//
// The test drives every stage of the front-end (discovery, parse, sema,
// desugar, MIR lowering) for examples/stdlib/StdlibDemo.obx, which imports
// all eight stdlib modules.  It uses the precompiled .obxi bundles when
// present, so the run is fast, but the pipeline stays correct even if the
// cache is absent (falls back to full parse).
//
// Prerequisites:
//
//	OBX_STDLIB=/path/to/stdlib          # stdlib root
//
// OBX_STDLIB is used instead of the obx.mod manifest field so tests are
// isolated from any project-local manifest configuration.
//
// Run with:
//
//	OBX_STDLIB=/path/to/stdlib go test ./cmd/cli/... -run TestStdlibBuild -v

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/minir"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// TestStdlibBuildPipeline exercises the complete front-end + MIR pipeline for
// StdlibDemo.obx (examples/stdlib/), which imports all eight stdlib modules.
func TestStdlibBuildPipeline(t *testing.T) {
	stdlibRoot := os.Getenv("OBX_STDLIB")
	if stdlibRoot == "" {
		t.Skip("OBX_STDLIB not set; skipping stdlib build integration test")
	}
	if _, err := os.Stat(stdlibRoot); err != nil {
		t.Skipf("OBX_STDLIB=%s does not exist: %v", stdlibRoot, err)
	}

	// Derive the examples directory from the stdlib root.
	// Convention: OBX_STDLIB = <project_root>/stdlib
	projectRoot := filepath.Dir(stdlibRoot)
	examplesDir := filepath.Join(projectRoot, "examples", "stdlib")
	if _, err := os.Stat(examplesDir); err != nil {
		t.Skipf("examples/stdlib not found at %s: %v", examplesDir, err)
	}

	// Override OBX_STDLIB so bootstrapFrontEnd's ResolveStdlibRoot picks it up
	// ahead of any project-local obx.mod manifest field.
	t.Setenv("OBX_STDLIB", stdlibRoot)

	// ── 1. Bootstrap ─────────────────────────────────────────────────────
	// Use arm64-apple-macos (the host on Apple Silicon); on other hosts the
	// platform directives will select POSIX=true LINUX=true which is equally
	// valid for testing the stdlib integration.
	ctx, _ := newContext(32)
	injectPlatformDirectives(ctx, target.Arm64AppleMacosName)

	roots := []string{stdlibRoot, examplesDir}

	// ── 2. Module discovery ───────────────────────────────────────────────
	sorted, graph, err := resolveModules(ctx, roots...)
	if err != nil {
		t.Fatalf("resolveModules: %v", err)
	}

	sorted, err = project.ReachableFrom(sorted, graph, "StdlibDemo")
	if err != nil {
		t.Fatalf("ReachableFrom(StdlibDemo): %v", err)
	}

	if len(sorted) == 0 {
		t.Fatal("no modules discovered — check OBX_STDLIB and examples/stdlib paths")
	}
	t.Logf("discovered %d module(s):", len(sorted))
	for _, h := range sorted {
		t.Logf("  %s  %s", h.Key, h.File)
	}

	// ── 3. Load precompiled bundles ───────────────────────────────────────
	preBundles, loadedNames := loadPrecompiledBundles(ctx, sorted, stdlibRoot)
	t.Logf("loaded %d precompiled bundle(s)", len(preBundles))

	toParse, skipped := splitParsedHeaders(sorted, loadedNames)
	if len(skipped) > 0 {
		t.Logf("skipping parse for precompiled: %v", skipped)
	}

	// ── 4. Parse ──────────────────────────────────────────────────────────
	obx := ast.NewOberonX()
	if ok := parseModules(toParse, ctx, obx); !ok {
		t.Fatalf("parse failed: %d error(s)", ctx.Reporter.ErrorCount())
	}

	// ── 5. Semantic analysis ──────────────────────────────────────────────
	s := sema.NewSema(ctx, obx)
	s.Validate()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("sema failed: %d error(s)", ctx.Reporter.ErrorCount())
	}

	// ── 6. Desugar + MIR lowering ─────────────────────────────────────────
	hirProg := desugar.NewGenerator(obx, ctx).Generate()
	lowered := minir.New(ctx).Lower(hirProg)
	mergePrecompiledMinirModules(lowered, preBundles)
	dedupMinirExternals(lowered)

	if len(lowered.Modules) == 0 {
		t.Fatal("expected at least one lowered MIR module")
	}
	t.Logf("MIR: lowered %d module(s)", len(lowered.Modules))

	// Verify StdlibDemo itself is present in the output.
	found := false
	for _, m := range lowered.Modules {
		if m.Name == "StdlibDemo" {
			found = true
			t.Logf("StdlibDemo: %d function(s), %d global(s), %d external(s)",
				len(m.Functions), len(m.Globals), len(m.Externals))
		}
	}
	if !found {
		t.Error("StdlibDemo module not found in lowered MIR output")
	}
}

// TestStdlibStaleBundleFallback verifies that when a precompiled .obxi bundle
// is stale (source file was modified after the bundle was written), the
// pipeline falls back to parsing the source instead of erroring out.
func TestStdlibStaleBundleFallback(t *testing.T) {
	stdlibRoot := os.Getenv("OBX_STDLIB")
	if stdlibRoot == "" {
		t.Skip("OBX_STDLIB not set; skipping stale-bundle fallback test")
	}

	// Check that at least one precompiled bundle exists to make this test
	// meaningful.
	ioBundle := filepath.Join(stdlibRoot, "cache", "IO.obxi")
	if _, err := os.Stat(ioBundle); err != nil {
		t.Skipf("precompiled IO.obxi not found at %s; run obx precompile-stdlib first", ioBundle)
	}

	t.Setenv("OBX_STDLIB", stdlibRoot)

	ctx, _ := newContext(32)
	injectPlatformDirectives(ctx, target.Arm64AppleMacosName)

	roots := []string{stdlibRoot}
	sorted, _, err := resolveModules(ctx, roots...)
	if err != nil {
		t.Fatalf("resolveModules: %v", err)
	}

	// Temporarily write a different bundle that will produce a hash mismatch.
	bundleData, err := os.ReadFile(ioBundle)
	if err != nil {
		t.Fatalf("read IO.obxi: %v", err)
	}
	// Flip one byte in the hash region (bytes 8-15) to simulate staleness.
	corrupt := make([]byte, len(bundleData))
	copy(corrupt, bundleData)
	if len(corrupt) > 9 {
		corrupt[9] ^= 0xFF
	}
	if err := os.WriteFile(ioBundle, corrupt, 0o644); err != nil {
		t.Fatalf("write corrupt bundle: %v", err)
	}
	t.Cleanup(func() { _ = os.WriteFile(ioBundle, bundleData, 0o644) })

	// loadPrecompiledBundles must NOT load the IO module (stale) and must NOT
	// crash.  It should log a warning and leave IO out of loadedNames.
	preBundles, loadedNames := loadPrecompiledBundles(ctx, sorted, stdlibRoot)

	if loadedNames["IO"] {
		t.Error("expected IO to be skipped due to stale bundle, but it was loaded")
	}
	_ = preBundles // other modules may still load fine
	t.Log("stale bundle correctly rejected; IO will be re-parsed")
}
