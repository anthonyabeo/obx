package sema

// TestStdlib validates that every stdlib module (POSIX/Darwin layer) parses
// and type-checks cleanly when compiled for a macOS / POSIX target.
//
// Each TestStdlib_<Module> test exercises one module in isolation together
// with the POSIX DEFINITION files it requires.  TestStdlib_All runs every
// module plus the demo program in a single compilation unit.

import (
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

// ── helpers ───────────────────────────────────────────────────────────────────

// stdlibRoot returns the absolute path to stdlib/ relative to this test file.
func stdlibRoot(t *testing.T) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller failed")
	}
	// thisFile = …/src/sema/stdlib_test.go  →  project root = ../../
	abs, err := filepath.Abs(filepath.Join(filepath.Dir(thisFile), "..", "..", "stdlib"))
	if err != nil {
		t.Fatalf("abs path: %v", err)
	}
	return abs
}

// examplesRoot returns the absolute path to examples/ relative to this test file.
func examplesRoot(t *testing.T) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller failed")
	}
	abs, err := filepath.Abs(filepath.Join(filepath.Dir(thisFile), "..", "..", "examples"))
	if err != nil {
		t.Fatalf("abs path: %v", err)
	}
	return abs
}

// newDarwinCtx creates a compiler.Context configured for macOS / POSIX target.
// WINDOWS=false means <* IF WINDOWS THEN *> blocks are skipped and the POSIX
// <* ELSE *> branch is selected.
func newDarwinCtx(t *testing.T) *compiler.Context {
	t.Helper()
	mgr := source.NewSourceManager()
	rep := diag.NewBufferedReporter(mgr, 100,
		diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
	ctx := compiler.New("stdlib_test", mgr, rep, ast.NewEnv(), 8)
	ctx.SetDirective("POSIX", true)
	ctx.SetDirective("DARWIN", true)
	ctx.SetDirective("LINUX", false)
	ctx.SetDirective("WINDOWS", false)
	return ctx
}

type srcUnit struct {
	name string
	src  []byte
}

// readUnit reads a file from the filesystem; fatals on error.
func readUnit(t *testing.T, dir, rel string) srcUnit {
	t.Helper()
	data, err := os.ReadFile(filepath.Join(dir, rel))
	if err != nil {
		t.Fatalf("read %s: %v", rel, err)
	}
	return srcUnit{name: rel, src: data}
}

// parseAndValidate parses all units in order, runs sema over all of them, and
// reports any errors via t.Error (so the test keeps running after the first
// failure rather than calling Fatal).
func parseAndValidate(t *testing.T, ctx *compiler.Context, units []srcUnit) {
	t.Helper()

	obx := ast.NewOberonX()
	for _, u := range units {
		p := parser.NewParser(ctx, u.name, u.src)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatalf("parse errors in %s", u.name)
		}
		obx.AddUnit(unit)
	}

	s := NewSema(ctx, obx)
	s.Validate()

	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Errorf("sema errors (%d)", ctx.Reporter.ErrorCount())
	}
}

// posixLayer returns all POSIX DEFINITION files in the order they must be
// parsed (all are mutually independent, but we keep a stable order).
func posixLayer(t *testing.T, root string) []srcUnit {
	t.Helper()
	files := []string{
		"posix/Stdio.obx",
		"posix/Stdlib.obx",
		"posix/StringH.obx",
		"posix/LibM.obx",
		"posix/TimeH.obx",
		"posix/Unistd.obx",
	}
	units := make([]srcUnit, len(files))
	for i, f := range files {
		units[i] = readUnit(t, root, f)
	}
	return units
}

// ── per-module tests ──────────────────────────────────────────────────────────

// TestStdlib_IO checks that IO.obx (which imports posix.Stdio) passes sema.
func TestStdlib_IO(t *testing.T) {
	root := stdlibRoot(t)
	ctx := newDarwinCtx(t)
	units := append(posixLayer(t, root), readUnit(t, root, "IO.obx"))
	parseAndValidate(t, ctx, units)
}

// TestStdlib_Strings checks that Strings.obx (posix.StringH) passes sema.
func TestStdlib_Strings(t *testing.T) {
	root := stdlibRoot(t)
	ctx := newDarwinCtx(t)
	units := append(posixLayer(t, root), readUnit(t, root, "Strings.obx"))
	parseAndValidate(t, ctx, units)
}

// TestStdlib_Math checks that Math.obx (posix.LibM) passes sema.
func TestStdlib_Math(t *testing.T) {
	root := stdlibRoot(t)
	ctx := newDarwinCtx(t)
	units := append(posixLayer(t, root), readUnit(t, root, "Math.obx"))
	parseAndValidate(t, ctx, units)
}

// TestStdlib_Fmt checks that Fmt.obx (posix.Stdio) passes sema.
func TestStdlib_Fmt(t *testing.T) {
	root := stdlibRoot(t)
	ctx := newDarwinCtx(t)
	units := append(posixLayer(t, root), readUnit(t, root, "Fmt.obx"))
	parseAndValidate(t, ctx, units)
}

// TestStdlib_Files checks that Files.obx (posix.Stdio) passes sema.
func TestStdlib_Files(t *testing.T) {
	root := stdlibRoot(t)
	ctx := newDarwinCtx(t)
	units := append(posixLayer(t, root), readUnit(t, root, "Files.obx"))
	parseAndValidate(t, ctx, units)
}

// TestStdlib_OS checks that OS.obx (posix.Stdlib, posix.Unistd) passes sema.
func TestStdlib_OS(t *testing.T) {
	root := stdlibRoot(t)
	ctx := newDarwinCtx(t)
	units := append(posixLayer(t, root), readUnit(t, root, "OS.obx"))
	parseAndValidate(t, ctx, units)
}

// TestStdlib_Mem checks that Mem.obx (posix.Stdlib, posix.StringH) passes sema.
func TestStdlib_Mem(t *testing.T) {
	root := stdlibRoot(t)
	ctx := newDarwinCtx(t)
	units := append(posixLayer(t, root), readUnit(t, root, "Mem.obx"))
	parseAndValidate(t, ctx, units)
}

// TestStdlib_Time checks that Time.obx (posix.TimeH) passes sema.
func TestStdlib_Time(t *testing.T) {
	root := stdlibRoot(t)
	ctx := newDarwinCtx(t)
	units := append(posixLayer(t, root), readUnit(t, root, "Time.obx"))
	parseAndValidate(t, ctx, units)
}

// TestStdlib_All validates the complete stdlib + the StdlibDemo example
// program in a single compilation unit.  All POSIX DEFINITION files are parsed
// first, then the stdlib modules in dependency order, and finally the demo.
func TestStdlib_All(t *testing.T) {
	slRoot := stdlibRoot(t)
	exRoot := examplesRoot(t)
	ctx := newDarwinCtx(t)

	units := posixLayer(t, slRoot)

	// Stdlib modules — none depend on each other (all depend only on posix defs).
	for _, f := range []string{
		"IO.obx",
		"Strings.obx",
		"Math.obx",
		"Fmt.obx",
		"Files.obx",
		"OS.obx",
		"Mem.obx",
		"Time.obx",
	} {
		units = append(units, readUnit(t, slRoot, f))
	}

	// StdlibDemo imports all stdlib modules.
	units = append(units, readUnit(t, exRoot, "stdlib/StdlibDemo.obx"))

	parseAndValidate(t, ctx, units)
}
