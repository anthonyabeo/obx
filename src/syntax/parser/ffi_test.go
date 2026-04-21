package parser

// Tests for the FFI (Foreign Function Interface) parser support.
// Covers §12.1 (extern DEFINITION modules), §12.4 (extern proc headings),
// attribute lists (dll, prefix, alias, varargs).

import (
	"testing"

	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// newFFICtx returns a fresh compiler context wired to a text-formatter reporter.
func newFFICtx(t *testing.T, fileName string) *compiler.Context {
	t.Helper()
	mgr := source.NewSourceManager()
	rep := diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
	return compiler.New(fileName, mgr, rep, ast.NewEnv(), 8)
}

// ─── Helpers ──────────────────────────────────────────────────────────────────

func parseFFIUnit(t *testing.T, src string) (ast.CompilationUnit, *compiler.Context) {
	t.Helper()
	ctx := newFFICtx(t, "ffi_test.obx")
	p := NewParser(ctx, "ffi_test.obx", []byte(src))
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("unexpected parse errors")
	}
	return unit, ctx
}

func assertNoErrors(t *testing.T, ctx *compiler.Context) {
	t.Helper()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Errorf("expected no errors but got %d", ctx.Reporter.ErrorCount())
	}
}

// ─── Tests ────────────────────────────────────────────────────────────────────

// TestFFIParse_BasicExternDefinition verifies that a DEFINITION with a [dll]
// attribute is recognised as an extern module.
func TestFFIParse_BasicExternDefinition(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	unit, _ := parseFFIUnit(t, src)

	def, ok := unit.(*ast.Definition)
	if !ok {
		t.Fatalf("expected *ast.Definition, got %T", unit)
	}

	if def.BName != "CLib" {
		t.Errorf("module name: got %q, want %q", def.BName, "CLib")
	}
	if !def.IsExtern {
		t.Error("IsExtern should be true for extern DEFINITION")
	}
	if def.Attrs == nil {
		t.Fatal("Attrs should not be nil")
	}
	if dll := def.Attrs.DLLName(); dll != "libc" {
		t.Errorf("DLLName: got %q, want %q", dll, "libc")
	}
}

// TestFFIParse_PrefixAttribute verifies the [prefix "..."] module attribute.
func TestFFIParse_PrefixAttribute(t *testing.T) {
	src := `
DEFINITION MathLib [dll "libm", prefix "c_"];

PROCEDURE sqrt(x: REAL): REAL;

END MathLib.
`
	unit, _ := parseFFIUnit(t, src)
	def := unit.(*ast.Definition)

	if !def.IsExtern {
		t.Fatal("should be extern")
	}
	if p := def.Attrs.Prefix(); p != "c_" {
		t.Errorf("Prefix: got %q, want %q", p, "c_")
	}
	if d := def.Attrs.DLLName(); d != "libm" {
		t.Errorf("DLLName: got %q, want %q", d, "libm")
	}
}

// TestFFIParse_ProcedureHeadingInExternDefinition verifies that procedures
// declared inside an extern DEFINITION are parsed without a body.
// Note: per-proc attribute lists follow the formal-parameter list.
func TestFFIParse_ProcedureHeadingInExternDefinition(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;
PROCEDURE printf(fmt: INTEGER): INTEGER [varargs];

END CLib.
`
	unit, _ := parseFFIUnit(t, src)
	def := unit.(*ast.Definition)

	if len(def.DeclSeq) != 2 {
		t.Fatalf("expected 2 declarations, got %d", len(def.DeclSeq))
	}

	for _, d := range def.DeclSeq {
		proc, ok := d.(*ast.ProcedureDecl)
		if !ok {
			t.Errorf("expected *ast.ProcedureDecl, got %T", d)
			continue
		}
		// Extern procs must have no body.
		if proc.Body != nil {
			t.Errorf("proc %s: expected no body in extern DEFINITION", proc.Head.Name.Name)
		}
	}
}

// TestFFIParse_VarArgsAttribute verifies that [varargs] on a procedure heading
// is parsed and accessible via AttributeList.HasVarArgs().
// The per-proc attribute list follows the formal-parameter list.
func TestFFIParse_VarArgsAttribute(t *testing.T) {
	src := `
DEFINITION IO [dll "libc"];

PROCEDURE printf(fmt: INTEGER): INTEGER [varargs];

END IO.
`
	unit, _ := parseFFIUnit(t, src)
	def := unit.(*ast.Definition)

	if len(def.DeclSeq) == 0 {
		t.Fatal("no declarations found")
	}
	proc := def.DeclSeq[0].(*ast.ProcedureDecl)

	if proc.Head.Attrs == nil {
		t.Fatal("proc Attrs should not be nil")
	}
	if !proc.Head.Attrs.HasVarArgs() {
		t.Error("HasVarArgs should be true for [varargs] attribute")
	}
}

// TestFFIParse_AliasAttribute verifies that [alias "cname"] is parsed.
func TestFFIParse_AliasAttribute(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE Quit(code: INTEGER) [alias "exit"];

END CLib.
`
	unit, _ := parseFFIUnit(t, src)
	def := unit.(*ast.Definition)

	if len(def.DeclSeq) == 0 {
		t.Fatal("no declarations found")
	}
	proc := def.DeclSeq[0].(*ast.ProcedureDecl)
	if proc.Head.Attrs == nil {
		t.Fatal("proc Attrs should not be nil")
	}
	if alias := proc.Head.Attrs.Alias(); alias != "exit" {
		t.Errorf("Alias: got %q, want %q", alias, "exit")
	}
}

// TestFFIParse_PerProcDllOverride verifies a per-procedure [dll "..."] that
// overrides the module-level dll.
func TestFFIParse_PerProcDllOverride(t *testing.T) {
	src := `
DEFINITION Mixed [dll "libfoo"];

PROCEDURE Foo(x: INTEGER);
PROCEDURE Bar(x: INTEGER) [dll "libbar"];

END Mixed.
`
	unit, _ := parseFFIUnit(t, src)
	def := unit.(*ast.Definition)

	if len(def.DeclSeq) != 2 {
		t.Fatalf("expected 2 procs, got %d", len(def.DeclSeq))
	}

	bar := def.DeclSeq[1].(*ast.ProcedureDecl)
	if bar.Head.Name.Name != "Bar" {
		t.Fatalf("unexpected proc name: %s", bar.Head.Name.Name)
	}
	if bar.Head.Attrs == nil {
		t.Fatal("Bar Attrs should not be nil")
	}
	if dll := bar.Head.Attrs.DLLName(); dll != "libbar" {
		t.Errorf("Bar DLLName: got %q, want %q", dll, "libbar")
	}
}

// TestFFIParse_NonExternDefinition verifies that a bare DEFINITION (without
// attribute list) is NOT marked as extern.
func TestFFIParse_NonExternDefinition(t *testing.T) {
	src := `
DEFINITION Iface;

PROCEDURE Foo(x: INTEGER);

END Iface.
`
	unit, _ := parseFFIUnit(t, src)
	def := unit.(*ast.Definition)

	if def.IsExtern {
		t.Error("plain DEFINITION should not be marked IsExtern")
	}
	if def.Attrs != nil {
		t.Error("plain DEFINITION should have nil Attrs")
	}
}

// TestFFIParse_MultipleAttributes verifies that multiple attributes can be
// combined: [dll "x", prefix "p_"] on the module and [varargs, alias "..."] on a proc.
func TestFFIParse_MultipleAttributes(t *testing.T) {
	src := `
DEFINITION Multi [dll "libmulti", prefix "m_"];

PROCEDURE Func(x: INTEGER): INTEGER [varargs, alias "func"];

END Multi.
`
	unit, _ := parseFFIUnit(t, src)
	def := unit.(*ast.Definition)

	if def.Attrs.DLLName() != "libmulti" {
		t.Errorf("module DLLName: got %q", def.Attrs.DLLName())
	}
	if def.Attrs.Prefix() != "m_" {
		t.Errorf("module Prefix: got %q", def.Attrs.Prefix())
	}

	proc := def.DeclSeq[0].(*ast.ProcedureDecl)
	if !proc.Head.Attrs.HasVarArgs() {
		t.Error("proc should have varargs")
	}
	if proc.Head.Attrs.Alias() != "func" {
		t.Errorf("proc alias: got %q", proc.Head.Attrs.Alias())
	}
}
