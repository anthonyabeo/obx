package sema

// Tests for the FFI semantic analysis:
//   - CName / DLLName resolution from module/proc attribute lists
//   - ProcedureSymbol.IsExternal / IsVarArgs flags
//   - §12.4 constraint: VAR/IN params forbidden in extern procs
//   - §12.4 constraint: CARRAY by value forbidden

import (
	"testing"

	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

// ─── helpers ──────────────────────────────────────────────────────────────────

func newSemaFFICtx(t *testing.T) *compiler.Context {
	t.Helper()
	mgr := source.NewSourceManager()
	rep := diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
	return compiler.New("ffi.obx", mgr, rep, ast.NewEnv(), 8)
}

// parseAndValidateFFI parses a single extern DEFINITION source, runs name
// resolution and type-checking, and returns the resulting AST Definition.
func parseAndValidateFFI(t *testing.T, src string) (*ast.Definition, *compiler.Context) {
	t.Helper()
	ctx := newSemaFFICtx(t)

	p := parser.NewParser(ctx, "ffi.obx", []byte(src))
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("unexpected parse errors")
	}

	obx := ast.NewOberonX()
	obx.AddUnit(unit)

	s := NewSema(ctx, obx)
	s.Validate()

	return unit.(*ast.Definition), ctx
}

// ─── CName resolution ─────────────────────────────────────────────────────────

// TestFFISema_CNameFromProcName verifies that CName = modulePrefix + procName
// when no per-proc alias is set.
func TestFFISema_CNameFromProcName(t *testing.T) {
	src := `
DEFINITION MathLib [dll "libm"];

PROCEDURE sqrt(x: REAL): REAL;
PROCEDURE sin(x: REAL): REAL;

END MathLib.
`
	def, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("unexpected semantic errors")
	}

	for _, procName := range []string{"sqrt", "sin"} {
		sym := ctx.Env.LookupQualified("MathLib", procName)
		if sym == nil {
			t.Fatalf("symbol %q not found in MathLib scope", procName)
		}
		ps, ok := sym.(*ast.ProcedureSymbol)
		if !ok {
			t.Fatalf("%q: expected *ProcedureSymbol, got %T", procName, sym)
		}
		if ps.CName != procName {
			t.Errorf("%q: CName = %q, want %q", procName, ps.CName, procName)
		}
		if ps.DLLName != "libm" {
			t.Errorf("%q: DLLName = %q, want %q", procName, ps.DLLName, "libm")
		}
	}
	_ = def
}

// TestFFISema_CNameWithModulePrefix verifies CName = prefix + procName.
func TestFFISema_CNameWithModulePrefix(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc", prefix "c_"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	_, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("unexpected semantic errors")
	}

	sym := ctx.Env.LookupQualified("CLib", "puts")
	ps := sym.(*ast.ProcedureSymbol)

	if ps.CName != "c_puts" {
		t.Errorf("CName: got %q, want %q", ps.CName, "c_puts")
	}
}

// TestFFISema_CNameWithAlias verifies CName = prefix + alias (alias wins over
// proc name).
func TestFFISema_CNameWithAlias(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc", prefix "c_"];

PROCEDURE Quit(code: INTEGER) [alias "exit"];

END CLib.
`
	_, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("unexpected semantic errors")
	}

	sym := ctx.Env.LookupQualified("CLib", "Quit")
	ps := sym.(*ast.ProcedureSymbol)

	// CName should be prefix + alias = "c_exit"
	if ps.CName != "c_exit" {
		t.Errorf("CName: got %q, want %q", ps.CName, "c_exit")
	}
}

// TestFFISema_AliasWithoutPrefix verifies CName = alias when no prefix is set.
func TestFFISema_AliasWithoutPrefix(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE MySqrt(x: REAL): REAL [alias "sqrt"];

END CLib.
`
	_, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("unexpected semantic errors")
	}

	sym := ctx.Env.LookupQualified("CLib", "MySqrt")
	ps := sym.(*ast.ProcedureSymbol)

	if ps.CName != "sqrt" {
		t.Errorf("CName: got %q, want %q", ps.CName, "sqrt")
	}
}

// TestFFISema_PerProcDLLOverride verifies that a per-proc [dll] overrides the
// module-level dll.
func TestFFISema_PerProcDLLOverride(t *testing.T) {
	src := `
DEFINITION Mixed [dll "libfoo"];

PROCEDURE Foo(x: INTEGER);
PROCEDURE Bar(x: INTEGER) [dll "libbar"];

END Mixed.
`
	_, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("unexpected semantic errors")
	}

	fooSym := ctx.Env.LookupQualified("Mixed", "Foo").(*ast.ProcedureSymbol)
	barSym := ctx.Env.LookupQualified("Mixed", "Bar").(*ast.ProcedureSymbol)

	if fooSym.DLLName != "libfoo" {
		t.Errorf("Foo DLLName: got %q, want %q", fooSym.DLLName, "libfoo")
	}
	if barSym.DLLName != "libbar" {
		t.Errorf("Bar DLLName: got %q, want %q", barSym.DLLName, "libbar")
	}
}

// TestFFISema_PerProcPrefixOverride verifies that a per-proc [prefix] overrides
// the module-level prefix.
func TestFFISema_PerProcPrefixOverride(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc", prefix "c_"];

PROCEDURE puts(s: INTEGER): INTEGER;
PROCEDURE MyWrite(s: INTEGER): INTEGER [prefix "x_"];

END CLib.
`
	_, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("unexpected semantic errors")
	}

	putsSym := ctx.Env.LookupQualified("CLib", "puts").(*ast.ProcedureSymbol)
	writeSym := ctx.Env.LookupQualified("CLib", "MyWrite").(*ast.ProcedureSymbol)

	if putsSym.CName != "c_puts" {
		t.Errorf("puts CName: got %q, want %q", putsSym.CName, "c_puts")
	}
	if writeSym.CName != "x_MyWrite" {
		t.Errorf("MyWrite CName: got %q, want %q", writeSym.CName, "x_MyWrite")
	}
}

// ─── IsExternal / IsVarArgs flags ─────────────────────────────────────────────

// TestFFISema_IsExternalFlag verifies that ProcedureSymbol.IsExternal is true
// for procedures declared in extern DEFINITIONs.
func TestFFISema_IsExternalFlag(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	_, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("unexpected semantic errors")
	}

	sym := ctx.Env.LookupQualified("CLib", "puts")
	ps, ok := sym.(*ast.ProcedureSymbol)
	if !ok {
		t.Fatalf("expected *ProcedureSymbol, got %T", sym)
	}
	if !ps.IsExternal {
		t.Error("IsExternal should be true for extern procedure")
	}
}

// TestFFISema_IsVarArgsFlag verifies that ProcedureSymbol.IsVarArgs is set from
// [varargs].
func TestFFISema_IsVarArgsFlag(t *testing.T) {
	src := `
DEFINITION IO [dll "libc"];

PROCEDURE printf(fmt: INTEGER): INTEGER [varargs];

END IO.
`
	_, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("unexpected semantic errors")
	}

	sym := ctx.Env.LookupQualified("IO", "printf")
	ps := sym.(*ast.ProcedureSymbol)

	if !ps.IsVarArgs {
		t.Error("IsVarArgs should be true for [varargs] procedure")
	}
}

// TestFFISema_IsVarArgsAbsent verifies that IsVarArgs stays false when
// [varargs] is not present.
func TestFFISema_IsVarArgsAbsent(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	_, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("unexpected semantic errors")
	}

	sym := ctx.Env.LookupQualified("CLib", "puts")
	ps := sym.(*ast.ProcedureSymbol)

	if ps.IsVarArgs {
		t.Error("IsVarArgs should be false when [varargs] is absent")
	}
}

// ─── §12.4 constraint checks ──────────────────────────────────────────────────

// TestFFISema_VARParamForbiddenInExternProc verifies that a VAR-qualified param
// in an extern proc is rejected.
func TestFFISema_VARParamForbiddenInExternProc(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE BadFn(VAR x: INTEGER);

END CLib.
`
	ctx := newSemaFFICtx(t)
	p := parser.NewParser(ctx, "ffi.obx", []byte(src))
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("unexpected parse errors")
	}

	obx := ast.NewOberonX()
	obx.AddUnit(unit)

	s := NewSema(ctx, obx)
	s.Validate()

	if ctx.Reporter.ErrorCount() == 0 {
		t.Error("expected error for VAR param in extern proc, but got none")
	}
}

// TestFFISema_INParamForbiddenInExternProc verifies that an IN-qualified param
// is also rejected.
func TestFFISema_INParamForbiddenInExternProc(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE BadFn(IN x: INTEGER);

END CLib.
`
	ctx := newSemaFFICtx(t)
	p := parser.NewParser(ctx, "ffi.obx", []byte(src))
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("unexpected parse errors")
	}

	obx := ast.NewOberonX()
	obx.AddUnit(unit)

	s := NewSema(ctx, obx)
	s.Validate()

	if ctx.Reporter.ErrorCount() == 0 {
		t.Error("expected error for IN param in extern proc, but got none")
	}
}

// TestFFISema_ValidExternProcNoErrors verifies that a well-formed extern
// DEFINITION with value parameters passes validation without errors.
func TestFFISema_ValidExternProcNoErrors(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE abs(x: INTEGER): INTEGER;
PROCEDURE puts(s: INTEGER): INTEGER;
PROCEDURE printf(fmt: INTEGER): INTEGER [varargs];

END CLib.
`
	_, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Error("unexpected errors for valid extern DEFINITION")
	}
}

// TestFFISema_ExternProcWithNoParams verifies that a parameter-less extern
// proc is valid.
func TestFFISema_ExternProcWithNoParams(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE getchar(): INTEGER;

END CLib.
`
	_, ctx := parseAndValidateFFI(t, src)
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Error("unexpected errors for extern proc with no params")
	}
}
