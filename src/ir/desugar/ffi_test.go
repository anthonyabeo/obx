package desugar_test

// Tests for HIR lowering of extern DEFINITION modules.
// Verifies that IsExternal, Mangled (CName), DLLName, IsVarArgs and Body=nil
// are correctly propagated into desugar.Function nodes.

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

// ─── helpers ──────────────────────────────────────────────────────────────────

func newDesugarFFICtx(t *testing.T) *compiler.Context {
	t.Helper()
	mgr := source.NewSourceManager()
	rep := diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
	return compiler.New("ffi_hir.obx", mgr, rep, ast.NewEnv(), 8)
}

// lowerFFIDefinition parses src as an extern DEFINITION, runs sema, and
// returns the resulting desugar.Module.
func lowerFFIDefinition(t *testing.T, src string) *desugar.Module {
	t.Helper()
	ctx := newDesugarFFICtx(t)

	p := parser.NewParser(ctx, "ffi_hir.obx", []byte(src))
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("parse errors")
	}

	obx := ast.NewOberonX()
	obx.AddUnit(unit)

	s := sema.NewSema(ctx, obx)
	s.Validate()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("sema errors")
	}

	gen := desugar.NewGenerator(obx, ctx)
	prog := gen.Generate()
	if len(prog.Modules) != 1 {
		t.Fatalf("expected 1 module, got %d", len(prog.Modules))
	}
	return prog.Modules[0]
}

// ─── Tests ────────────────────────────────────────────────────────────────────

// TestFFIHIR_IsExternalFlag verifies that extern procs become desugar.Function
// nodes with IsExternal=true and Body=nil.
func TestFFIHIR_IsExternalFlag(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	mod := lowerFFIDefinition(t, src)

	if len(mod.Decls) != 1 {
		t.Fatalf("expected 1 decl, got %d", len(mod.Decls))
	}
	fn, ok := mod.Decls[0].(*desugar.Function)
	if !ok {
		t.Fatalf("expected *desugar.Function, got %T", mod.Decls[0])
	}

	if !fn.IsExternal {
		t.Error("IsExternal should be true for extern proc")
	}
	if fn.Body != nil {
		t.Error("Body should be nil for extern proc")
	}
}

// TestFFIHIR_MangledNameIsCName verifies that Mangled is set to the resolved
// C symbol name (CName from sema).
func TestFFIHIR_MangledNameIsCName(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc", prefix "c_"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	mod := lowerFFIDefinition(t, src)

	fn := mod.Decls[0].(*desugar.Function)
	if fn.Mangled != "c_puts" {
		t.Errorf("Mangled: got %q, want %q", fn.Mangled, "c_puts")
	}
}

// TestFFIHIR_MangledWithAlias verifies Mangled = prefix + alias.
func TestFFIHIR_MangledWithAlias(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc", prefix "c_"];

PROCEDURE Quit(code: INTEGER) [alias "exit"];

END CLib.
`
	mod := lowerFFIDefinition(t, src)

	fn := mod.Decls[0].(*desugar.Function)
	if fn.Mangled != "c_exit" {
		t.Errorf("Mangled: got %q, want %q", fn.Mangled, "c_exit")
	}
}

// TestFFIHIR_DLLNamePropagated verifies that DLLName is carried from sema into
// the HIR function node.
func TestFFIHIR_DLLNamePropagated(t *testing.T) {
	src := `
DEFINITION CLib [dll "libm"];

PROCEDURE sqrt(x: REAL): REAL;

END CLib.
`
	mod := lowerFFIDefinition(t, src)

	fn := mod.Decls[0].(*desugar.Function)
	if fn.DLLName != "libm" {
		t.Errorf("DLLName: got %q, want %q", fn.DLLName, "libm")
	}
}

// TestFFIHIR_IsVarArgsPropagated verifies IsVarArgs flows from sema to HIR.
func TestFFIHIR_IsVarArgsPropagated(t *testing.T) {
	src := `
DEFINITION IO [dll "libc"];

PROCEDURE printf(fmt: INTEGER): INTEGER [varargs];

END IO.
`
	mod := lowerFFIDefinition(t, src)

	fn := mod.Decls[0].(*desugar.Function)
	if !fn.IsVarArgs {
		t.Error("IsVarArgs should be true for [varargs] extern proc")
	}
}

// TestFFIHIR_NonVarArgsProcIsNotVariadic verifies that a non-varargs proc
// stays IsVarArgs=false.
func TestFFIHIR_NonVarArgsProcIsNotVariadic(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	mod := lowerFFIDefinition(t, src)

	fn := mod.Decls[0].(*desugar.Function)
	if fn.IsVarArgs {
		t.Error("IsVarArgs should be false for non-varargs proc")
	}
}

// TestFFIHIR_ExternModuleHasNoInit verifies that the desugar.Module produced
// from an extern DEFINITION has a nil Init field (no initializer body).
func TestFFIHIR_ExternModuleHasNoInit(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	mod := lowerFFIDefinition(t, src)

	if mod.Init != nil {
		t.Error("extern DEFINITION module should have nil Init (no initializer)")
	}
}

// TestFFIHIR_MultipleProcedures verifies that all extern procs are lowered.
func TestFFIHIR_MultipleProcedures(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;
PROCEDURE getchar(): INTEGER;
PROCEDURE printf(fmt: INTEGER): INTEGER [varargs];

END CLib.
`
	mod := lowerFFIDefinition(t, src)

	if len(mod.Decls) != 3 {
		t.Fatalf("expected 3 decls, got %d", len(mod.Decls))
	}
	for i, d := range mod.Decls {
		fn, ok := d.(*desugar.Function)
		if !ok {
			t.Errorf("decl[%d]: expected *desugar.Function, got %T", i, d)
			continue
		}
		if !fn.IsExternal {
			t.Errorf("decl[%d] %q: IsExternal should be true", i, fn.Name)
		}
		if fn.Body != nil {
			t.Errorf("decl[%d] %q: Body should be nil", i, fn.Name)
		}
	}
}

// TestFFIHIR_PerProcDLLOverride verifies per-proc DLLName override in HIR.
func TestFFIHIR_PerProcDLLOverride(t *testing.T) {
	src := `
DEFINITION Mixed [dll "libfoo"];

PROCEDURE Foo(x: INTEGER);
PROCEDURE Bar(x: INTEGER) [dll "libbar"];

END Mixed.
`
	mod := lowerFFIDefinition(t, src)

	if len(mod.Decls) != 2 {
		t.Fatalf("expected 2 decls, got %d", len(mod.Decls))
	}

	foo := mod.Decls[0].(*desugar.Function)
	bar := mod.Decls[1].(*desugar.Function)

	if foo.DLLName != "libfoo" {
		t.Errorf("Foo DLLName: got %q, want %q", foo.DLLName, "libfoo")
	}
	if bar.DLLName != "libbar" {
		t.Errorf("Bar DLLName: got %q, want %q", bar.DLLName, "libbar")
	}
}
