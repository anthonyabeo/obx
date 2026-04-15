package obxir_test

// Tests for IRBuilder FFI support:
//   - External functions produce IsExternal=true stubs with no blocks.
//   - module.Externals is populated with the correct CName / DLLName.
//   - External function stubs are registered in the module env so callers resolve.
//   - Extern DEFINITION module (no Init) does not panic.
//   - A MODULE that imports an extern DEFINITION and calls it lowers cleanly.

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/obxir"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

// ─── helpers ──────────────────────────────────────────────────────────────────

func newIRCtx(t *testing.T) *compiler.Context {
	t.Helper()
	mgr := source.NewSourceManager()
	rep := diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
	return compiler.New("ir_ffi.obx", mgr, rep, ast.NewEnv(), 8)
}

// buildMIRFromSources parses each (fileName, content) pair in order, runs
// sema over all units together, lowers to HIR and then to MIR.
func buildMIRFromSources(t *testing.T, ctx *compiler.Context, sources []struct{ name, src string }) *obxir.Program {
	t.Helper()

	obx := ast.NewOberonX()
	for _, s := range sources {
		p := parser.NewParser(ctx, s.name, []byte(s.src))
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatalf("parse errors in %s", s.name)
		}
		obx.AddUnit(unit)
	}

	sm := sema.NewSema(ctx, obx)
	sm.Validate()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("sema errors")
	}

	gen := desugar.NewGenerator(obx)
	hirProg := gen.Generate()

	builder := obxir.NewIRBuilder(ctx.Target.WordSize)
	return builder.Build(hirProg)
}

// ─── Extern DEFINITION only ───────────────────────────────────────────────────

// TestFFIIR_ExternDefinitionDoesNotPanic verifies that building MIR from an
// extern DEFINITION does not panic (the bug fix: nil Init, external funcs).
func TestFFIIR_ExternDefinitionDoesNotPanic(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	ctx := newIRCtx(t)
	prog := buildMIRFromSources(t, ctx, []struct{ name, src string }{
		{"CLib.obx", src},
	})

	if len(prog.Modules) != 1 {
		t.Fatalf("expected 1 module, got %d", len(prog.Modules))
	}
}

// TestFFIIR_ExternalsPopulated verifies that module.Externals contains entries
// for each extern proc in the DEFINITION.
func TestFFIIR_ExternalsPopulated(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;
PROCEDURE getchar(): INTEGER;

END CLib.
`
	ctx := newIRCtx(t)
	prog := buildMIRFromSources(t, ctx, []struct{ name, src string }{
		{"CLib.obx", src},
	})

	mod := prog.Modules[0]
	if len(mod.Externals) != 2 {
		t.Fatalf("Externals: expected 2 entries, got %d", len(mod.Externals))
	}

	// Build a name set for easy lookup.
	names := make(map[string]string)
	for _, e := range mod.Externals {
		names[e.CName] = e.DLLName
	}

	for _, want := range []string{"puts", "getchar"} {
		dll, ok := names[want]
		if !ok {
			t.Errorf("Externals: %q not found", want)
			continue
		}
		if dll != "libc" {
			t.Errorf("Externals[%q].DLLName: got %q, want %q", want, dll, "libc")
		}
	}
}

// TestFFIIR_ExternalsWithPrefix verifies CName = prefix + procName in Externals.
func TestFFIIR_ExternalsWithPrefix(t *testing.T) {
	src := `
DEFINITION MathLib [dll "libm", prefix "c_"];

PROCEDURE sqrt(x: REAL): REAL;

END MathLib.
`
	ctx := newIRCtx(t)
	prog := buildMIRFromSources(t, ctx, []struct{ name, src string }{
		{"MathLib.obx", src},
	})

	mod := prog.Modules[0]
	if len(mod.Externals) == 0 {
		t.Fatal("Externals should not be empty")
	}
	if mod.Externals[0].CName != "c_sqrt" {
		t.Errorf("CName: got %q, want %q", mod.Externals[0].CName, "c_sqrt")
	}
}

// TestFFIIR_ExternalFunctionIsExternal verifies that the obxir.Function stub
// has IsExternal=true.
func TestFFIIR_ExternalFunctionIsExternal(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	ctx := newIRCtx(t)
	prog := buildMIRFromSources(t, ctx, []struct{ name, src string }{
		{"CLib.obx", src},
	})

	mod := prog.Modules[0]
	var found *obxir.Function
	for _, fn := range mod.Funcs {
		if fn.FnName == "puts" {
			found = fn
			break
		}
	}
	if found == nil {
		t.Fatal("'puts' stub not found in module.Funcs")
	}
	if !found.IsExternal {
		t.Error("IsExternal should be true")
	}
}

// TestFFIIR_ExternalFunctionHasNoBlocks verifies that the body-less stub has
// no basic blocks (only external signature metadata).
func TestFFIIR_ExternalFunctionHasNoBlocks(t *testing.T) {
	src := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	ctx := newIRCtx(t)
	prog := buildMIRFromSources(t, ctx, []struct{ name, src string }{
		{"CLib.obx", src},
	})

	mod := prog.Modules[0]
	var found *obxir.Function
	for _, fn := range mod.Funcs {
		if fn.FnName == "puts" {
			found = fn
			break
		}
	}
	if found == nil {
		t.Fatal("'puts' stub not found")
	}
	if len(found.Blocks) != 0 {
		t.Errorf("external stub should have 0 blocks, got %d", len(found.Blocks))
	}
}

// TestFFIIR_ExternalFunctionVariadicFlag verifies that the stub carries the
// Variadic flag from IsVarArgs.
func TestFFIIR_ExternalFunctionVariadicFlag(t *testing.T) {
	src := `
DEFINITION IO [dll "libc"];

PROCEDURE printf(fmt: INTEGER): INTEGER [varargs];

END IO.
`
	ctx := newIRCtx(t)
	prog := buildMIRFromSources(t, ctx, []struct{ name, src string }{
		{"IO.obx", src},
	})

	mod := prog.Modules[0]
	var found *obxir.Function
	for _, fn := range mod.Funcs {
		if fn.FnName == "printf" {
			found = fn
			break
		}
	}
	if found == nil {
		t.Fatal("'printf' stub not found")
	}
	if !found.Variadic {
		t.Error("Variadic should be true for [varargs] extern proc")
	}
}

// TestFFIIR_ExternDLLNameInExternals verifies per-proc DLL override in Externals.
func TestFFIIR_ExternDLLNameInExternals(t *testing.T) {
	src := `
DEFINITION Mixed [dll "libfoo"];

PROCEDURE Foo(x: INTEGER);
PROCEDURE Bar(x: INTEGER) [dll "libbar"];

END Mixed.
`
	ctx := newIRCtx(t)
	prog := buildMIRFromSources(t, ctx, []struct{ name, src string }{
		{"Mixed.obx", src},
	})

	mod := prog.Modules[0]
	dllOf := make(map[string]string)
	for _, e := range mod.Externals {
		dllOf[e.CName] = e.DLLName
	}

	if dllOf["Foo"] != "libfoo" {
		t.Errorf("Foo DLLName: got %q, want %q", dllOf["Foo"], "libfoo")
	}
	if dllOf["Bar"] != "libbar" {
		t.Errorf("Bar DLLName: got %q, want %q", dllOf["Bar"], "libbar")
	}
}

// ─── Two-unit build: DEFINITION + MODULE ─────────────────────────────────────

// TestFFIIR_TwoUnitBuildDoesNotPanic verifies that a MODULE that imports an
// extern DEFINITION and calls its procedures lowers to MIR without panicking.
func TestFFIIR_TwoUnitBuildDoesNotPanic(t *testing.T) {
	defSrc := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	modSrc := `
MODULE Main;
IMPORT CLib;

BEGIN
  CLib.puts(0)
END Main.
`
	ctx := newIRCtx(t)
	prog := buildMIRFromSources(t, ctx, []struct{ name, src string }{
		{"CLib.obx", defSrc},
		{"Main.obx", modSrc},
	})

	if len(prog.Modules) != 2 {
		t.Fatalf("expected 2 modules, got %d", len(prog.Modules))
	}
}

// TestFFIIR_CallingModuleExternalsIsEmpty verifies that the calling MODULE has
// no Externals of its own (they live on the DEFINITION module).
func TestFFIIR_CallingModuleExternalsIsEmpty(t *testing.T) {
	defSrc := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	modSrc := `
MODULE Main;
IMPORT CLib;

BEGIN
  CLib.puts(0)
END Main.
`
	ctx := newIRCtx(t)
	prog := buildMIRFromSources(t, ctx, []struct{ name, src string }{
		{"CLib.obx", defSrc},
		{"Main.obx", modSrc},
	})

	var mainMod *obxir.Module
	for _, m := range prog.Modules {
		if m.Name == "Main" {
			mainMod = m
			break
		}
	}
	if mainMod == nil {
		t.Fatal("Main module not found")
	}

	// The Main module itself declares no foreign symbols.
	if len(mainMod.Externals) != 0 {
		t.Errorf("Main.Externals: expected 0, got %d", len(mainMod.Externals))
	}
}

// TestFFIIR_DefinitionModuleExternalsNonEmpty verifies the DEFINITION module
// carries its Externals when built alongside a calling MODULE.
func TestFFIIR_DefinitionModuleExternalsNonEmpty(t *testing.T) {
	defSrc := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;
PROCEDURE printf(fmt: INTEGER): INTEGER [varargs];

END CLib.
`
	modSrc := `
MODULE Main;
IMPORT CLib;

BEGIN
  CLib.puts(0)
END Main.
`
	ctx := newIRCtx(t)
	prog := buildMIRFromSources(t, ctx, []struct{ name, src string }{
		{"CLib.obx", defSrc},
		{"Main.obx", modSrc},
	})

	var clibMod *obxir.Module
	for _, m := range prog.Modules {
		if m.Name == "CLib" {
			clibMod = m
			break
		}
	}
	if clibMod == nil {
		t.Fatal("CLib module not found")
	}
	if len(clibMod.Externals) != 2 {
		t.Errorf("CLib.Externals: expected 2, got %d", len(clibMod.Externals))
	}
}




