package codegen

// Tests for assembly emission of FFI extern declarations.
// Verifies that:
//   - module.Externals is propagated to asmModule.Externals by Compile()
//   - RISC-V target emits `.extern <CName>` for each extern
//   - External (body-less) functions are skipped in instruction selection
//   - Variadic extern (e.g. printf) is handled without panic

import (
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/src/codegen/target/riscv"
	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/obxir"
	"github.com/anthonyabeo/obx/src/opt"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

// ─── helpers ──────────────────────────────────────────────────────────────────

func newCodegenFFICtx(t *testing.T) *compiler.Context {
	t.Helper()
	mgr := source.NewSourceManager()
	rep := diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
	return compiler.New("ffi_cg.obx", mgr, rep, ast.NewEnv(), 8)
}

// buildFFIProgramMIR parses the supplied (name, src) pairs, validates, and
// lowers to MIR, returning the resulting *obxir.Program.
func buildFFIProgramMIR(t *testing.T, ctx *compiler.Context, sources []struct{ name, src string }) *obxir.Program {
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

	gen := desugar.NewGenerator(obx, ctx)
	hirProg := gen.Generate()

	builder := obxir.NewIRBuilder(ctx.Target.WordSize)
	prog := builder.Build(hirProg, ctx)

	for _, module := range prog.Modules {
		for _, function := range module.Funcs {
			if function.IsExternal {
				continue // extern stubs have no blocks — skip CFG building
			}
			opt.BuildCFG(function)
		}
	}
	return prog
}

// compileModuleRV64 compiles a single obxir.Module to RISC-V assembly.
func compileModuleRV64(t *testing.T, module *obxir.Module) string {
	t.Helper()
	root, err := project.FindProjectRoot()
	if err != nil {
		t.Fatalf("project root: %v", err)
	}
	asm, err := Compile(module, riscv.NewRV64IMAFDTarget(),
		root+"/src/codegen/target/desc", CompileOptions{})
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	return asm
}

// ─── Tests ────────────────────────────────────────────────────────────────────

// TestFFICodegen_ExternDirectiveRISCV verifies that the RISC-V emitter produces
// a `.extern <CName>` line for each foreign symbol in the extern DEFINITION.
func TestFFICodegen_ExternDirectiveRISCV(t *testing.T) {
	defSrc := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	ctx := newCodegenFFICtx(t)
	prog := buildFFIProgramMIR(t, ctx, []struct{ name, src string }{
		{"CLib.obx", defSrc},
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

	asm := compileModuleRV64(t, clibMod)

	if !strings.Contains(asm, ".extern puts") {
		t.Errorf("expected '.extern puts' in RISC-V assembly, got:\n%s", asm)
	}
}

// TestFFICodegen_MultipleExternDirectives verifies that multiple extern procs
// each get their own `.extern` line.
func TestFFICodegen_MultipleExternDirectives(t *testing.T) {
	defSrc := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;
PROCEDURE getchar(): INTEGER;
PROCEDURE printf(fmt: INTEGER): INTEGER [varargs];

END CLib.
`
	ctx := newCodegenFFICtx(t)
	prog := buildFFIProgramMIR(t, ctx, []struct{ name, src string }{
		{"CLib.obx", defSrc},
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

	asm := compileModuleRV64(t, clibMod)

	for _, sym := range []string{"puts", "getchar", "printf"} {
		if !strings.Contains(asm, ".extern "+sym) {
			t.Errorf("expected '.extern %s' in RISC-V assembly\n%s", sym, asm)
		}
	}
}

// TestFFICodegen_ExternWithPrefix verifies that the prefixed CName (e.g. c_puts)
// is used in the `.extern` directive, not the OBX procedure name.
func TestFFICodegen_ExternWithPrefix(t *testing.T) {
	defSrc := `
DEFINITION CLib [dll "libc", prefix "c_"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	ctx := newCodegenFFICtx(t)
	prog := buildFFIProgramMIR(t, ctx, []struct{ name, src string }{
		{"CLib.obx", defSrc},
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

	asm := compileModuleRV64(t, clibMod)

	if !strings.Contains(asm, ".extern c_puts") {
		t.Errorf("expected '.extern c_puts', got:\n%s", asm)
	}
	// The un-prefixed name must NOT appear as an extern.
	if strings.Contains(asm, ".extern puts") {
		t.Errorf("un-prefixed '.extern puts' should not appear; got:\n%s", asm)
	}
}

// TestFFICodegen_ExternDirectiveWithAlias verifies that alias-renamed symbols
// emit the alias as the `.extern` name.
func TestFFICodegen_ExternDirectiveWithAlias(t *testing.T) {
	defSrc := `
DEFINITION CLib [dll "libc"];

PROCEDURE Quit(code: INTEGER) [alias "exit"];

END CLib.
`
	ctx := newCodegenFFICtx(t)
	prog := buildFFIProgramMIR(t, ctx, []struct{ name, src string }{
		{"CLib.obx", defSrc},
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

	asm := compileModuleRV64(t, clibMod)

	if !strings.Contains(asm, ".extern exit") {
		t.Errorf("expected '.extern exit', got:\n%s", asm)
	}
}

// TestFFICodegen_ExternModuleNoFunctionBodies verifies that an extern DEFINITION
// module produces no function bodies in the assembly output (only .extern lines).
func TestFFICodegen_ExternModuleNoFunctionBodies(t *testing.T) {
	defSrc := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;

END CLib.
`
	ctx := newCodegenFFICtx(t)
	prog := buildFFIProgramMIR(t, ctx, []struct{ name, src string }{
		{"CLib.obx", defSrc},
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

	asm := compileModuleRV64(t, clibMod)

	// No .globl or function prologue should be present for extern symbols.
	if strings.Contains(asm, ".globl puts") {
		t.Errorf("extern symbol 'puts' should not appear as .globl:\n%s", asm)
	}
}

// TestFFICodegen_VariadicExternDoesNotPanic verifies that compiling a module
// with a [varargs] extern proc (e.g. printf) does not panic.
func TestFFICodegen_VariadicExternDoesNotPanic(t *testing.T) {
	defSrc := `
DEFINITION IO [dll "libc"];

PROCEDURE printf(fmt: INTEGER): INTEGER [varargs];

END IO.
`
	ctx := newCodegenFFICtx(t)
	prog := buildFFIProgramMIR(t, ctx, []struct{ name, src string }{
		{"IO.obx", defSrc},
	})

	var ioMod *obxir.Module
	for _, m := range prog.Modules {
		if m.Name == "IO" {
			ioMod = m
			break
		}
	}
	if ioMod == nil {
		t.Fatal("IO module not found")
	}

	// Should not panic.
	asm := compileModuleRV64(t, ioMod)

	if !strings.Contains(asm, ".extern printf") {
		t.Errorf("expected '.extern printf' in output:\n%s", asm)
	}
}

// TestFFICodegen_AsmModuleExternalsPopulated verifies that asm.Module.Externals
// is populated from obxir.Module.Externals during Compile().
func TestFFICodegen_AsmModuleExternalsPopulated(t *testing.T) {
	defSrc := `
DEFINITION CLib [dll "libc"];

PROCEDURE puts(s: INTEGER): INTEGER;
PROCEDURE getchar(): INTEGER;

END CLib.
`
	ctx := newCodegenFFICtx(t)
	prog := buildFFIProgramMIR(t, ctx, []struct{ name, src string }{
		{"CLib.obx", defSrc},
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

	root, err := project.FindProjectRoot()
	if err != nil {
		t.Fatalf("project root: %v", err)
	}

	_, compileErr := Compile(clibMod, riscv.NewRV64IMAFDTarget(),
		root+"/src/codegen/target/desc", CompileOptions{})
	if compileErr != nil {
		t.Fatalf("Compile error: %v", compileErr)
	}

	// After Compile, module.Asm should have Externals propagated.
	if clibMod.Asm == nil {
		t.Fatal("module.Asm is nil after Compile")
	}
	if len(clibMod.Asm.Externals) != 2 {
		t.Errorf("asm.Externals: expected 2, got %d", len(clibMod.Asm.Externals))
	}
}
