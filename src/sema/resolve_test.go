package sema

import (
	"os"
	"testing"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

const testSource = `
MODULE Test;

VAR
  x: INTEGER

PROCEDURE DoSomething;
VAR
  y: INTEGER;
BEGIN
  x := y
END DoSomething

END Test.
`

func TestNameResolution_BasicProcedure(t *testing.T) {
	env := ast.NewEnvironment(ast.GlobalEnviron, "")
	sm := report.NewSourceManager()
	ctx := &report.Context{
		FileName: "Test.obx",
		FilePath: "Test.obx",
		Content:  []byte(testSource),
		Source:   sm,
		Env:      env,
		Reporter: report.NewBufferedReporter(sm, 25, report.StdoutSink{
			Source: sm,
			Writer: os.Stdout,
		}),
		TabWidth: 4,
	}

	// Parse the file
	p := parser.NewParser(ctx)
	unit := p.Parse()

	// Set up OberonX and semantic analysis
	obx := &ast.OberonX{Units: map[string]ast.CompilationUnit{"Test": unit}}
	sema := NewSema(ctx, obx)
	sema.Validate()

	// Check for diagnostics (should be none)
	if diags := ctx.Reporter.Diagnostics(); len(diags) > 0 {
		t.Error("errors encountered")
		ctx.Reporter.Flush()
	}

	// Collect symbols
	env = unit.(*ast.Module).Env
	if env == nil {
		t.Fatal("module environment is nil")
	}

	checkSymbol := func(name, expectedMangled string) {
		sym := env.Lookup(name)
		if sym == nil {
			t.Errorf("symbol %q not resolved", name)
			return
		}
		actual := sym.MangledName()
		if actual != expectedMangled {
			t.Errorf("symbol %q: expected mangled name %q, got %q", name, expectedMangled, actual)
		}
	}

	checkSymbol("x", "Test$x")
	checkSymbol("DoSomething", "Test$DoSomething")

	proc := env.Lookup("DoSomething").(*ast.ProcedureSymbol)
	procEnv := proc.Env
	if procEnv == nil {
		t.Fatal("procedure environment for DoSomething is nil")
	}

	symY := procEnv.Lookup("y")
	if symY == nil || symY.MangledName() != "Test$DoSomething$y" {
		t.Errorf("symbol y: expected mangled name %q, got %v", "Test$DoSomething$y", symY)
	}
}
