package sema

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/support/source"
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
	sm := source.NewSourceManager()
	ctx := &diag.Context{
		FileName:  "Test.obx",
		FilePath:  "Test.obx",
		Content:   []byte(testSource),
		Source:    sm,
		Env:       ast.NewEnv(),
		Reporter:  diag.NewBufferedReporter(sm, 25, diag.Stdout(formatter.NewTextFormatter(sm, 0))),
	}

	// Parse the file
	p := parser.NewParser(ctx)
	unit := p.Parse()

	ctx.Env.SetCurrentScope(ctx.Env.ModuleScope(unit.Name()))
	resolve := NewNameResolver(ctx)
	resolve.Resolve(unit)

	// Check for diagnostics (should be none)
	if diags := ctx.Reporter.Diagnostics(); len(diags) > 0 {
		t.Error("errors encountered")
		ctx.Reporter.Flush()
	}

	// Collect symbols
	module := unit.(*ast.Module)
	env := ctx.Env.ModuleScope(module.BName)
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

	assign := findAssignmentTo("x", module)
	d, ok := assign.LValue.(*ast.Designator)
	if !ok || d.QIdent.Symbol.MangledName() != "Test$x" {
		t.Errorf("assignment to x: expected mangled name 'Test$x', got %q", d.QIdent.Name)
	}

	d, ok = assign.RValue.(*ast.Designator)
	if !ok || d.QIdent.Symbol.MangledName() != "Test$DoSomething$y" {
		t.Errorf("assignment to x: expected mangled name of RHS to be 'Test$DoSomething$y', got %q", d.QIdent.Name)
	}

}

func TestResolveQualifiedIdentifier(t *testing.T) {
	obxSrc := `
    MODULE Math;
        VAR Pi*: REAL;
    END Math.

    MODULE Main;
        IMPORT Math;
        VAR x: REAL;
        BEGIN
            x := Math.Pi
        END Main.
    `

	tmp := t.TempDir()
	writeModuleFiles(t, tmp, obxSrc)

	headers, err := project.DiscoverAndScan(tmp)
	if err != nil {
		t.Fatalf("DiscoverAndScan: %v", err)
	}

	graph, err := project.BuildImportGraph(headers)
	if err != nil {
		t.Fatalf("BuildImportGraph: %v", err)
	}

	sorted, err := project.TopoSort(graph)
	if err != nil {
		t.Fatalf("TopoSort: %v", err)
	}

	obx := ast.NewOberonX()
	srcMgr := source.NewSourceManager()
	reporter := diag.NewBufferedReporter(srcMgr, 32, diag.Stdout(formatter.NewTextFormatter(srcMgr, 0)))

	ctx := &diag.Context{
		Source:    srcMgr,
		Reporter:  reporter,
		Env:       ast.NewEnv(),
	}

	for _, header := range sorted {
		data, err := os.ReadFile(header.File)
		if err != nil {
			t.Fatalf("read %s: %v", header.File, err)
		}

		ctx.FileName = filepath.Base(header.File)
		ctx.FilePath = header.File
		ctx.Content = data[header.StartPos:header.EndPos]

		p := parser.NewParser(ctx)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatalf("%d errors found", ctx.Reporter.ErrorCount())
		}

		obx.AddUnit(unit)
	}

	resolve := NewNameResolver(ctx)
	for _, unit := range obx.Units {
		ctx.Env.SetCurrentScope(ctx.Env.ModuleScope(unit.Name()))
		resolve.Resolve(unit)
	}

	// Look into the AST for `Math.Pi` reference
	main, _ := obx.Lookup("Main")
	assign := findAssignmentTo("x", main) // helper to drill down AST

	dsg, ok := assign.RValue.(*ast.Designator)
	if !ok {
		t.Fatalf("expected Designator, got %T", assign.RValue)
	}

	if dsg.QIdent.Symbol == nil {
		t.Errorf("expected resolved symbol for Math.Pi")
	}

	if dsg.QIdent.Symbol.MangledName() != "Math$Pi" {
		t.Errorf("expected mangled name 'Math$Pi', got '%s'", dsg.QIdent.Symbol.MangledName())
	}

}

func TestNameResolutionUndefined(t *testing.T) {
	src := `
		MODULE Main;
		BEGIN
			y := 10  (* y is undefined *)
		END Main.
	`

	sm := source.NewSourceManager()
	ctx := &diag.Context{
		FileName:  "Test.obx",
		FilePath:  "Test.obx",
		Content:   []byte(src),
		Source:    sm,
		Env:       ast.NewEnv(),
		Reporter:  diag.NewBufferedReporter(sm, 25, diag.Stdout(formatter.NewTextFormatter(sm, 0))),
	}

	// Parse the file
	p := parser.NewParser(ctx)
	unit := p.Parse()

	resolve := NewNameResolver(ctx)
	resolve.Resolve(unit)

	if ctx.Reporter.ErrorCount() == 0 {
		t.Fatal("expected errors, got none")
	}

	// Check for diagnostics (should be none)
	if diags := ctx.Reporter.Diagnostics(); len(diags) > 0 {
		if !strings.Contains(diags[0].Message, "undeclared identifier: 'y'") {
			t.Errorf("expected \"undeclared identifier 'y', got %q\"", diags[0].Message)
		}
	}
}

func TestNameResolution_Basic(t *testing.T) {
	src := `
		MODULE Main;
		VAR x: INTEGER;

		PROCEDURE Foo;
		VAR y: INTEGER;
		BEGIN
			x := 1;  (* should resolve to Main.x *)
			y := 2  (* should resolve to Foo.y *)
		END Foo;

		END Main.
	`

	sm := source.NewSourceManager()
	ctx := &diag.Context{
		FileName:  "Test.obx",
		FilePath:  "Test.obx",
		Content:   []byte(src),
		Source:    sm,
		Env:       ast.NewEnv(),
		Reporter:  diag.NewBufferedReporter(sm, 25, diag.Stdout(formatter.NewTextFormatter(sm, 0))),
	}

	// Parse the file
	p := parser.NewParser(ctx)
	unit := p.Parse()

	assign := findAssignmentTo("y", unit)

	resolve := NewNameResolver(ctx)
	resolve.Resolve(unit)

	dsg, ok := assign.LValue.(*ast.Designator)
	if !ok {
		t.Errorf("expected designator, got %T", assign.LValue)
	}

	if dsg.QIdent.Symbol == nil || dsg.QIdent.Symbol.MangledName() != "Main$Foo$y" {
		t.Errorf("expected y to resolve to local Main$Foo$y, got %v", dsg.QIdent.Name)
	}
}

