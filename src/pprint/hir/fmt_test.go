package hir

import (
	"encoding/json"
	"fmt"
	"os"
	"testing"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/cmd/cli"
	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

func parseSourceAndLowerToHIR(t *testing.T, ctx *report.Context) *hir.Program {
	p := parser.NewParser(ctx)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("Parser errors")
	}

	obx := ast.NewOberonX()
	obx.AddUnit(unit)

	s := sema.NewSema(ctx, obx)
	s.Validate()

	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("semantics errors")
	}

	gen := hir.NewGenerator(ctx, obx)

	return gen.Generate()
}

func TestFormatSampleProgram(t *testing.T) {
	input := []byte(`
		MODULE Test;
		VAR x: INTEGER;
		BEGIN x := 42
		END Test.
	`)

	filename := "test.obx"
	mgr := report.NewSourceManager()
	ctx := &report.Context{
		FileName: filename,
		Content:  input,
		Env:      ast.NewEnv(),
		Source:   mgr,
		Reporter: report.NewBufferedReporter(mgr, 25, report.StdoutSink{
			Source: mgr,
			Writer: os.Stdout,
		}),
		TabWidth:  4,
		Names:     adt.NewStack[string](),
		ExprLists: adt.NewStack[[]ast.Expression](),
	}

	program := parseSourceAndLowerToHIR(t, ctx)

	formatter := NewFormatter(program)
	result := formatter.FormatProgram()

	// Marshal to JSON to check structure
	data, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		t.Fatalf("failed to marshal formatted HIR: %v", err)
	}

	Root, err := cli.FindProjectRoot()
	if err != nil {
		t.Errorf("FindProjectRoot failed: %v", err)
	}

	// Write the JSON to a file for debugging (optional)
	outFile := fmt.Sprintf("%s/out/test_output.json", Root)
	_ = os.WriteFile(outFile, data, 0644)

	var out map[string]any
	if err := json.Unmarshal(data, &out); err != nil {
		t.Fatalf("invalid JSON: %v", err)
	}

	if out["type"] != "HIRProgram" {
		t.Errorf("expected type 'HIRProgram', got %v", out["type"])
	}

	modules, ok := out["modules"].([]any)
	if !ok || len(modules) == 0 {
		t.Fatalf("expected non-empty modules array")
	}

	firstModule, ok := modules[0].(map[string]any)
	if !ok || firstModule["type"] != "Module" {
		t.Fatalf("expected first module to be type 'Module'")
	}

	if firstModule["name"] != "Test" {
		t.Errorf("expected module name 'TestModule', got %v", firstModule["name"])
	}
}

func TestFormatProgramWithProcedureAndCall(t *testing.T) {
	input := []byte(`
		MODULE ProcTest;
		VAR x: INTEGER;

		PROCEDURE Inc;
		BEGIN
			x := x + 1
		END Inc;

		BEGIN
			Inc()
		END ProcTest.
	`)

	filename := "proc_test.obx"
	mgr := report.NewSourceManager()
	ctx := &report.Context{
		FileName: filename,
		Content:  input,
		Env:      ast.NewEnv(),
		Source:   mgr,
		Reporter: report.NewBufferedReporter(mgr, 25, report.StdoutSink{
			Source: mgr,
			Writer: os.Stdout,
		}),
		TabWidth:  4,
		Names:     adt.NewStack[string](),
		ExprLists: adt.NewStack[[]ast.Expression](),
	}

	program := parseSourceAndLowerToHIR(t, ctx)
	formatter := NewFormatter(program)
	result := formatter.FormatProgram()

	data, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		t.Fatalf("failed to marshal formatted HIR: %v", err)
	}

	Root, err := cli.FindProjectRoot()
	if err != nil {
		t.Errorf("FindProjectRoot failed: %v", err)
	}

	// Write the JSON to a file for debugging (optional)
	outputFile := fmt.Sprintf("%s/out/%s.json", Root, filename)
	_ = os.WriteFile(outputFile, data, 0644)

	var out map[string]any
	if err := json.Unmarshal(data, &out); err != nil {
		t.Fatalf("invalid JSON: %v", err)
	}

	if out["type"] != "HIRProgram" {
		t.Errorf("expected type 'HIRProgram', got %v", out["type"])
	}

	modules, ok := out["modules"].([]any)
	if !ok || len(modules) == 0 {
		t.Fatalf("expected non-empty modules array")
	}

	firstModule, ok := modules[0].(map[string]any)
	if !ok || firstModule["type"] != "Module" {
		t.Fatalf("expected first module to be type 'Module'")
	}

	decls, ok := firstModule["declarations"].(map[string]any)
	if !ok {
		t.Fatalf("expected declarations in module")
	}
	declList, ok := decls["decls"].([]any)
	if !ok {
		t.Fatalf("expected decls to be a list")
	}

	foundProc := false
	for _, d := range declList {
		decl, ok := d.(map[string]any)
		if ok && decl["type"] == "Function" && decl["name"] == "Inc" {
			foundProc = true
			break
		}
	}
	if !foundProc {
		t.Errorf("expected procedure 'Inc' in declarations")
	}

	statements, ok := firstModule["statements"].(map[string]any)
	if !ok {
		t.Fatalf("expected statements in module")
	}
	body, ok := statements["body"].(map[string]any)
	if !ok {
		t.Fatalf("expected body in main function")
	}
	stmts, ok := body["statements"].([]any)
	if !ok || len(stmts) == 0 {
		t.Fatalf("expected at least one statement in main body")
	}
	foundCall := false
	for _, s := range stmts {
		stmt, ok := s.(map[string]any)
		if ok && stmt["type"] == "FunctionCall" {
			foundCall = true
			break
		}
	}
	if !foundCall {
		t.Errorf("expected a procedure call to 'Inc' in main body")
	}
}

func TestFormatProgramWithMultipleStatementsAndLoop(t *testing.T) {
	input := []byte(`
		MODULE LoopTest;
		VAR x, y: INTEGER;

		BEGIN
			x := 0;
			y := 10;
			WHILE x < y DO
				x := x + 1
			END
		END LoopTest.
	`)

	filename := "loop_test.obx"
	mgr := report.NewSourceManager()
	ctx := &report.Context{
		FileName: filename,
		Content:  input,
		Env:      ast.NewEnv(),
		Source:   mgr,
		Reporter: report.NewBufferedReporter(mgr, 25, report.StdoutSink{
			Source: mgr,
			Writer: os.Stdout,
		}),
		TabWidth:  4,
		Names:     adt.NewStack[string](),
		ExprLists: adt.NewStack[[]ast.Expression](),
	}

	program := parseSourceAndLowerToHIR(t, ctx)
	formatter := NewFormatter(program)
	result := formatter.FormatProgram()

	data, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		t.Fatalf("failed to marshal formatted HIR: %v", err)
	}

	Root, err := cli.FindProjectRoot()
	if err != nil {
		t.Errorf("FindProjectRoot failed: %v", err)
	}

	// Write the JSON to a file for debugging (optional)
	outputFile := fmt.Sprintf("%s/out/%s.json", Root, filename)
	_ = os.WriteFile(outputFile, data, 0644)

	var out map[string]any
	if err := json.Unmarshal(data, &out); err != nil {
		t.Fatalf("invalid JSON: %v", err)
	}

	modules, ok := out["modules"].([]any)
	if !ok || len(modules) == 0 {
		t.Fatalf("expected non-empty modules array")
	}

	firstModule, ok := modules[0].(map[string]any)
	if !ok || firstModule["type"] != "Module" {
		t.Fatalf("expected first module to be type 'Module'")
	}

	statements, ok := firstModule["statements"].(map[string]any)
	if !ok {
		t.Fatalf("expected statements in module")
	}
	body, ok := statements["body"].(map[string]any)
	if !ok {
		t.Fatalf("expected body in main function")
	}
	stmts, ok := body["statements"].([]any)
	if !ok || len(stmts) < 3 {
		t.Fatalf("expected at least three statements in main body")
	}

	// Check for assignments and loop
	foundAssign1, foundAssign2, foundLoop := false, false, false
	for _, s := range stmts {
		stmt, ok := s.(map[string]any)
		if !ok {
			continue
		}
		switch stmt["type"] {
		case "Assignment":
			target, _ := stmt["target"].(map[string]any)
			if target != nil && target["name"] == "x" {
				foundAssign1 = true
			}
			if target != nil && target["name"] == "y" {
				foundAssign2 = true
			}
		case "LoopStmt":
			foundLoop = true
		}
	}
	if !foundAssign1 {
		t.Errorf("expected assignment to 'x' in main body")
	}
	if !foundAssign2 {
		t.Errorf("expected assignment to 'y' in main body")
	}
	if !foundLoop {
		t.Errorf("expected a loop statement in main body")
	}
}
