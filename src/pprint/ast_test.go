package pprint

import (
	"encoding/json"
	"os"
	"testing"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

func parseSource(t *testing.T, input []byte, ctx *report.Context) *ast.Oberon {
	p := parser.NewParser(ctx)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("Parser errors")
	}

	obx := ast.NewOberon()
	obx.AddUnit(unit.Name(), unit)

	return obx
}

func TestPrettyPrintJSON(t *testing.T) {
	input := []byte(`
		MODULE Test;
		VAR x: INTEGER;
		BEGIN x := 42
		END Test.
	`)
	table := ast.NewEnvironment(ast.GlobalEnviron, "Main")

	filename := "test.obx"
	mgr := report.NewSourceManager()
	ctx := &report.Context{
		FileName: filename,
		Content:  input,
		Env:      table,
		Source:   mgr,
		Reporter: report.NewBufferedReporter(mgr, 25, report.StdoutSink{
			Source: mgr,
			Writer: os.Stdout,
		}),
		TabWidth: 4,
	}

	obx := parseSource(t, input, ctx)
	data, err := PrettyPrintJSON(obx, ctx)
	if err != nil {
		t.Fatalf("PrettyPrintJSON failed: %v", err)
	}

	// Write the JSON to a file for debugging (optional)
	_ = os.WriteFile("test_output.json", data, 0644)

	var result map[string]any
	if err := json.Unmarshal(data, &result); err != nil {
		t.Fatalf("Invalid JSON: %v", err)
	}

	// Validate top-level type
	if result["type"] != "Oberon" {
		t.Errorf("expected top-level type 'Oberon', got %v", result["type"])
	}

	// Check "units" array
	units, ok := result["units"].([]any)
	if !ok || len(units) == 0 {
		t.Fatalf("expected non-empty 'units' array, got: %v", result["units"])
	}

	// Check first unit type
	firstUnit, ok := units[0].(map[string]any)
	if !ok {
		t.Fatalf("expected object in 'units[0]', got: %T", units[0])
	}
	if firstUnit["type"] != "Module" {
		t.Errorf("expected first unit type 'Module', got %v", firstUnit["type"])
	}

	// Optional: check expected fields
	if _, ok := firstUnit["declarations"]; !ok {
		t.Errorf("expected 'declarations' field in module")
	}
	if _, ok := firstUnit["statements"]; !ok {
		t.Errorf("expected 'statements' field in module")
	}
}

func TestPrettyPrintJSON_LargerProgram(t *testing.T) {
	input := []byte(`
		MODULE Bigger;
		IMPORT Out;

		CONST Pi = 3.14;

		TYPE
			Point = RECORD
				x, y: INTEGER
			END;

		VAR
			p: Point;
			count: INTEGER;

		PROCEDURE Init*;
		BEGIN
			p.x := 0; p.y := 0;
			count := 0
		END Init;

		PROCEDURE Main*;
		BEGIN
			Init();
			Out.Int(count, 0)
		END Main;

		BEGIN
			Main()
		END Bigger.
	`)

	table := ast.NewEnvironment(ast.GlobalEnviron, "Main")
	out := ast.NewEnvironment(table, "Out")
	out.Insert(ast.NewProcedureSymbol("Out.Int", ast.Exported))

	filename := "test.obx"
	mgr := report.NewSourceManager()
	ctx := &report.Context{
		FileName: filename,
		Content:  input,
		Env:      table,
		Source:   mgr,
		Reporter: report.NewBufferedReporter(mgr, 25, report.StdoutSink{
			Source: mgr,
			Writer: os.Stdout,
		}),
		TabWidth: 4,
	}

	obx := parseSource(t, input, ctx)
	data, err := PrettyPrintJSON(obx, ctx)
	if err != nil {
		t.Fatalf("PrettyPrintJSON failed: %v", err)
	}

	// Write the output to a file for debugging (optional)
	_ = os.WriteFile("test_output.json", data, 0644)

	var result map[string]any
	if err := json.Unmarshal(data, &result); err != nil {
		t.Fatalf("Invalid JSON: %v", err)
	}

	// Ensure top-level type is Oberon
	if result["type"] != "Oberon" {
		t.Errorf("expected top-level type 'Oberon', got %v", result["type"])
	}

	units, ok := result["units"].([]any)
	if !ok || len(units) == 0 {
		t.Fatalf("expected non-empty 'units' array, got: %v", result["units"])
	}

	module, ok := units[0].(map[string]any)
	if !ok || module["type"] != "Module" {
		t.Fatalf("expected 'Module' in units[0], got %v", module["type"])
	}

	proc := FindProcedureByName(result, "Main")
	if proc == nil {
		t.Errorf("expected to find procedure with name 'Main'")
	}

	decl := FindVariableByName(result, "p")
	if decl == nil {
		t.Errorf("Expected variable declaration for 'p'")
	}

	ty := FindTypeByName(result, "Point")
	if ty == nil {
		t.Errorf("Expected variable declaration for 'p'")
	}
}
