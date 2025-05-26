package pprint

import (
	"encoding/json"
	"os"
	"testing"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/syntax/scan"
)

func parseSource(t *testing.T, input []byte) *ast.Oberon {
	filename := "test.obx"
	mgr := report.NewSourceManager()
	mgr.Load(filename, input)
	r := report.NewBufferedReporter(mgr, 25, report.StdoutSink{
		Source: mgr,
		Writer: os.Stdout,
	})

	table := ast.NewEnvironment(ast.GlobalEnviron, "Main")

	lex := scan.Scan(mgr.GetSourceFile(filename), mgr)
	p := parser.NewParser(lex, r, table, nil)
	unit := p.Parse()

	if r.ErrorCount() > 0 {
		r.Flush()
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
	obx := parseSource(t, input)
	data, err := PrettyPrintJSON(obx)
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
