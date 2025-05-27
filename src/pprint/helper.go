package pprint

import (
	"encoding/json"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

func formatPosition(pos *report.Position) any {
	if pos == nil {
		return nil
	}
	return map[string]any{
		"line":   pos.Line,
		"column": pos.Column,
		"file":   pos.File,
	}
}

func formatRange(rng *report.Range) any {
	if rng == nil {
		return nil
	}
	return map[string]any{
		"start": formatPosition(rng.Start),
		"end":   formatPosition(rng.End),
	}
}

func visitList[T ast.Node](nodes []T, v ast.Visitor) []any {
	result := make([]any, len(nodes))
	for i, node := range nodes {
		result[i] = node.Accept(v)
	}
	return result
}

func PrettyPrintJSON(obx *ast.Oberon, ctx *report.Context) ([]byte, error) {
	v := NewAstPrinter(obx, ctx)
	return json.MarshalIndent(v.Print(), "", "  ")
}
