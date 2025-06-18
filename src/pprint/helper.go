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

func visitSelector(selectors []ast.Selector, v ast.Visitor) []any {
	result := make([]any, len(selectors))

	vst := v.(*AstPrinter)
	for i, sel := range selectors {
		switch s := sel.(type) {
		case *ast.DotOp:
			result[i] = map[string]any{
				"type":  "DotOp",
				"name":  sel.(*ast.DotOp).Field,
				"range": formatRange(vst.ctx.Source.Span(vst.ctx.FileName, s.StartOffset, s.EndOffset)),
			}
		case *ast.IndexOp:
			result[i] = map[string]any{
				"type":    "IndexOp",
				"indices": visitList(s.List, v),
				"range":   formatRange(vst.ctx.Source.Span(vst.ctx.FileName, s.StartOffset, s.EndOffset)),
			}
		case *ast.PtrDeref:
			result[i] = map[string]any{
				"type":  "PtrDeref",
				"range": formatRange(vst.ctx.Source.Span(vst.ctx.FileName, s.StartOffset, s.EndOffset)),
			}
		case *ast.TypeGuard:
			result[i] = map[string]any{
				"type":  "TypeGuard",
				"Type":  s.Ty.Accept(v),
				"range": formatRange(vst.ctx.Source.Span(vst.ctx.FileName, s.StartOffset, s.EndOffset)),
			}
		}
	}
	return result
}

func PrettyPrintJSON(obx *ast.OberonX, ctx *report.Context) ([]byte, error) {
	v := NewAstPrinter(obx, ctx)
	return json.MarshalIndent(v.Print(), "", "  ")
}
