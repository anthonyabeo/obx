package minir

import (
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/sema/types"
)

func makeIdentity() *desugar.Function {
	i32 := types.Int32Type
	return &desugar.Function{
		Name:   "identity",
		Params: []*desugar.Param{{Name: "x", Kind: desugar.ValueParam, Typ: i32}},
		Result: i32,
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.ReturnStmt{Result: &desugar.Param{Name: "x", Kind: desugar.ValueParam, Typ: i32}},
		}},
	}
}

func TestDOT_Identity(t *testing.T) {
	fns := Lower(&desugar.Program{Modules: []*desugar.Module{{Decls: []desugar.Decl{makeIdentity()}}}})
	if len(fns) != 1 { t.Fatalf("expected 1 fn") }
	dot := FormatDOT(fns[0])
	if !strings.Contains(dot, "digraph") {
		t.Fatalf("dot output missing digraph: %s", dot)
	}
	if !strings.Contains(dot, "entry") {
		t.Fatalf("dot output missing entry label: %s", dot)
	}
}

func TestDOT_CountedLoop(t *testing.T) {
	// reuse lower_test's sum10 builder
	i32 := types.Int32Type
	nVar := &desugar.Variable{Name: "n", Type: i32}
	sVar := &desugar.Variable{Name: "s", Type: i32}
	nRef := &desugar.VariableRef{Name: "n", Mangled: "n", SemaType: i32}
	sRef := &desugar.VariableRef{Name: "s", Mangled: "s", SemaType: i32}
	lit0 := &desugar.Literal{Kind: 0, Value: "0", SemaType: i32}
	lit1 := &desugar.Literal{Kind: 0, Value: "1", SemaType: i32}
	lit9 := &desugar.Literal{Kind: 0, Value: "9", SemaType: i32}
	fn := &desugar.Function{
		Name:   "sum10",
		Result: i32,
		Locals: []desugar.Decl{nVar, sVar},
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.AssignStmt{Left: nRef, Right: lit0},
			&desugar.AssignStmt{Left: sRef, Right: lit0},
			&desugar.LoopStmt{Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
				&desugar.IfStmt{Cond: &desugar.BinaryExpr{Op: 0, Left: nRef, Right: lit9, SemaType: types.BooleanType}, Then: &desugar.CompoundStmt{Stmts: []desugar.Stmt{&desugar.ExitStmt{}}}},
				&desugar.AssignStmt{Left: sRef, Right: &desugar.BinaryExpr{Op: 0, Left: sRef, Right: nRef, SemaType: i32}},
				&desugar.AssignStmt{Left: nRef, Right: &desugar.BinaryExpr{Op: 0, Left: nRef, Right: lit1, SemaType: i32}},
			}}},
			&desugar.ReturnStmt{Result: sRef},
		}},
	}
	fns := Lower(&desugar.Program{Modules: []*desugar.Module{{Decls: []desugar.Decl{fn}}}})
	if len(fns) != 1 { t.Fatalf("expected 1 fn") }
	dot := FormatDOT(fns[0])
	// Expect at least one back-edge with constraint=false attribute
	if !strings.Contains(dot, "constraint=false") {
		t.Fatalf("expected back-edge constraint=false in DOT output: %s", dot)
	}
}

func TestDOT_Case(t *testing.T) {
	i32 := types.Int32Type
	x := &desugar.Param{Name: "x", Kind: desugar.ValueParam, Typ: i32}
	fn := &desugar.Function{
		Name:   "classify",
		Params: []*desugar.Param{x},
		Result: i32,
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.CaseStmt{
				Expr: x,
				Cases: []*desugar.Case{
					{Labels: []*desugar.LabelRange{{Low: &desugar.Literal{Value: "1", SemaType: i32}, High: &desugar.Literal{Value: "1", SemaType: i32}}}, Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{&desugar.ReturnStmt{Result: &desugar.Literal{Value: "10", SemaType: i32}}}}},
					{Labels: []*desugar.LabelRange{{Low: &desugar.Literal{Value: "2", SemaType: i32}, High: &desugar.Literal{Value: "2", SemaType: i32}}}, Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{&desugar.ReturnStmt{Result: &desugar.Literal{Value: "20", SemaType: i32}}}}},
				},
				Else: &desugar.CompoundStmt{Stmts: []desugar.Stmt{&desugar.ReturnStmt{Result: &desugar.Literal{Value: "0", SemaType: i32}}}},
			},
		}},
	}
	fns := Lower(&desugar.Program{Modules: []*desugar.Module{{Decls: []desugar.Decl{fn}}}})
	if len(fns) != 1 { t.Fatalf("expected 1 fn") }
	dot := FormatDOT(fns[0])
	if !strings.Contains(dot, "case_body_") {
		t.Fatalf("expected case_body nodes in DOT output: %s", dot)
	}
}

