package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type WhileStmt struct {
	While    *token.Position
	BoolExpr Expression
	StmtSeq  []Statement
	ElsIfs   []*ElsIfBranch
}

func (w *WhileStmt) Pos() *token.Position {
	return w.While
}

func (w *WhileStmt) End() *token.Position {
	panic("not implemented")
}

func (w *WhileStmt) Accept(vst Visitor) {
	vst.VisitWhileStmt(w)
}

func (w *WhileStmt) stmt() {}
func (w *WhileStmt) String() string {
	return ""
}
