package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type RepeatStmt struct {
	Repeat   *token.Position
	StmtSeq  []Statement
	BoolExpr Expression
}

func (r *RepeatStmt) Pos() *token.Position {
	return r.Repeat
}

func (r *RepeatStmt) End() *token.Position {
	panic("not implemented")
}

func (r *RepeatStmt) stmt() {}
func (r *RepeatStmt) String() string {
	return ""
}
