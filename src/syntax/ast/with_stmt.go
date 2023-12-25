package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type Guard struct {
	Expr    Expression
	Type    Expression
	StmtSeq []Statement
}

type WithStmt struct {
	With *token.Position
	Arms []*Guard
	Else []Statement
}

func (w *WithStmt) stmt()                {}
func (w *WithStmt) Pos() *token.Position { return w.With }
func (w *WithStmt) End() *token.Position { panic("not implemented") }
func (w *WithStmt) String() string       { panic("not implemented") }
func (w *WithStmt) Accept(vst Visitor)   { vst.VisitWithStmt(w) }
