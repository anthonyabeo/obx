package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type ForStmt struct {
	For      *token.Position
	CtlVar   *Ident
	InitVal  Expression
	FinalVal Expression
	By       Expression
	StmtSeq  []Statement
}

func (stmt *ForStmt) stmt()                {}
func (stmt *ForStmt) Pos() *token.Position { return stmt.For }
func (stmt *ForStmt) End() *token.Position { panic("not implemented") }
func (stmt *ForStmt) String() string       { panic("not implemented") }
func (stmt *ForStmt) Accept(vst Visitor)   { vst.VisitForStmt(stmt) }
