package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type ElsIfBranch struct {
	BoolExpr Expression
	ThenPath []Statement
}

type IfStmt struct {
	If             *token.Position
	BoolExpr       Expression
	ThenPath       []Statement
	ElseIfBranches []*ElsIfBranch
	ElsePath       []Statement
}

func (stmt *IfStmt) Pos() *token.Position {
	return stmt.If
}

func (stmt *IfStmt) End() *token.Position {
	panic("not implemented")
}

func (stmt *IfStmt) stmt() {}

func (stmt *IfStmt) String() string {
	return ""
}
