package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type LoopStmt struct {
	Loop    *token.Position
	StmtSeq []Statement
}

func (l *LoopStmt) Pos() *token.Position {
	return l.Loop
}

func (l *LoopStmt) End() *token.Position {
	panic("not implemented")
}

func (l *LoopStmt) stmt()          {}
func (l *LoopStmt) String() string { return "" }
