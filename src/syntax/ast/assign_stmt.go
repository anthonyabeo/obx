package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type AssignStmt struct {
	AssignPos *token.Position
	LValue    Expression
	RValue    Expression
}

func (a *AssignStmt) Pos() *token.Position {
	return a.AssignPos
}

func (a *AssignStmt) End() *token.Position {
	return a.RValue.End()
}

func (a *AssignStmt) stmt() {}

func (a *AssignStmt) String() string {
	return fmt.Sprintf("%v := %v", a.LValue, a.RValue)
}
