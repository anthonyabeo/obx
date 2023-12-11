package ast

import "fmt"

type AssignStmt struct {
	LValue Expression
	RValue Expression
}

func (a *AssignStmt) stmt() {}

func (a *AssignStmt) String() string {
	return fmt.Sprintf("%v := %v", a.LValue, a.RValue)
}
