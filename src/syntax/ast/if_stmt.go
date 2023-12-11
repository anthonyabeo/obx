package ast

type IfStmt struct {
	BoolExpr Expression
	ThenPath Statement
	ElsePath Statement
}

func (stmt *IfStmt) stmt() {}

func (stmt *IfStmt) String() string {
	return ""
}
