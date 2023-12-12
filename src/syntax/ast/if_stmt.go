package ast

type ElsIfBranch struct {
	BoolExpr Expression
	ThenPath []Statement
}

type IfStmt struct {
	BoolExpr       Expression
	ThenPath       []Statement
	ElseIfBranches []*ElsIfBranch
	ElsePath       []Statement
}

func (stmt *IfStmt) stmt() {}

func (stmt *IfStmt) String() string {
	return ""
}
