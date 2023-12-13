package ast

type WhileStmt struct {
	BoolExpr Expression
	StmtSeq  []Statement
	ElsIfs   []*ElsIfBranch
}

func (w *WhileStmt) stmt() {}
func (w *WhileStmt) String() string {
	return ""
}
