package ast

type RepeatStmt struct {
	StmtSeq  []Statement
	BoolExpr Expression
}

func (r *RepeatStmt) stmt() {}
func (r *RepeatStmt) String() string {
	return ""
}
