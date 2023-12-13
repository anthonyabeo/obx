package ast

type LoopStmt struct {
	StmtSeq []Statement
}

func (l *LoopStmt) stmt()          {}
func (l *LoopStmt) String() string { return "" }
