package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type LabelRange struct {
	Begin Expression
	End   Expression
}

// Case
// ----------------------------------------------------------
type Case struct {
	CaseLabelList []*LabelRange
	StmtSeq       []Statement
}

// CaseStmt
// ----------------------------------------------------------
type CaseStmt struct {
	Case  *token.Position
	Expr  Expression
	Cases []*Case
	Else  []Statement
}

func (stmt *CaseStmt) Pos() *token.Position {
	return stmt.Case
}

func (stmt *CaseStmt) End() *token.Position {
	panic("not implemented")
}

func (stmt *CaseStmt) Accept(vst Visitor) { vst.VisitCaseStmt(stmt) }

func (stmt *CaseStmt) stmt() {}

func (stmt *CaseStmt) String() string {
	panic("not implemented")
}
