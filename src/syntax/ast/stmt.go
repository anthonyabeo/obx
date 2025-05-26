package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/report"
)

type (
	ForStmt struct {
		CtlVar   string
		InitVal  Expression
		FinalVal Expression
		By       Expression
		StmtSeq  []Statement

		Label string

		Pos *report.Position
		Rng *report.Range
	}

	ExitStmt struct {
		Label string
		Pos   *report.Position
		Rng   *report.Range
	}

	Guard struct {
		Expr    Expression
		Type    Expression
		StmtSeq []Statement
		Pos     *report.Position
		Rng     *report.Range
	}

	WithStmt struct {
		Arms []*Guard
		Else []Statement
		Pos  *report.Position
		Rng  *report.Range
	}

	ProcedureCall struct {
		Callee       *Designator
		ActualParams []Expression
		Pos          *report.Position
		Rng          *report.Range
	}

	WhileStmt struct {
		BoolExpr Expression
		StmtSeq  []Statement
		ElsIfs   []*ElseIfBranch
		Label    string
		Pos      *report.Position
		Rng      *report.Range
	}

	ReturnStmt struct {
		Value Expression
		Pos   *report.Position
		Rng   *report.Range
	}

	RepeatStmt struct {
		StmtSeq  []Statement
		BoolExpr Expression
		Label    string
		Pos      *report.Position
		Rng      *report.Range
	}

	LoopStmt struct {
		StmtSeq []Statement

		Label string

		Pos *report.Position
		Rng *report.Range
	}

	ElseIfBranch struct {
		BoolExpr Expression
		ThenPath []Statement
		Pos      *report.Position
		Rng      *report.Range
	}

	IfStmt struct {
		BoolExpr       Expression
		ThenPath       []Statement
		ElseIfBranches []*ElseIfBranch
		ElsePath       []Statement
		Pos            *report.Position
		Rng            *report.Range
	}

	AssignmentStmt struct {
		LValue Expression
		RValue Expression

		Pos *report.Position
		Rng *report.Range
	}

	CaseStmt struct {
		Expr  Expression
		Cases []*Case
		Else  []Statement

		Pos *report.Position
		Rng *report.Range
	}

	Case struct {
		CaseLabelList []*LabelRange
		StmtSeq       []Statement

		Pos *report.Position
		Rng *report.Range
	}

	LabelRange struct {
		High Expression
		Low  Expression
		Pos  *report.Position
		Rng  *report.Range
	}

	BadStmt struct {
		Pos *report.Position
		Rng *report.Range
	}
)

func (stmt *ForStmt) stmt()                      {}
func (stmt *ForStmt) String() string             { panic("not implemented") }
func (stmt *ForStmt) Accept(vst Visitor) any     { return vst.VisitForStmt(stmt) }
func (stmt *ForStmt) Position() *report.Position { return stmt.Pos }
func (stmt *ForStmt) Range() *report.Range       { return stmt.Rng }

func (stmt *ExitStmt) stmt()                      {}
func (stmt *ExitStmt) String() string             { return "exit" }
func (stmt *ExitStmt) Accept(vst Visitor) any     { return vst.VisitExitStmt(stmt) }
func (stmt *ExitStmt) Position() *report.Position { return stmt.Pos }
func (stmt *ExitStmt) Range() *report.Range       { return stmt.Rng }

func (w *WithStmt) stmt()                      {}
func (w *WithStmt) String() string             { panic("not implemented") }
func (w *WithStmt) Accept(vst Visitor) any     { return vst.VisitWithStmt(w) }
func (w *WithStmt) Position() *report.Position { return w.Pos }
func (w *WithStmt) Range() *report.Range       { return w.Rng }

func (g *Guard) Accept(vst Visitor) any { return vst.VisitGuard(g) }
func (g *Guard) String() string {
	var stmts []string
	for _, stmt := range g.StmtSeq {
		stmts = append(stmts, stmt.String())
	}

	return fmt.Sprintf("guard %s: %s", g.Expr, strings.Join(stmts, "; "))
}
func (g *Guard) Position() *report.Position { return g.Pos }
func (g *Guard) Range() *report.Range       { return g.Rng }

func (p *ProcedureCall) stmt()                  {}
func (p *ProcedureCall) Accept(vst Visitor) any { return vst.VisitProcedureCall(p) }
func (p *ProcedureCall) String() string {
	var args []string
	for _, arg := range p.ActualParams {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", p.Callee, strings.Join(args, ", "))
}
func (p *ProcedureCall) Position() *report.Position { return p.Pos }
func (p *ProcedureCall) Range() *report.Range       { return p.Rng }

func (w *WhileStmt) stmt()                      {}
func (w *WhileStmt) Accept(vst Visitor) any     { return vst.VisitWhileStmt(w) }
func (w *WhileStmt) String() string             { panic("not implement") }
func (w *WhileStmt) Position() *report.Position { return w.Pos }
func (w *WhileStmt) Range() *report.Range       { return w.Rng }

func (r *ReturnStmt) stmt()                  {}
func (r *ReturnStmt) Accept(vst Visitor) any { return vst.VisitReturnStmt(r) }
func (r *ReturnStmt) String() string {
	s := "return"
	if r.Value != nil {
		s += fmt.Sprintf(" %s", r.Value)
	}

	return s
}
func (r *ReturnStmt) Position() *report.Position { return r.Pos }
func (r *ReturnStmt) Range() *report.Range       { return r.Rng }

func (r *RepeatStmt) stmt()                      {}
func (r *RepeatStmt) Accept(vst Visitor) any     { return vst.VisitRepeatStmt(r) }
func (r *RepeatStmt) String() string             { panic("not implemented") }
func (r *RepeatStmt) Position() *report.Position { return r.Pos }
func (r *RepeatStmt) Range() *report.Range       { return r.Rng }

func (l *LoopStmt) stmt()                      {}
func (l *LoopStmt) Accept(vst Visitor) any     { return vst.VisitLoopStmt(l) }
func (l *LoopStmt) String() string             { panic("not implemented") }
func (l *LoopStmt) Position() *report.Position { return l.Pos }
func (l *LoopStmt) Range() *report.Range       { return l.Rng }

func (stmt *IfStmt) stmt()                      {}
func (stmt *IfStmt) Accept(vst Visitor) any     { return vst.VisitIfStmt(stmt) }
func (stmt *IfStmt) String() string             { panic("not implemented") }
func (stmt *IfStmt) Position() *report.Position { return stmt.Pos }
func (stmt *IfStmt) Range() *report.Range       { return stmt.Rng }

func (e *ElseIfBranch) Accept(vst Visitor) any     { return vst.VisitElseIfBranch(e) }
func (e *ElseIfBranch) String() string             { panic("not implemented") }
func (e *ElseIfBranch) Position() *report.Position { return e.Pos }
func (e *ElseIfBranch) Range() *report.Range       { return e.Rng }

func (a *AssignmentStmt) stmt()                      {}
func (a *AssignmentStmt) Accept(vst Visitor) any     { return vst.VisitAssignmentStmt(a) }
func (a *AssignmentStmt) String() string             { return fmt.Sprintf("%v := %v", a.LValue, a.RValue) }
func (a *AssignmentStmt) Position() *report.Position { return a.Pos }
func (a *AssignmentStmt) Range() *report.Range       { return a.Rng }

func (stmt *CaseStmt) stmt()                      {}
func (stmt *CaseStmt) Accept(vst Visitor) any     { return vst.VisitCaseStmt(stmt) }
func (stmt *CaseStmt) String() string             { panic("not implemented") }
func (stmt *CaseStmt) Position() *report.Position { return stmt.Pos }
func (stmt *CaseStmt) Range() *report.Range       { return stmt.Rng }

func (c *Case) Accept(vst Visitor) any { return vst.VisitCase(c) }
func (c *Case) String() string {
	var cases []string
	for _, label := range c.CaseLabelList {
		cases = append(cases, fmt.Sprintf("%s..%s", label.High, label.Low))
	}

	return fmt.Sprintf("case %s: %s", strings.Join(cases, ", "), c.StmtSeq)
}
func (c *Case) Position() *report.Position { return c.Pos }
func (c *Case) Range() *report.Range       { return c.Rng }

func (l *LabelRange) Accept(vst Visitor) any     { return vst.VisitLabelRange(l) }
func (l *LabelRange) String() string             { return fmt.Sprintf("%s..%s", l.High, l.Low) }
func (l *LabelRange) Position() *report.Position { return l.Pos }
func (l *LabelRange) Range() *report.Range       { return l.Rng }

func (b *BadStmt) stmt()                      {}
func (b *BadStmt) Accept(vst Visitor) any     { return vst.VisitBadStmt(b) }
func (b *BadStmt) String() string             { return "<BadStmt>" }
func (b *BadStmt) Position() *report.Position { return b.Pos }
func (b *BadStmt) Range() *report.Range       { return b.Rng }
