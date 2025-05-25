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
		ElsIfs   []*ElsIfBranch
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

	ElsIfBranch struct {
		BoolExpr Expression
		ThenPath []Statement
	}

	IfStmt struct {
		BoolExpr       Expression
		ThenPath       []Statement
		ElseIfBranches []*ElsIfBranch
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
		Begin Expression
		End   Expression
		Pos   *report.Position
		Rng   *report.Range
	}

	BadStmt struct {
		Pos *report.Position
		Rng *report.Range
	}
)

func (stmt *ForStmt) stmt()                      {}
func (stmt *ForStmt) String() string             { panic("not implemented") }
func (stmt *ForStmt) Accept(vst Visitor)         { vst.VisitForStmt(stmt) }
func (stmt *ForStmt) Position() *report.Position { return stmt.Pos }
func (stmt *ForStmt) Range() *report.Range       { return stmt.Rng }

func (stmt *ExitStmt) stmt()                      {}
func (stmt *ExitStmt) String() string             { panic("not implemented") }
func (stmt *ExitStmt) Accept(vst Visitor)         { vst.VisitExitStmt(stmt) }
func (stmt *ExitStmt) Position() *report.Position { return stmt.Pos }
func (stmt *ExitStmt) Range() *report.Range       { return stmt.Rng }

func (w *WithStmt) stmt()                      {}
func (w *WithStmt) String() string             { panic("not implemented") }
func (w *WithStmt) Accept(vst Visitor)         { vst.VisitWithStmt(w) }
func (w *WithStmt) Position() *report.Position { return w.Pos }
func (w *WithStmt) Range() *report.Range       { return w.Rng }

func (p *ProcedureCall) stmt()              {}
func (p *ProcedureCall) Accept(vst Visitor) { vst.VisitProcedureCall(p) }
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
func (w *WhileStmt) Accept(vst Visitor)         { vst.VisitWhileStmt(w) }
func (w *WhileStmt) String() string             { panic("not implement") }
func (w *WhileStmt) Position() *report.Position { return w.Pos }
func (w *WhileStmt) Range() *report.Range       { return w.Rng }

func (r *ReturnStmt) stmt()              {}
func (r *ReturnStmt) Accept(vst Visitor) { vst.VisitReturnStmt(r) }
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
func (r *RepeatStmt) Accept(vst Visitor)         { vst.VisitRepeatStmt(r) }
func (r *RepeatStmt) String() string             { panic("not implemented") }
func (r *RepeatStmt) Position() *report.Position { return r.Pos }
func (r *RepeatStmt) Range() *report.Range       { return r.Rng }

func (l *LoopStmt) stmt()                      {}
func (l *LoopStmt) Accept(vst Visitor)         { vst.VisitLoopStmt(l) }
func (l *LoopStmt) String() string             { panic("not implemented") }
func (l *LoopStmt) Position() *report.Position { return l.Pos }
func (l *LoopStmt) Range() *report.Range       { return l.Rng }

func (stmt *IfStmt) stmt()                      {}
func (stmt *IfStmt) Accept(vst Visitor)         { vst.VisitIfStmt(stmt) }
func (stmt *IfStmt) String() string             { panic("not implemented") }
func (stmt *IfStmt) Position() *report.Position { return stmt.Pos }
func (stmt *IfStmt) Range() *report.Range       { return stmt.Rng }

func (a *AssignmentStmt) stmt()                      {}
func (a *AssignmentStmt) Accept(vst Visitor)         { vst.VisitAssignmentStmt(a) }
func (a *AssignmentStmt) String() string             { return fmt.Sprintf("%v := %v", a.LValue, a.RValue) }
func (a *AssignmentStmt) Position() *report.Position { return a.Pos }
func (a *AssignmentStmt) Range() *report.Range       { return a.Rng }

func (stmt *CaseStmt) stmt()                      {}
func (stmt *CaseStmt) Accept(vst Visitor)         { vst.VisitCaseStmt(stmt) }
func (stmt *CaseStmt) String() string             { panic("not implemented") }
func (stmt *CaseStmt) Position() *report.Position { return stmt.Pos }
func (stmt *CaseStmt) Range() *report.Range       { return stmt.Rng }

func (b *BadStmt) stmt()                      {}
func (b *BadStmt) Accept(Visitor)             { panic("you should not be here") }
func (b *BadStmt) String() string             { return "<BadStmt>" }
func (b *BadStmt) Position() *report.Position { return b.Pos }
func (b *BadStmt) Range() *report.Range       { return b.Rng }
