package ast

import (
	"fmt"
	"strings"
)

type (
	ForStmt struct {
		CtlVar   string
		InitVal  Expression
		FinalVal Expression
		By       Expression
		StmtSeq  []Statement

		Label string
	}

	ExitStmt struct {
		Label string
	}

	Guard struct {
		Expr    Expression
		Type    Expression
		StmtSeq []Statement
	}

	WithStmt struct {
		Arms []*Guard
		Else []Statement
	}

	ProcedureCall struct {
		Callee       *Designator
		ActualParams []Expression
	}

	WhileStmt struct {
		BoolExpr Expression
		StmtSeq  []Statement
		ElsIfs   []*ElsIfBranch

		Label string
	}

	ReturnStmt struct {
		Value Expression
	}

	RepeatStmt struct {
		StmtSeq  []Statement
		BoolExpr Expression

		Label string
	}

	LoopStmt struct {
		StmtSeq []Statement

		Label string
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
	}

	AssignmentStmt struct {
		LValue Expression
		RValue Expression
	}

	CaseStmt struct {
		Expr  Expression
		Cases []*Case
		Else  []Statement
	}

	Case struct {
		CaseLabelList []*LabelRange
		StmtSeq       []Statement
	}

	LabelRange struct {
		Begin Expression
		End   Expression
	}

	BadStmt struct{}
)

func (stmt *ForStmt) stmt()              {}
func (stmt *ForStmt) String() string     { panic("not implemented") }
func (stmt *ForStmt) Accept(vst Visitor) { vst.VisitForStmt(stmt) }

func (stmt *ExitStmt) stmt()              {}
func (stmt *ExitStmt) String() string     { panic("not implemented") }
func (stmt *ExitStmt) Accept(vst Visitor) { vst.VisitExitStmt(stmt) }

func (w *WithStmt) stmt()              {}
func (w *WithStmt) String() string     { panic("not implemented") }
func (w *WithStmt) Accept(vst Visitor) { vst.VisitWithStmt(w) }

func (p *ProcedureCall) stmt()              {}
func (p *ProcedureCall) Accept(vst Visitor) { vst.VisitProcCall(p) }
func (p *ProcedureCall) String() string {
	var args []string
	for _, arg := range p.ActualParams {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", p.Callee, strings.Join(args, ", "))
}

func (w *WhileStmt) stmt()              {}
func (w *WhileStmt) Accept(vst Visitor) { vst.VisitWhileStmt(w) }
func (w *WhileStmt) String() string     { panic("not implement") }

func (r *ReturnStmt) stmt()              {}
func (r *ReturnStmt) Accept(vst Visitor) { vst.VisitReturnStmt(r) }
func (r *ReturnStmt) String() string     { return fmt.Sprintf("return %v", r.Value) }

func (r *RepeatStmt) stmt()              {}
func (r *RepeatStmt) Accept(vst Visitor) { vst.VisitRepeatStmt(r) }
func (r *RepeatStmt) String() string     { panic("not implemented") }

func (l *LoopStmt) stmt()              {}
func (l *LoopStmt) Accept(vst Visitor) { vst.VisitLoopStmt(l) }
func (l *LoopStmt) String() string     { panic("not implemented") }

func (stmt *IfStmt) stmt()              {}
func (stmt *IfStmt) Accept(vst Visitor) { vst.VisitIfStmt(stmt) }
func (stmt *IfStmt) String() string     { panic("not implemented") }

func (a *AssignmentStmt) stmt()              {}
func (a *AssignmentStmt) Accept(vst Visitor) { vst.VisitAssignmentStmt(a) }
func (a *AssignmentStmt) String() string     { return fmt.Sprintf("%v := %v", a.LValue, a.RValue) }

func (stmt *CaseStmt) stmt()              {}
func (stmt *CaseStmt) Accept(vst Visitor) { vst.VisitCaseStmt(stmt) }
func (stmt *CaseStmt) String() string     { panic("not implemented") }

func (b *BadStmt) stmt()          {}
func (b *BadStmt) Accept(Visitor) { panic("you should not be here") }
func (b *BadStmt) String() string { return "<BadStmt>" }
