package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type (
	ForStmt struct {
		For      *token.Position
		CtlVar   *Ident
		InitVal  Expression
		FinalVal Expression
		By       Expression
		StmtSeq  []Statement
	}

	ExitStmt struct {
		Exit *token.Position
	}

	Guard struct {
		Expr    Expression
		Type    Expression
		StmtSeq []Statement
	}

	WithStmt struct {
		With *token.Position
		Arms []*Guard
		Else []Statement
	}

	ProcCall struct {
		NamePos      *token.Position
		Dsg          *Designator
		ActualParams []Expression
	}

	WhileStmt struct {
		While    *token.Position
		BoolExpr Expression
		StmtSeq  []Statement
		ElsIfs   []*ElsIfBranch
	}

	ReturnStmt struct {
		Return *token.Position
		Value  Expression
	}

	RepeatStmt struct {
		Repeat   *token.Position
		StmtSeq  []Statement
		BoolExpr Expression
	}

	LoopStmt struct {
		Loop    *token.Position
		StmtSeq []Statement
	}

	ElsIfBranch struct {
		BoolExpr Expression
		ThenPath []Statement
	}

	IfStmt struct {
		If             *token.Position
		BoolExpr       Expression
		ThenPath       []Statement
		ElseIfBranches []*ElsIfBranch
		ElsePath       []Statement
	}

	AssignStmt struct {
		AssignPos *token.Position
		LValue    Expression
		RValue    Expression
	}

	CaseStmt struct {
		Case  *token.Position
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
)

func (stmt *ForStmt) stmt()                {}
func (stmt *ForStmt) Pos() *token.Position { return stmt.For }
func (stmt *ForStmt) End() *token.Position { panic("not implemented") }
func (stmt *ForStmt) String() string       { panic("not implemented") }
func (stmt *ForStmt) Accept(vst Visitor)   { vst.VisitForStmt(stmt) }

func (stmt *ExitStmt) stmt()                {}
func (stmt *ExitStmt) Pos() *token.Position { return stmt.Exit }
func (stmt *ExitStmt) End() *token.Position { panic("not implemented") }
func (stmt *ExitStmt) String() string       { panic("not implemented") }
func (stmt *ExitStmt) Accept(vst Visitor)   { vst.VisitExitStmt(stmt) }

func (w *WithStmt) stmt()                {}
func (w *WithStmt) Pos() *token.Position { return w.With }
func (w *WithStmt) End() *token.Position { panic("not implemented") }
func (w *WithStmt) String() string       { panic("not implemented") }
func (w *WithStmt) Accept(vst Visitor)   { vst.VisitWithStmt(w) }

func (p *ProcCall) Pos() *token.Position { return p.NamePos }
func (p *ProcCall) End() *token.Position { panic("not implemented") }
func (p *ProcCall) Accept(vst Visitor)   { vst.VisitProcCall(p) }
func (p *ProcCall) stmt()                {}
func (p *ProcCall) String() string       { panic("not implemented") }

func (w *WhileStmt) Pos() *token.Position { return w.While }
func (w *WhileStmt) End() *token.Position { panic("not implemented") }
func (w *WhileStmt) Accept(vst Visitor)   { vst.VisitWhileStmt(w) }
func (w *WhileStmt) stmt()                {}
func (w *WhileStmt) String() string       { panic("not implement") }

func (r *ReturnStmt) Pos() *token.Position { return r.Return }
func (r *ReturnStmt) End() (pos *token.Position) {
	if r.Value != nil {
		pos = r.Value.End()
	} else {
		pos = r.Pos()
		pos.Column += len("return")
	}

	return
}
func (r *ReturnStmt) Accept(vst Visitor) { vst.VisitReturnStmt(r) }
func (r *ReturnStmt) stmt()              {}
func (r *ReturnStmt) String() string     { return fmt.Sprintf("return %v", r.Value) }

func (r *RepeatStmt) Pos() *token.Position { return r.Repeat }
func (r *RepeatStmt) End() *token.Position { panic("not implemented") }
func (r *RepeatStmt) Accept(vst Visitor)   { vst.VisitRepeatStmt(r) }
func (r *RepeatStmt) stmt()                {}
func (r *RepeatStmt) String() string       { panic("not implemented") }

func (l *LoopStmt) Pos() *token.Position { return l.Loop }
func (l *LoopStmt) End() *token.Position { panic("not implemented") }
func (l *LoopStmt) Accept(vst Visitor)   { vst.VisitLoopStmt(l) }
func (l *LoopStmt) stmt()                {}
func (l *LoopStmt) String() string       { panic("not implemented") }

func (stmt *IfStmt) Pos() *token.Position { return stmt.If }
func (stmt *IfStmt) End() *token.Position { panic("not implemented") }
func (stmt *IfStmt) Accept(vst Visitor)   { vst.VisitIfStmt(stmt) }
func (stmt *IfStmt) stmt()                {}
func (stmt *IfStmt) String() string       { panic("not implemented") }

func (a *AssignStmt) Pos() *token.Position { return a.AssignPos }
func (a *AssignStmt) End() *token.Position { return a.RValue.End() }
func (a *AssignStmt) Accept(vst Visitor)   { vst.VisitAssignStmt(a) }
func (a *AssignStmt) stmt()                {}
func (a *AssignStmt) String() string       { return fmt.Sprintf("%v := %v", a.LValue, a.RValue) }

func (stmt *CaseStmt) Pos() *token.Position { return stmt.Case }
func (stmt *CaseStmt) End() *token.Position { panic("not implemented") }
func (stmt *CaseStmt) Accept(vst Visitor)   { vst.VisitCaseStmt(stmt) }
func (stmt *CaseStmt) stmt()                {}
func (stmt *CaseStmt) String() string       { panic("not implemented") }
