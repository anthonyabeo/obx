package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/types"
)

type (
	ForStmt struct {
		CtlVar   *IdentifierDef
		InitVal  Expression
		FinalVal Expression
		By       Expression
		StmtSeq  []Statement

		Label string

		StartOffset int
		EndOffset   int
	}

	ExitStmt struct {
		Label       string
		StartOffset int
		EndOffset   int
	}

	Guard struct {
		Expr        Expression
		Type        Expression
		StmtSeq     []Statement
		StartOffset int
		EndOffset   int
	}

	WithStmt struct {
		Arms        []*Guard
		Else        []Statement
		StartOffset int
		EndOffset   int
	}

	ProcedureCall struct {
		Callee       *Designator
		ActualParams []Expression
		StartOffset  int
		EndOffset    int
	}

	WhileStmt struct {
		BoolExpr    Expression
		StmtSeq     []Statement
		ElsIfs      []*ElseIfBranch
		Label       string
		StartOffset int
		EndOffset   int
	}

	ReturnStmt struct {
		Value       Expression
		StartOffset int
		EndOffset   int
	}

	RepeatStmt struct {
		StmtSeq     []Statement
		BoolExpr    Expression
		Label       string
		StartOffset int
		EndOffset   int
	}

	LoopStmt struct {
		StmtSeq []Statement

		Label string

		StartOffset int
		EndOffset   int
	}

	ElseIfBranch struct {
		BoolExpr    Expression
		ThenPath    []Statement
		StartOffset int
		EndOffset   int
	}

	IfStmt struct {
		BoolExpr       Expression
		ThenPath       []Statement
		ElseIfBranches []*ElseIfBranch
		ElsePath       []Statement
		StartOffset    int
		EndOffset      int
	}

	AssignmentStmt struct {
		LValue Expression
		RValue Expression

		StartOffset int
		EndOffset   int
	}

	CaseStmt struct {
		Expr  Expression
		Cases []*Case
		Else  []Statement

		StartOffset int
		EndOffset   int
	}

	Case struct {
		CaseLabelList []*LabelRange
		StmtSeq       []Statement

		StartOffset int
		EndOffset   int
	}

	LabelRange struct {
		High     Expression
		Low      Expression
		SemaType types.Type

		StartOffset int
		EndOffset   int
	}

	BadStmt struct {
		StartOffset int
		EndOffset   int
	}
)

func (stmt *ForStmt) stmt()                  {}
func (stmt *ForStmt) String() string         { panic("not implemented") }
func (stmt *ForStmt) Accept(vst Visitor) any { return vst.VisitForStmt(stmt) }
func (stmt *ForStmt) Pos() int               { return stmt.StartOffset }
func (stmt *ForStmt) End() int               { return stmt.EndOffset }
func (stmt *ForStmt) Children() []Node {
	children := []Node{stmt.By, stmt.CtlVar, stmt.InitVal, stmt.FinalVal}

	for _, statement := range stmt.StmtSeq {
		children = append(children, statement)
	}

	return children
}

func (stmt *ExitStmt) stmt()                  {}
func (stmt *ExitStmt) String() string         { return "exit" }
func (stmt *ExitStmt) Accept(vst Visitor) any { return vst.VisitExitStmt(stmt) }
func (stmt *ExitStmt) Pos() int               { return stmt.StartOffset }
func (stmt *ExitStmt) End() int               { return stmt.EndOffset }
func (stmt *ExitStmt) Children() []Node       { return []Node{} }

func (w *WithStmt) stmt()                  {}
func (w *WithStmt) String() string         { panic("not implemented") }
func (w *WithStmt) Accept(vst Visitor) any { return vst.VisitWithStmt(w) }
func (w *WithStmt) Pos() int               { return w.StartOffset }
func (w *WithStmt) End() int               { return w.EndOffset }
func (w *WithStmt) Children() []Node {
	children := make([]Node, 0)

	for _, arm := range w.Arms {
		children = append(children, arm)
	}

	for _, statement := range w.Else {
		children = append(children, statement)
	}

	return children
}

func (g *Guard) Accept(vst Visitor) any { return vst.VisitGuard(g) }
func (g *Guard) String() string {
	var stmts []string
	for _, stmt := range g.StmtSeq {
		stmts = append(stmts, stmt.String())
	}

	return fmt.Sprintf("guard %s: %s", g.Expr, strings.Join(stmts, "; "))
}
func (g *Guard) Pos() int { return g.StartOffset }
func (g *Guard) End() int { return g.EndOffset }
func (g *Guard) Children() []Node {
	children := []Node{g.Expr, g.Type}

	for _, statement := range g.StmtSeq {
		children = append(children, statement)
	}

	return children
}

func (p *ProcedureCall) stmt()                  {}
func (p *ProcedureCall) Accept(vst Visitor) any { return vst.VisitProcedureCall(p) }
func (p *ProcedureCall) String() string {
	var args []string
	for _, arg := range p.ActualParams {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", p.Callee, strings.Join(args, ", "))
}
func (p *ProcedureCall) Pos() int { return p.StartOffset }
func (p *ProcedureCall) End() int { return p.EndOffset }
func (p *ProcedureCall) Children() []Node {
	children := []Node{p.Callee}

	for _, param := range p.ActualParams {
		children = append(children, param)
	}

	return children
}

func (w *WhileStmt) stmt()                  {}
func (w *WhileStmt) Accept(vst Visitor) any { return vst.VisitWhileStmt(w) }
func (w *WhileStmt) String() string         { panic("not implement") }
func (w *WhileStmt) Pos() int               { return w.StartOffset }
func (w *WhileStmt) End() int               { return w.EndOffset }
func (w *WhileStmt) Children() []Node {
	children := []Node{w.BoolExpr}

	for _, statement := range w.StmtSeq {
		children = append(children, statement)
	}

	for _, elsIf := range w.ElsIfs {
		children = append(children, elsIf)
	}

	return children
}

func (r *ReturnStmt) stmt()                  {}
func (r *ReturnStmt) Accept(vst Visitor) any { return vst.VisitReturnStmt(r) }
func (r *ReturnStmt) String() string {
	s := "return"
	if r.Value != nil {
		s += fmt.Sprintf(" %s", r.Value)
	}

	return s
}
func (r *ReturnStmt) Pos() int { return r.StartOffset }
func (r *ReturnStmt) End() int { return r.EndOffset }
func (r *ReturnStmt) Children() []Node {
	children := make([]Node, 0)
	if r.Value != nil {
		children = append(children, r.Value)
	}

	return children
}

func (r *RepeatStmt) stmt()                  {}
func (r *RepeatStmt) Accept(vst Visitor) any { return vst.VisitRepeatStmt(r) }
func (r *RepeatStmt) String() string         { panic("not implemented") }
func (r *RepeatStmt) Pos() int               { return r.StartOffset }
func (r *RepeatStmt) End() int               { return r.EndOffset }
func (r *RepeatStmt) Children() []Node {
	children := []Node{r.BoolExpr}

	for _, statement := range r.StmtSeq {
		children = append(children, statement)
	}

	return children
}

func (l *LoopStmt) stmt()                  {}
func (l *LoopStmt) Accept(vst Visitor) any { return vst.VisitLoopStmt(l) }
func (l *LoopStmt) String() string         { panic("not implemented") }
func (l *LoopStmt) Pos() int               { return l.StartOffset }
func (l *LoopStmt) End() int               { return l.EndOffset }
func (l *LoopStmt) Children() []Node {
	children := make([]Node, 0)
	for _, statement := range l.StmtSeq {
		children = append(children, statement)
	}

	return children
}

func (stmt *IfStmt) stmt()                  {}
func (stmt *IfStmt) Accept(vst Visitor) any { return vst.VisitIfStmt(stmt) }
func (stmt *IfStmt) String() string         { panic("not implemented") }
func (stmt *IfStmt) Pos() int               { return stmt.StartOffset }
func (stmt *IfStmt) End() int               { return stmt.EndOffset }
func (stmt *IfStmt) Children() []Node {
	children := []Node{stmt.BoolExpr}

	for _, statement := range stmt.ThenPath {
		children = append(children, statement)
	}

	for _, branch := range stmt.ElseIfBranches {
		children = append(children, branch)
	}

	for _, statement := range stmt.ElsePath {
		children = append(children, statement)
	}

	return children

}

func (e *ElseIfBranch) Accept(vst Visitor) any { return vst.VisitElseIfBranch(e) }
func (e *ElseIfBranch) String() string         { panic("not implemented") }
func (e *ElseIfBranch) Pos() int               { return e.StartOffset }
func (e *ElseIfBranch) End() int               { return e.EndOffset }
func (e *ElseIfBranch) Children() []Node {
	children := []Node{e.BoolExpr}
	for _, statement := range e.ThenPath {
		children = append(children, statement)
	}

	return children
}

func (a *AssignmentStmt) stmt()                  {}
func (a *AssignmentStmt) Accept(vst Visitor) any { return vst.VisitAssignmentStmt(a) }
func (a *AssignmentStmt) String() string         { return fmt.Sprintf("%v := %v", a.LValue, a.RValue) }
func (a *AssignmentStmt) Pos() int               { return a.StartOffset }
func (a *AssignmentStmt) End() int               { return a.EndOffset }
func (a *AssignmentStmt) Children() []Node       { return []Node{a.LValue, a.RValue} }

func (stmt *CaseStmt) stmt()                  {}
func (stmt *CaseStmt) Accept(vst Visitor) any { return vst.VisitCaseStmt(stmt) }
func (stmt *CaseStmt) String() string         { panic("not implemented") }
func (stmt *CaseStmt) Pos() int               { return stmt.StartOffset }
func (stmt *CaseStmt) End() int               { return stmt.EndOffset }
func (stmt *CaseStmt) Children() []Node {
	children := []Node{stmt.Expr}

	for _, c := range stmt.Cases {
		children = append(children, c)
	}

	for _, statement := range stmt.Else {
		children = append(children, statement)
	}

	return children
}

func (c *Case) Accept(vst Visitor) any { return vst.VisitCase(c) }
func (c *Case) String() string {
	var cases []string
	for _, label := range c.CaseLabelList {
		cases = append(cases, fmt.Sprintf("%s..%s", label.High, label.Low))
	}

	return fmt.Sprintf("case %s: %s", strings.Join(cases, ", "), c.StmtSeq)
}
func (c *Case) Pos() int { return c.StartOffset }
func (c *Case) End() int { return c.EndOffset }
func (c *Case) Children() []Node {
	children := make([]Node, 0)

	for _, labelRange := range c.CaseLabelList {
		children = append(children, labelRange)
	}

	for _, statement := range c.StmtSeq {
		children = append(children, statement)
	}

	return children
}

func (l *LabelRange) Accept(vst Visitor) any { return vst.VisitLabelRange(l) }
func (l *LabelRange) String() string {
	if l.High == nil {
		return l.Low.String()
	}

	return fmt.Sprintf("%s..%s", l.High, l.Low)
}
func (l *LabelRange) Pos() int         { return l.StartOffset }
func (l *LabelRange) End() int         { return l.EndOffset }
func (l *LabelRange) Children() []Node { return []Node{l.Low, l.High} }

func (b *BadStmt) stmt()                  {}
func (b *BadStmt) Accept(vst Visitor) any { return vst.VisitBadStmt(b) }
func (b *BadStmt) String() string         { return "<BadStmt>" }
func (b *BadStmt) Pos() int               { return b.StartOffset }
func (b *BadStmt) End() int               { return b.EndOffset }
func (b *BadStmt) Children() []Node       { return []Node{} }
