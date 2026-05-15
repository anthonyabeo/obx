package desugar

import (
	"bytes"
	"fmt"
	"strings"
)

type (
	AssignStmt struct {
		Left        Expr
		Right       Expr
		StartOffset int
		EndOffset   int
	}

	ReturnStmt struct {
		Result      Expr
		StartOffset int
		EndOffset   int
	}

	IfStmt struct {
		Cond        Expr
		Then        *CompoundStmt
		Else        *CompoundStmt
		ElseIfs     []*ElseIfBranch
		StartOffset int
		EndOffset   int
	}

	ElseIfBranch struct {
		Cond        Expr
		Body        *CompoundStmt
		StartOffset int
		EndOffset   int
	}

	LoopStmt struct {
		Body        *CompoundStmt
		Label       string
		StartOffset int
		EndOffset   int
	}

	CaseStmt struct {
		Expr        Expr
		Cases       []*Case
		Else        *CompoundStmt
		StartOffset int
		EndOffset   int
	}

	Case struct {
		Labels      []*LabelRange
		Body        *CompoundStmt
		StartOffset int
		EndOffset   int
	}

	LabelRange struct {
		Low         Expr
		High        Expr // same as Low for singleton values
		StartOffset int
		EndOffset   int
	}

	WithStmt struct {
		Guards      []*WithGuard
		Else        *CompoundStmt
		StartOffset int
		EndOffset   int
	}

	WithGuard struct {
		Expr        Expr
		Type        Expr
		Body        *CompoundStmt
		StartOffset int
		EndOffset   int
	}

	ExitStmt struct {
		LoopLabel   string
		StartOffset int
		EndOffset   int
	}

	CompoundStmt struct {
		Stmts []Stmt
	}
)

func (*AssignStmt) stmt()   {}
func (*ReturnStmt) stmt()   {}
func (*IfStmt) stmt()       {}
func (*LoopStmt) stmt()     {}
func (*CaseStmt) stmt()     {}
func (*WithStmt) stmt()     {}
func (*ExitStmt) stmt()     {}
func (*CompoundStmt) stmt() {}
func (*FuncCall) stmt()     {}

func (stmt *AssignStmt) Pos() int { return stmt.StartOffset }
func (stmt *ReturnStmt) Pos() int { return stmt.StartOffset }
func (stmt *IfStmt) Pos() int     { return stmt.StartOffset }
func (stmt *LoopStmt) Pos() int   { return stmt.StartOffset }
func (stmt *CaseStmt) Pos() int   { return stmt.StartOffset }
func (stmt *WithStmt) Pos() int   { return stmt.StartOffset }
func (stmt *ExitStmt) Pos() int   { return stmt.StartOffset }
func (stmt *CompoundStmt) Pos() int {
	if len(stmt.Stmts) == 0 {
		return -1
	}
	return stmt.Stmts[0].Pos()
}

func (stmt *AssignStmt) End() int { return stmt.EndOffset }
func (stmt *ReturnStmt) End() int { return stmt.EndOffset }
func (stmt *IfStmt) End() int     { return stmt.EndOffset }
func (stmt *LoopStmt) End() int   { return stmt.EndOffset }
func (stmt *CaseStmt) End() int   { return stmt.EndOffset }
func (stmt *WithStmt) End() int   { return stmt.EndOffset }
func (stmt *ExitStmt) End() int   { return stmt.EndOffset }
func (stmt *CompoundStmt) End() int {
	if len(stmt.Stmts) == 0 {
		return -1
	}
	return stmt.Stmts[0].End()
}

func (stmt *AssignStmt) String() string { return fmt.Sprintf("%s := %s", stmt.Left, stmt.Right) }
func (stmt *ReturnStmt) String() string {
	if stmt.Result != nil {
		return fmt.Sprintf("return %s", stmt.Result)
	}

	return "return"
}
func (stmt *IfStmt) String() string   { panic("not implemented") }
func (stmt *LoopStmt) String() string { return fmt.Sprintf("LOOP %s END", stmt.Body) }
func (stmt *CaseStmt) String() string {
	out := fmt.Sprintf("Case %s of\n", stmt.Expr.String())
	for _, alt := range stmt.Cases {
		labels := make([]string, len(alt.Labels))
		for i, lbl := range alt.Labels {
			if lbl.Low != nil && lbl.High != nil {
				labels[i] = fmt.Sprintf("%s..%s", lbl.Low.String(), lbl.High.String())
			} else {
				labels[i] = lbl.Low.String()
			}
		}
		out += fmt.Sprintf("%s =>\n", strings.Join(labels, ", "))
		for _, s := range alt.Body.Stmts {
			out += s.String() + "\n"
		}
	}
	if len(stmt.Else.Stmts) > 0 {
		out += "  else =>\n"
		for _, b := range stmt.Else.Stmts {
			out += b.String() + "\n"
		}
	}
	out += "END"
	return out
}
func (stmt *WithStmt) String() string { panic("not implemented") }
func (stmt *CompoundStmt) String() string {
	buf := new(bytes.Buffer)

	for _, s := range stmt.Stmts {
		buf.WriteString(s.String() + "\n")
	}

	return buf.String()
}
func (stmt *ExitStmt) String() string { return "EXIT" }
