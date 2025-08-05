package hir

import (
	"bytes"
	"fmt"
	"strings"
)

type (
	AssignStmt struct {
		Left  Expr
		Right Expr
	}

	ReturnStmt struct {
		Result Expr
	}

	IfStmt struct {
		Cond    Expr
		Then    *CompoundStmt
		Else    *CompoundStmt
		ElseIfs []*ElseIfBranch
	}

	ElseIfBranch struct {
		Cond Expr
		Body *CompoundStmt
	}

	LoopStmt struct {
		Body  *CompoundStmt
		Label string
	}

	CaseStmt struct {
		Expr  Expr
		Cases []*Case
		Else  *CompoundStmt
	}

	Case struct {
		Labels []*LabelRange
		Body   *CompoundStmt
	}

	LabelRange struct {
		Low  Expr
		High Expr // same as Low for singleton values
	}

	WithStmt struct {
		Guards []*WithGuard
		Else   *CompoundStmt
	}

	WithGuard struct {
		Expr Expr
		Type Expr
		Body *CompoundStmt
	}

	ExitStmt struct {
		loopLabel string
	}

	CompoundStmt struct {
		Stmts []Stmt
	}
)

func (*AssignStmt) stmt() {}

func (*ReturnStmt) stmt()   {}
func (*IfStmt) stmt()       {}
func (*LoopStmt) stmt()     {}
func (*CaseStmt) stmt()     {}
func (*WithStmt) stmt()     {}
func (*ExitStmt) stmt()     {}
func (*CompoundStmt) stmt() {}
func (*FuncCall) stmt()     {}

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
