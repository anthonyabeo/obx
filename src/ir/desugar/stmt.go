package desugar

import (
	"bytes"
	"fmt"
	"strings"
)

type (
	AssignStmt struct {
		NodeBase
		Left  Expr
		Right Expr
	}

	ReturnStmt struct {
		NodeBase
		Result Expr
	}

	IfStmt struct {
		NodeBase
		Cond    Expr
		Then    *CompoundStmt
		Else    *CompoundStmt
		ElseIfs []*ElseIfBranch
	}

	ElseIfBranch struct {
		NodeBase
		Cond Expr
		Body *CompoundStmt
	}

	LoopStmt struct {
		NodeBase
		Body  *CompoundStmt
		Label string
	}

	CaseStmt struct {
		NodeBase
		Expr  Expr
		Cases []*Case
		Else  *CompoundStmt
	}

	Case struct {
		NodeBase
		Labels []*LabelRange
		Body   *CompoundStmt
	}

	LabelRange struct {
		NodeBase
		Low  Expr
		High Expr // same as Low for singleton values
	}

	WithStmt struct {
		NodeBase
		Guards []*WithGuard
		Else   *CompoundStmt
	}

	WithGuard struct {
		NodeBase
		Expr Expr
		Type Expr
		Body *CompoundStmt
	}

	ExitStmt struct {
		NodeBase
		LoopLabel string
	}

	CompoundStmt struct {
		NodeBase
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
