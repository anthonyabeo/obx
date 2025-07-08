package hir

import (
	"bytes"
	"fmt"
	"strings"
)

type (
	AssignStmt struct {
		Lhs Expr
		Rhs Expr
	}

	CallStmt struct {
		Proc Expr
		Args []Expr
	}

	ReturnStmt struct {
		Result Expr // nil for procedures
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

	WhileStmt struct {
		Cond    Expr
		Body    *CompoundStmt
		ElseIfs []*ElseIfBranch
	}

	LoopStmt struct {
		Body  *CompoundStmt
		label string
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

func (*AssignStmt) isStmt()   {}
func (*CallStmt) isStmt()     {}
func (*ReturnStmt) isStmt()   {}
func (*IfStmt) isStmt()       {}
func (*WhileStmt) isStmt()    {}
func (*LoopStmt) isStmt()     {}
func (*CaseStmt) isStmt()     {}
func (*WithStmt) isStmt()     {}
func (*ExitStmt) isStmt()     {}
func (*CompoundStmt) isStmt() {}

func (stmt *AssignStmt) String() string { return fmt.Sprintf("%s := %s", stmt.Lhs, stmt.Rhs) }
func (stmt *CallStmt) String() string {
	var args []string
	for _, arg := range stmt.Args {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", stmt.Proc, strings.Join(args, ", "))
}
func (stmt *ReturnStmt) String() string {
	if stmt.Result != nil {
		return fmt.Sprintf("RETURN %s", stmt.Result)
	}

	return "RETURN"
}
func (stmt *IfStmt) String() string { panic("not implemented") }
func (stmt *WhileStmt) String() string {
	return fmt.Sprintf("WHILE %s DO\n %s\n  END", stmt.Cond, stmt.Body)
}
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
