package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

// IdentProps is a set of flags denoting the properties of an identifier
type IdentProps int

// Properties of an identifier
const (
	Exported IdentProps = 1 << iota
	ReadOnly
	Predeclared
	Unexported

	ExportedReadOnly = Exported | ReadOnly
)

type (
	IdentifierDef struct {
		Name  string
		Props IdentProps
	}

	BasicLit struct {
		Kind token.Kind // token.INT, token.REAL, token.HEXCHAR, or token.STRING
		Val  string     // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
	}

	BinaryExpr struct {
		Left, Right Expression // operands
		Op          token.Kind // operator
	}

	FunctionCall struct {
		Callee       Expression
		ActualParams []Expression
	}

	QualifiedIdent struct {
		Prefix string
		Name   string
	}

	ExprRange struct {
		Beg Expression
		End Expression
	}

	Set struct {
		Elem []Expression
	}

	UnaryExpr struct {
		Op      token.Kind
		Operand Expression
	}

	Designator struct {
		QualifiedIdent Expression
		Select         Selector
	}

	BadExpr struct{}
)

// func (id *IdentifierDef) expr()              {}
// func (id *IdentifierDef) Accept(vst Visitor) { vst.VisitIdentifierDef(id) }
func (id *IdentifierDef) String() string {
	name := id.Name
	switch id.Props {
	case ExportedReadOnly:
		name += "-"
	case Exported:
		name += "*"
	default:
	}

	return name
}

func (e *ExprRange) String() string     { return fmt.Sprintf("%s..%s", e.Beg, e.End) }
func (e *ExprRange) Accept(vst Visitor) { vst.VisitExprRange(e) }
func (e *ExprRange) expr()              {}

func (b *BasicLit) expr()              {}
func (b *BasicLit) String() string     { return b.Val }
func (b *BasicLit) Accept(vst Visitor) { vst.VisitBasicLit(b) }

func (b *BinaryExpr) expr()              {}
func (b *BinaryExpr) String() string     { return fmt.Sprintf("%v %v %v", b.Left, b.Op, b.Right) }
func (b *BinaryExpr) Accept(vst Visitor) { vst.VisitBinaryExpr(b) }

func (f *FunctionCall) Accept(vst Visitor) { vst.VisitFunctionCall(f) }
func (f *FunctionCall) expr()              {}
func (f *FunctionCall) String() string {
	var args []string
	for _, arg := range f.ActualParams {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", f.Callee, strings.Join(args, ", "))
}

func (q *QualifiedIdent) Accept(vst Visitor) { vst.VisitQualifiedIdent(q) }
func (q *QualifiedIdent) expr()              {}
func (q *QualifiedIdent) String() string {
	if q.Prefix == "" {
		return q.Name
	}
	return fmt.Sprintf("%v.%v", q.Prefix, q.Name)
}

func (s *Set) expr() {}
func (s *Set) String() string {
	var elems []string
	for _, elem := range s.Elem {
		elems = append(elems, elem.String())
	}

	return fmt.Sprintf("{%s}", strings.Join(elems, ", "))
}
func (s *Set) Accept(vst Visitor) { vst.VisitSet(s) }

func (u *UnaryExpr) Accept(vst Visitor) { vst.VisitUnaryExpr(u) }
func (u *UnaryExpr) expr()              {}
func (u *UnaryExpr) String() string     { return fmt.Sprintf("%v%v", u.Op, u.Operand) }

func (d *Designator) Accept(vst Visitor) { vst.VisitDesignator(d) }
func (d *Designator) expr()              {}
func (d *Designator) String() string {
	s := d.QualifiedIdent.String()
	if d.Select != nil {
		s += d.Select.String()
	}

	return s
}

func (b *BadExpr) Accept(Visitor) { panic("not implemented") }
func (b *BadExpr) expr()          {}
func (b *BadExpr) String() string { panic("not implemented") }

// Selectors
// ---------------------

type Selector interface {
	sel()
	String() string
}

// DotOp
// ---------------------
type DotOp struct {
	Field string
}

func (d *DotOp) String() string { return fmt.Sprintf(".%s", d.Field) }
func (d *DotOp) sel()           {}

// IndexOp
// ---------------------
type IndexOp struct {
	List []Expression
}

func (id *IndexOp) String() string {
	var list []string
	for _, expr := range id.List {
		list = append(list, expr.String())
	}

	return fmt.Sprintf("[%v]", strings.Join(list, ","))
}
func (id *IndexOp) sel() {}

// TypeGuard
// ---------------------
type TypeGuard struct {
	Ty Expression
}

func (t *TypeGuard) String() string { return fmt.Sprintf("{%s}", t.Ty) }
func (t *TypeGuard) sel()           {}

// PtrDeref
// ---------------------
type PtrDeref struct{}

func (deref *PtrDeref) String() string { panic("not implement") }
func (deref *PtrDeref) sel()           {}
