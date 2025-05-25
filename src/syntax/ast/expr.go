package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/report"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

// IdentProps is a set of flags denoting the properties of an identifier
type IdentProps int

// Properties of an identifier
const (
	Exported IdentProps = 1 << iota
	Unexported
	ReadOnly
	Predeclared

	ExportedReadOnly = Exported | ReadOnly
)

type (
	IdentifierDef struct {
		Name  string
		Props IdentProps

		Pos *report.Position
		Rng *report.Range
	}

	BasicLit struct {
		Kind token.Kind // token.INT, token.REAL, token.HEXCHAR, or token.STRING
		Val  string     // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`

		Pos *report.Position
		Rng *report.Range
	}

	BinaryExpr struct {
		Left, Right Expression // operands
		Op          token.Kind // operator

		Pos *report.Position
		Rng *report.Range
	}

	FunctionCall struct {
		Callee       *Designator
		ActualParams []Expression

		Pos *report.Position
		Rng *report.Range
	}

	QualifiedIdent struct {
		Prefix string
		Name   string

		Pos *report.Position
		Rng *report.Range
	}

	ExprRange struct {
		Beg Expression
		End Expression

		Pos *report.Position
		Rng *report.Range
	}

	Set struct {
		Elem []Expression
		Pos  *report.Position
		Rng  *report.Range
	}

	UnaryExpr struct {
		Op      token.Kind
		Operand Expression

		Pos *report.Position
		Rng *report.Range
	}

	Designator struct {
		QIdent *QualifiedIdent
		Select []Selector

		Pos *report.Position
		Rng *report.Range
	}

	Nil struct {
		Pos *report.Position
		Rng *report.Range
	}

	BadExpr struct {
		Pos *report.Position
		Rng *report.Range
	}
)

func (*Nil) String() string               { return "nil" }
func (n *Nil) Accept(vst Visitor)         { vst.VisitNil(n) }
func (*Nil) expr()                        {}
func (n *Nil) Position() *report.Position { return n.Pos }
func (n *Nil) Range() *report.Range       { return n.Rng }

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
func (id *IdentifierDef) Position() *report.Position { return id.Pos }
func (id *IdentifierDef) Range() *report.Range       { return id.Rng }

func (e *ExprRange) String() string             { return fmt.Sprintf("%s..%s", e.Beg, e.End) }
func (e *ExprRange) Accept(vst Visitor)         { vst.VisitExprRange(e) }
func (e *ExprRange) expr()                      {}
func (e *ExprRange) Position() *report.Position { panic("not implemented") }
func (e *ExprRange) Range() *report.Range       { panic("not implemented") }

func (b *BasicLit) expr()                      {}
func (b *BasicLit) String() string             { return b.Val }
func (b *BasicLit) Accept(vst Visitor)         { vst.VisitBasicLit(b) }
func (b *BasicLit) Position() *report.Position { return b.Pos }
func (b *BasicLit) Range() *report.Range       { return b.Rng }

func (b *BinaryExpr) expr()                      {}
func (b *BinaryExpr) String() string             { return fmt.Sprintf("%v %v %v", b.Left, b.Op, b.Right) }
func (b *BinaryExpr) Accept(vst Visitor)         { vst.VisitBinaryExpr(b) }
func (b *BinaryExpr) Position() *report.Position { return b.Pos }
func (b *BinaryExpr) Range() *report.Range       { return b.Rng }

func (f *FunctionCall) Accept(vst Visitor) { vst.VisitFunctionCall(f) }
func (f *FunctionCall) expr()              {}
func (f *FunctionCall) String() string {
	var args []string
	for _, arg := range f.ActualParams {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", f.Callee, strings.Join(args, ", "))
}
func (f *FunctionCall) Position() *report.Position { return f.Pos }
func (f *FunctionCall) Range() *report.Range       { return f.Rng }

func (q *QualifiedIdent) Accept(vst Visitor) { vst.VisitQualifiedIdent(q) }
func (q *QualifiedIdent) expr()              {}
func (q *QualifiedIdent) String() string {
	if q.Prefix == "" {
		return q.Name
	}
	return fmt.Sprintf("%v.%v", q.Prefix, q.Name)
}
func (q *QualifiedIdent) Position() *report.Position { return q.Pos }
func (q *QualifiedIdent) Range() *report.Range       { return q.Rng }

func (s *Set) expr() {}
func (s *Set) String() string {
	var elems []string
	for _, elem := range s.Elem {
		elems = append(elems, elem.String())
	}

	return fmt.Sprintf("{%s}", strings.Join(elems, ", "))
}
func (s *Set) Accept(vst Visitor)         { vst.VisitSet(s) }
func (s *Set) Position() *report.Position { return s.Pos }
func (s *Set) Range() *report.Range       { return s.Rng }

func (u *UnaryExpr) Accept(vst Visitor)         { vst.VisitUnaryExpr(u) }
func (u *UnaryExpr) expr()                      {}
func (u *UnaryExpr) String() string             { return fmt.Sprintf("%v%v", u.Op, u.Operand) }
func (u *UnaryExpr) Position() *report.Position { return u.Pos }
func (u *UnaryExpr) Range() *report.Range       { return u.Rng }

func (d *Designator) Accept(vst Visitor) { vst.VisitDesignator(d) }
func (d *Designator) expr()              {}
func (d *Designator) String() string {
	s := d.QIdent.String()
	for _, sel := range d.Select {
		switch sel := sel.(type) {
		case *DotOp:
			s += sel.String()
		case *IndexOp:
			s += sel.String()
		case *TypeGuard:
			s += sel.String()
		case *PtrDeref:
			s += sel.String()
		default:
			panic(fmt.Sprintf("unknown selector type: %T", sel))
		}
	}

	return s
}
func (d *Designator) Position() *report.Position { return d.Pos }
func (d *Designator) Range() *report.Range       { return d.Rng }

func (b *BadExpr) Accept(Visitor)             {}
func (b *BadExpr) expr()                      {}
func (b *BadExpr) String() string             { return "<bad expr>" }
func (b *BadExpr) Position() *report.Position { return b.Pos }
func (b *BadExpr) Range() *report.Range       { return b.Rng }

// Selectors
// ---------------------

type Selector interface {
	sel()
	String() string
	Position() *report.Position
	Range() *report.Range
}

// DotOp
// ---------------------
type DotOp struct {
	Field string
	Pos   *report.Position
	Rng   *report.Range
}

func (d *DotOp) String() string             { return fmt.Sprintf(".%s", d.Field) }
func (d *DotOp) sel()                       {}
func (d *DotOp) Position() *report.Position { return d.Pos }
func (d *DotOp) Range() *report.Range       { return d.Rng }

// IndexOp
// ---------------------
type IndexOp struct {
	List []Expression
	Pos  *report.Position
	Rng  *report.Range
}

func (id *IndexOp) String() string {
	var list []string
	for _, expr := range id.List {
		list = append(list, expr.String())
	}

	return fmt.Sprintf("[%v]", strings.Join(list, ","))
}
func (id *IndexOp) sel()                       {}
func (id *IndexOp) Position() *report.Position { return id.Pos }
func (id *IndexOp) Range() *report.Range       { return id.Rng }

// TypeGuard
// ---------------------
type TypeGuard struct {
	Ty  Expression
	Pos *report.Position
	Rng *report.Range
}

func (t *TypeGuard) String() string             { return fmt.Sprintf("(%s)", t.Ty) }
func (t *TypeGuard) sel()                       {}
func (t *TypeGuard) Position() *report.Position { return t.Pos }
func (t *TypeGuard) Range() *report.Range       { return t.Rng }

// PtrDeref
// ---------------------
type PtrDeref struct {
	Pos *report.Position
	Rng *report.Range
}

func (deref *PtrDeref) sel()                       {}
func (deref *PtrDeref) String() string             { return "^" }
func (deref *PtrDeref) Position() *report.Position { return deref.Pos }
func (deref *PtrDeref) Range() *report.Range       { return deref.Rng }
