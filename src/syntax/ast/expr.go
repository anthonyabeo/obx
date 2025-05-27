package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

// IdentProps is a set of flags denoting the properties of an identifier
type IdentProps int

func (p IdentProps) String() string {
	switch p {
	case Exported:
		return "Exported"
	case Unexported:
		return "Unexported"
	case ReadOnly:
		return "ReadOnly"
	case Predeclared:
		return "Predeclared"
	case ExportedReadOnly:
		return "Exported and Read Only"
	}
	return "Unknown"
}

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

		StartOffset int
		EndOffset   int
	}

	BasicLit struct {
		Kind token.Kind // token.INT, token.REAL, token.HEXCHAR, or token.STRING
		Val  string     // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`

		StartOffset int
		EndOffset   int
	}

	BinaryExpr struct {
		Left, Right Expression // operands
		Op          token.Kind // operator

		StartOffset int
		EndOffset   int
	}

	FunctionCall struct {
		Callee       *Designator
		ActualParams []Expression

		StartOffset int
		EndOffset   int
	}

	QualifiedIdent struct {
		Prefix string
		Name   string

		StartOffset int
		EndOffset   int
	}

	ExprRange struct {
		Low  Expression
		High Expression

		StartOffset int
		EndOffset   int
	}

	Set struct {
		Elem        []Expression
		StartOffset int
		EndOffset   int
	}

	UnaryExpr struct {
		Op      token.Kind
		Operand Expression

		StartOffset int
		EndOffset   int
	}

	Designator struct {
		QIdent *QualifiedIdent
		Select []Selector

		StartOffset int
		EndOffset   int
	}

	Nil struct {
		StartOffset int
		EndOffset   int
	}

	BadExpr struct {
		StartOffset int
		EndOffset   int
	}
)

func (*Nil) expr()                    {}
func (*Nil) String() string           { return "nil" }
func (n *Nil) Accept(vst Visitor) any { return vst.VisitNil(n) }
func (n *Nil) Pos() int               { return n.StartOffset }
func (n *Nil) End() int               { return n.EndOffset }

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
func (id *IdentifierDef) Pos() int               { return id.StartOffset }
func (id *IdentifierDef) End() int               { return id.EndOffset }
func (id *IdentifierDef) Accept(vst Visitor) any { return vst.VisitIdentifierDef(id) }

func (e *ExprRange) String() string         { return fmt.Sprintf("%s..%s", e.Low, e.High) }
func (e *ExprRange) Accept(vst Visitor) any { return vst.VisitExprRange(e) }
func (e *ExprRange) expr()                  {}
func (e *ExprRange) Pos() int               { return e.StartOffset }
func (e *ExprRange) End() int               { return e.EndOffset }

func (b *BasicLit) expr()                  {}
func (b *BasicLit) String() string         { return b.Val }
func (b *BasicLit) Accept(vst Visitor) any { return vst.VisitBasicLit(b) }
func (b *BasicLit) Pos() int               { return b.StartOffset }
func (b *BasicLit) End() int               { return b.EndOffset }

func (b *BinaryExpr) expr()                  {}
func (b *BinaryExpr) String() string         { return fmt.Sprintf("%v %v %v", b.Left, b.Op, b.Right) }
func (b *BinaryExpr) Accept(vst Visitor) any { return vst.VisitBinaryExpr(b) }
func (b *BinaryExpr) Pos() int               { return b.StartOffset }
func (b *BinaryExpr) End() int               { return b.EndOffset }

func (f *FunctionCall) Accept(vst Visitor) any { return vst.VisitFunctionCall(f) }
func (f *FunctionCall) expr()                  {}
func (f *FunctionCall) String() string {
	var args []string
	for _, arg := range f.ActualParams {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", f.Callee, strings.Join(args, ", "))
}
func (f *FunctionCall) Pos() int { return f.StartOffset }
func (f *FunctionCall) End() int { return f.EndOffset }

func (q *QualifiedIdent) Accept(vst Visitor) any { return vst.VisitQualifiedIdent(q) }
func (q *QualifiedIdent) expr()                  {}
func (q *QualifiedIdent) String() string {
	if q.Prefix == "" {
		return q.Name
	}
	return fmt.Sprintf("%v.%v", q.Prefix, q.Name)
}
func (q *QualifiedIdent) Pos() int { return q.StartOffset }
func (q *QualifiedIdent) End() int { return q.EndOffset }

func (s *Set) expr() {}
func (s *Set) String() string {
	var elems []string
	for _, elem := range s.Elem {
		elems = append(elems, elem.String())
	}

	return fmt.Sprintf("{%s}", strings.Join(elems, ", "))
}
func (s *Set) Accept(vst Visitor) any { return vst.VisitSet(s) }
func (s *Set) Pos() int               { return s.StartOffset }
func (s *Set) End() int               { return s.EndOffset }

func (u *UnaryExpr) Accept(vst Visitor) any { return vst.VisitUnaryExpr(u) }
func (u *UnaryExpr) expr()                  {}
func (u *UnaryExpr) String() string         { return fmt.Sprintf("%v%v", u.Op, u.Operand) }
func (u *UnaryExpr) Pos() int               { return u.StartOffset }
func (u *UnaryExpr) End() int               { return u.EndOffset }

func (d *Designator) Accept(vst Visitor) any { return vst.VisitDesignator(d) }
func (d *Designator) expr()                  {}
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
func (d *Designator) Pos() int { return d.StartOffset }
func (d *Designator) End() int { return d.EndOffset }

func (b *BadExpr) Accept(vst Visitor) any { return vst.VisitBadExpr(b) }
func (b *BadExpr) expr()                  {}
func (b *BadExpr) String() string         { return "<bad expr>" }
func (b *BadExpr) Pos() int               { return b.StartOffset }
func (b *BadExpr) End() int               { return b.EndOffset }

// Selectors
// ---------------------

type Selector interface {
	Node
	sel()
}

// DotOp
// ---------------------
type DotOp struct {
	Field       string
	StartOffset int
	EndOffset   int
}

func (d *DotOp) String() string         { return fmt.Sprintf(".%s", d.Field) }
func (d *DotOp) sel()                   {}
func (d *DotOp) Pos() int               { return d.StartOffset }
func (d *DotOp) End() int               { return d.EndOffset }
func (d *DotOp) Accept(vst Visitor) any { return vst.VisitDotOp(d) }

// IndexOp
// ---------------------
type IndexOp struct {
	List        []Expression
	StartOffset int
	EndOffset   int
}

func (id *IndexOp) String() string {
	var list []string
	for _, expr := range id.List {
		list = append(list, expr.String())
	}

	return fmt.Sprintf("[%v]", strings.Join(list, ","))
}
func (id *IndexOp) sel()                   {}
func (id *IndexOp) Pos() int               { return id.StartOffset }
func (id *IndexOp) End() int               { return id.EndOffset }
func (id *IndexOp) Accept(vst Visitor) any { return vst.VisitIndexOp(id) }

// TypeGuard
// ---------------------
type TypeGuard struct {
	Ty          Expression
	StartOffset int
	EndOffset   int
}

func (t *TypeGuard) String() string         { return fmt.Sprintf("(%s)", t.Ty) }
func (t *TypeGuard) sel()                   {}
func (t *TypeGuard) Pos() int               { return t.StartOffset }
func (t *TypeGuard) End() int               { return t.EndOffset }
func (t *TypeGuard) Accept(vst Visitor) any { return vst.VisitTypeGuard(t) }

// PtrDeref
// ---------------------
type PtrDeref struct {
	StartOffset int
	EndOffset   int
}

func (deref *PtrDeref) sel()                   {}
func (deref *PtrDeref) String() string         { return "^" }
func (deref *PtrDeref) Pos() int               { return deref.StartOffset }
func (deref *PtrDeref) End() int               { return deref.EndOffset }
func (deref *PtrDeref) Accept(vst Visitor) any { return vst.VisitPtrDeref(deref) }
