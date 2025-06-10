package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
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
		Name        string
		Props       IdentProps
		Symbol      Symbol
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	BasicLit struct {
		Kind        token.Kind // token.INT, token.REAL, token.HEXCHAR, or token.STRING
		Val         string     // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	BinaryExpr struct {
		Left, Right Expression // operands
		Op          token.Kind // operator
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	FunctionCall struct {
		Callee       *Designator
		ActualParams []Expression
		SemaType     types.Type
		StartOffset  int
		EndOffset    int
	}

	QualifiedIdent struct {
		Prefix      string
		Name        string
		Symbol      Symbol
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	ExprRange struct {
		Low         Expression
		High        Expression
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	Set struct {
		Elem        []Expression
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	UnaryExpr struct {
		Op          token.Kind
		Operand     Expression
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	Designator struct {
		QIdent      *QualifiedIdent
		Select      []Selector
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	Nil struct {
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	BadExpr struct {
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}
)

func (*Nil) expr()                    {}
func (*Nil) String() string           { return "nil" }
func (n *Nil) Accept(vst Visitor) any { return vst.VisitNil(n) }
func (n *Nil) Pos() int               { return n.StartOffset }
func (n *Nil) End() int               { return n.EndOffset }
func (n *Nil) Children() []Node       { return []Node{} }
func (n *Nil) Type() types.Type       { return n.SemaType }

func (id *IdentifierDef) expr() {}
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
func (id *IdentifierDef) Children() []Node       { return []Node{} }
func (id *IdentifierDef) Type() types.Type       { return id.SemaType }

func (e *ExprRange) String() string         { return fmt.Sprintf("%s..%s", e.Low, e.High) }
func (e *ExprRange) Accept(vst Visitor) any { return vst.VisitExprRange(e) }
func (e *ExprRange) expr()                  {}
func (e *ExprRange) Pos() int               { return e.StartOffset }
func (e *ExprRange) End() int               { return e.EndOffset }
func (e *ExprRange) Children() []Node       { return []Node{e.Low, e.High} }
func (e *ExprRange) Type() types.Type       { return e.SemaType }

func (b *BasicLit) expr()                  {}
func (b *BasicLit) String() string         { return b.Val }
func (b *BasicLit) Accept(vst Visitor) any { return vst.VisitBasicLit(b) }
func (b *BasicLit) Pos() int               { return b.StartOffset }
func (b *BasicLit) End() int               { return b.EndOffset }
func (b *BasicLit) Children() []Node       { return []Node{} }
func (b *BasicLit) Type() types.Type       { return b.SemaType }

func (b *BinaryExpr) expr()                  {}
func (b *BinaryExpr) String() string         { return fmt.Sprintf("%v %v %v", b.Left, b.Op, b.Right) }
func (b *BinaryExpr) Accept(vst Visitor) any { return vst.VisitBinaryExpr(b) }
func (b *BinaryExpr) Pos() int               { return b.StartOffset }
func (b *BinaryExpr) End() int               { return b.EndOffset }
func (b *BinaryExpr) Children() []Node       { return []Node{b.Left, b.Right} }
func (b *BinaryExpr) Type() types.Type       { return b.SemaType }

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
func (f *FunctionCall) Children() []Node {
	children := []Node{f.Callee}
	for _, param := range f.ActualParams {
		children = append(children, param)
	}

	return children
}
func (f *FunctionCall) Type() types.Type { return f.SemaType }

func (q *QualifiedIdent) Accept(vst Visitor) any { return vst.VisitQualifiedIdent(q) }
func (q *QualifiedIdent) expr()                  {}
func (q *QualifiedIdent) String() string {
	if q.Prefix == "" {
		return q.Name
	}
	return fmt.Sprintf("%v.%v", q.Prefix, q.Name)
}
func (q *QualifiedIdent) Pos() int         { return q.StartOffset }
func (q *QualifiedIdent) End() int         { return q.EndOffset }
func (q *QualifiedIdent) Children() []Node { return []Node{} }
func (q *QualifiedIdent) Type() types.Type { return q.SemaType }

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
func (s *Set) Children() []Node {
	children := make([]Node, len(s.Elem))
	for _, expression := range s.Elem {
		children = append(children, expression)
	}

	return children
}
func (s *Set) Type() types.Type { return s.SemaType }

func (u *UnaryExpr) Accept(vst Visitor) any { return vst.VisitUnaryExpr(u) }
func (u *UnaryExpr) expr()                  {}
func (u *UnaryExpr) String() string         { return fmt.Sprintf("%v%v", u.Op, u.Operand) }
func (u *UnaryExpr) Pos() int               { return u.StartOffset }
func (u *UnaryExpr) End() int               { return u.EndOffset }
func (u *UnaryExpr) Children() []Node       { return []Node{u.Operand} }
func (u *UnaryExpr) Type() types.Type       { return u.SemaType }

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
func (d *Designator) Children() []Node {
	children := []Node{d.QIdent}
	if len(d.Select) > 0 {
		for _, selector := range d.Select {
			children = append(children, selector)

		}
	}

	return children
}
func (d *Designator) Type() types.Type { return d.SemaType }

func (b *BadExpr) Accept(vst Visitor) any { return vst.VisitBadExpr(b) }
func (b *BadExpr) expr()                  {}
func (b *BadExpr) String() string         { return "<bad expr>" }
func (b *BadExpr) Pos() int               { return b.StartOffset }
func (b *BadExpr) End() int               { return b.EndOffset }
func (b *BadExpr) Children() []Node       { return []Node{} }
func (b *BadExpr) Type() types.Type       { return b.SemaType }

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
	Symbol      Symbol
	StartOffset int
	EndOffset   int
}

func (d *DotOp) String() string         { return fmt.Sprintf(".%s", d.Field) }
func (d *DotOp) sel()                   {}
func (d *DotOp) Pos() int               { return d.StartOffset }
func (d *DotOp) End() int               { return d.EndOffset }
func (d *DotOp) Accept(vst Visitor) any { return vst.VisitDotOp(d) }
func (d *DotOp) Children() []Node       { return []Node{} }

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
func (id *IndexOp) Children() []Node {
	children := make([]Node, len(id.List))
	for _, expression := range id.List {
		children = append(children, expression)
	}

	return children
}

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
func (t *TypeGuard) Children() []Node       { return []Node{t.Ty} }

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
func (deref *PtrDeref) Children() []Node       { return []Node{} }
