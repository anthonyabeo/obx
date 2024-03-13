package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

// IdentProps is a set of flags denoting the properties of an identifier
type IdentProps int

// Properties of an identifier
const (
	Exported IdentProps = 1 << iota
	ReadOnly
	Predeclared
)

type (
	BasicLit struct {
		ValuePos *token.Position // literal position
		Kind     token.Token     // token.INT, token.REAL, token.HEXCHAR, or token.STRING
		Val      string          // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
		EType    types.Type
		IRValue  ir.Value
	}

	BinaryExpr struct {
		OpPos       *token.Position
		Left, Right Expression  // operands
		Op          token.Token // operator
		EType       types.Type
		IRValue     ir.Value
	}

	FuncCall struct {
		Callee       Expression
		ActualParams []Expression
		EType        types.Type
		IRValue      ir.Value
	}

	Ident struct {
		NamePos *token.Position
		Name    string
		IProps  IdentProps

		EType   types.Type
		IRValue ir.Value
	}

	QualifiedIdent struct {
		Module Expression
		Sel    *Ident

		EType   types.Type
		IRValue ir.Value
	}

	ExprRange struct {
		Beg Expression
		Ed  Expression

		EType   types.Type
		IRValue ir.Value
	}

	Set struct {
		Elem    []Expression
		EType   types.Type
		IRValue ir.Value
	}

	UnaryExpr struct {
		OpPos   *token.Position
		Op      token.Token
		X       Expression
		EType   types.Type
		IRValue ir.Value
	}

	Designator struct {
		QPos           *token.Position
		QualifiedIdent Expression
		Selector       Selector
		EType          types.Type
		IRValue        ir.Value
	}

	BadExpr struct {
		From *token.Position
		To   *token.Position
	}
)

func (e *ExprRange) String() string       { return fmt.Sprintf("%s..%s", e.Beg, e.Ed) }
func (e *ExprRange) Pos() *token.Position { return e.Beg.Pos() }
func (e *ExprRange) End() *token.Position { return e.Ed.Pos() }
func (e *ExprRange) Accept(vst Visitor)   { vst.VisitExprRange(e) }
func (e *ExprRange) expr()                {}
func (e *ExprRange) Type() types.Type     { return e.EType }
func (e *ExprRange) Value() ir.Value      { return e.IRValue }

func (b *BasicLit) expr()                {}
func (b *BasicLit) Pos() *token.Position { return b.ValuePos }
func (b *BasicLit) End() *token.Position { panic("not implemented") }
func (b *BasicLit) String() string       { return b.Val }
func (b *BasicLit) Accept(vst Visitor)   { vst.VisitBasicLit(b) }
func (b *BasicLit) Type() types.Type     { return b.EType }
func (b *BasicLit) Value() ir.Value      { return b.IRValue }

func (b *BinaryExpr) expr()                {}
func (b *BinaryExpr) Pos() *token.Position { return b.OpPos }
func (b *BinaryExpr) End() *token.Position { return b.Right.End() }
func (b *BinaryExpr) String() string       { return fmt.Sprintf("%v %v %v", b.Left, b.Op, b.Right) }
func (b *BinaryExpr) Type() types.Type     { return b.EType }
func (b *BinaryExpr) Accept(vst Visitor)   { vst.VisitBinaryExpr(b) }
func (b *BinaryExpr) Value() ir.Value      { return b.IRValue }

func (f *FuncCall) Pos() *token.Position { return f.Callee.Pos() }
func (f *FuncCall) End() (pos *token.Position) {
	size := len(f.ActualParams)
	if size > 0 {
		pos = f.ActualParams[size-1].End()
		pos.Column += len(")")
	} else {
		pos = f.Callee.End()
		pos.Column += len("()")
	}

	return
}
func (f *FuncCall) Type() types.Type   { return f.EType }
func (f *FuncCall) Accept(vst Visitor) { vst.VisitFuncCall(f) }
func (f *FuncCall) expr()              {}
func (f *FuncCall) String() string {
	var args []string
	for _, arg := range f.ActualParams {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", f.Callee, strings.Join(args, ", "))
}
func (f *FuncCall) Value() ir.Value { return f.IRValue }

func (id *Ident) Pos() *token.Position { return id.NamePos }
func (id *Ident) End() *token.Position {
	return &token.Position{
		Filename: id.NamePos.Filename,
		Line:     id.NamePos.Line,
		Column:   id.NamePos.Column + len(id.Name),
	}
}
func (id *Ident) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString(id.Name)

	if id.IProps == Exported {
		buf.WriteString("*")
	}

	if id.IProps == Exported|ReadOnly {
		buf.WriteString("-")
	}

	return buf.String()
}
func (id *Ident) expr()              {}
func (id *Ident) Accept(vst Visitor) { vst.VisitIdentifier(id) }
func (id *Ident) Props() IdentProps  { return id.IProps }
func (id *Ident) Type() types.Type   { return id.EType }
func (id *Ident) Value() ir.Value    { return id.IRValue }

func (q *QualifiedIdent) Pos() *token.Position { return q.Module.Pos() }
func (q *QualifiedIdent) End() *token.Position { panic("not implemented") }
func (q *QualifiedIdent) Type() types.Type     { return q.EType }
func (q *QualifiedIdent) Accept(vst Visitor)   { vst.VisitQualifiedIdent(q) }
func (q *QualifiedIdent) expr()                {}
func (q *QualifiedIdent) String() string       { return fmt.Sprintf("%v.%v", q.Module, q.Sel) }
func (q *QualifiedIdent) Value() ir.Value      { return q.IRValue }

func (s *Set) expr() {}
func (s *Set) String() string {
	var elems []string
	for _, elem := range s.Elem {
		elems = append(elems, elem.String())
	}

	return fmt.Sprintf("{%s}", strings.Join(elems, ", "))
}
func (s *Set) Pos() *token.Position { panic("not implemented") }
func (s *Set) End() *token.Position { panic("not implemented") }
func (s *Set) Accept(vst Visitor)   { vst.VisitSet(s) }
func (s *Set) Type() types.Type     { return s.EType }
func (s *Set) Value() ir.Value      { return s.IRValue }

func (u *UnaryExpr) Pos() *token.Position { return u.OpPos }
func (u *UnaryExpr) End() *token.Position { return u.X.End() }
func (u *UnaryExpr) Type() types.Type     { return u.EType }
func (u *UnaryExpr) Accept(vst Visitor)   { vst.VisitUnaryExpr(u) }
func (u *UnaryExpr) expr()                {}
func (u *UnaryExpr) String() string       { return fmt.Sprintf("%v%v", u.Op, u.X) }
func (u *UnaryExpr) Value() ir.Value      { return u.IRValue }

func (d *Designator) Pos() *token.Position       { return d.QPos }
func (d *Designator) End() (pos *token.Position) { panic("not implemented") }
func (d *Designator) Type() types.Type           { return d.EType }
func (d *Designator) Accept(vst Visitor)         { vst.VisitDesignator(d) }
func (d *Designator) expr()                      {}
func (d *Designator) String() string {
	s := d.QualifiedIdent.String()
	if d.Selector != nil {
		s += d.Selector.String()
	}

	return s
}
func (d *Designator) Value() ir.Value { return d.IRValue }

func (b *BadExpr) Pos() *token.Position { return b.From }
func (b *BadExpr) End() *token.Position { return b.To }
func (b *BadExpr) Accept(Visitor)       { panic("not implemented") }
func (b *BadExpr) Type() types.Type     { return nil }
func (b *BadExpr) expr()                {}
func (b *BadExpr) String() string       { panic("not implemented") }
func (b *BadExpr) Value() ir.Value      { return nil }

// Selectors
// ---------------------

type Selector interface {
	sel()
	String() string
}

// DotOp
// ---------------------
type DotOp struct {
	Field *Ident
}

func (d *DotOp) Pos() *token.Position { return d.Field.Pos() }
func (d *DotOp) End() *token.Position { return d.Field.End() }
func (d *DotOp) String() string       { return fmt.Sprintf(".%s", d.Field) }
func (d *DotOp) sel()                 {}

// IndexOp
// ---------------------
type IndexOp struct {
	List []Expression
}

func (id *IndexOp) Pos() *token.Position { panic("not implement") }
func (id *IndexOp) End() *token.Position { panic("not implement") }
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

func (t *TypeGuard) Pos() *token.Position { panic("not implement") }
func (t *TypeGuard) End() *token.Position { panic("not implement") }
func (t *TypeGuard) String() string       { return fmt.Sprintf("(%s)", t.Ty) }
func (t *TypeGuard) sel()                 {}

// PtrDref
// ---------------------
type PtrDref struct{}

func (deref *PtrDref) Pos() *token.Position { panic("not implement") }
func (deref *PtrDref) End() *token.Position { panic("not implement") }
func (deref *PtrDref) String() string       { panic("not implement") }
func (deref *PtrDref) sel()                 {}
