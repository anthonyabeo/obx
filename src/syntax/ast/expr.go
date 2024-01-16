package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// IdentProps is a set of flags denoting the properties of an identifier
type IdentProps int

// Properties of an identifier
const (
	IsExported IdentProps = 1 << iota
	IsReadOnly
	IsPredeclared
)

type (
	BasicLit struct {
		ValuePos *token.Position // literal position
		Kind     token.Token     // token.INT, token.REAL, token.HEXCHAR, or token.STRING
		Value    string          // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
		EType    types.Type
	}

	BinaryExpr struct {
		OpPos       *token.Position
		Left, Right Expression  // operands
		Op          token.Token // operator
		EType       types.Type
	}

	FuncCall struct {
		Dsg          Expression
		ActualParams []Expression
		EType        types.Type
	}

	Ident struct {
		NamePos *token.Position
		Name    string
		IProps  IdentProps
		EType   types.Type
	}

	QualifiedIdent struct {
		X     Expression
		Sel   *Ident
		EType types.Type
	}

	Set struct {
		Elem  []Expression
		EType types.Type
	}

	UnaryExpr struct {
		OpPos *token.Position
		Op    token.Token
		X     Expression
		EType types.Type
	}

	Designator struct {
		QIdentPos      *token.Position
		QualifiedIdent Expression
		Selector       Selector
		EType          types.Type
	}

	BadExpr struct {
		From *token.Position
		To   *token.Position
	}
)

func (b *BasicLit) expr()                {}
func (b *BasicLit) Pos() *token.Position { return b.ValuePos }
func (b *BasicLit) End() *token.Position { panic("not implemented") }
func (b *BasicLit) String() string       { return b.Value }
func (b *BasicLit) Accept(vst Visitor)   { vst.VisitBasicLit(b) }
func (b *BasicLit) Type() types.Type     { return b.EType }

func (b *BinaryExpr) expr()                {}
func (b *BinaryExpr) Pos() *token.Position { return b.OpPos }
func (b *BinaryExpr) End() *token.Position { return b.Right.End() }
func (b *BinaryExpr) String() string       { return fmt.Sprintf("%v %v %v", b.Left, b.Op, b.Right) }
func (b *BinaryExpr) Type() types.Type     { return b.EType }
func (b *BinaryExpr) Accept(vst Visitor)   { vst.VisitBinaryExpr(b) }

func (f *FuncCall) Pos() *token.Position { return f.Dsg.Pos() }
func (f *FuncCall) End() (pos *token.Position) {
	size := len(f.ActualParams)
	if size > 0 {
		pos = f.ActualParams[size-1].End()
		pos.Column += len(")")
	} else {
		pos = f.Dsg.End()
		pos.Column += len("()")
	}

	return
}
func (f *FuncCall) Type() types.Type   { return f.EType }
func (f *FuncCall) Accept(vst Visitor) { vst.VisitFuncCall(f) }
func (f *FuncCall) expr()              {}
func (f *FuncCall) String() string     { panic("not implemented") }

func (id *Ident) Pos() *token.Position { return id.NamePos }
func (id *Ident) End() *token.Position {
	return &token.Position{
		Filename: id.NamePos.Filename,
		Line:     id.NamePos.Line,
		Column:   id.NamePos.Column + len(id.Name),
	}
}
func (id *Ident) String() string     { return id.Name }
func (id *Ident) expr()              {}
func (id *Ident) Accept(vst Visitor) { vst.VisitIdentifier(id) }
func (id *Ident) Props() IdentProps  { return id.IProps }
func (id *Ident) Type() types.Type   { return id.EType }

func (q *QualifiedIdent) Pos() *token.Position { return q.X.Pos() }
func (q *QualifiedIdent) End() *token.Position { panic("not implemented") }
func (q *QualifiedIdent) Type() types.Type     { return q.EType }
func (q *QualifiedIdent) Accept(vst Visitor)   { vst.VisitQualifiedIdent(q) }
func (q *QualifiedIdent) expr()                {}
func (q *QualifiedIdent) String() string       { return fmt.Sprintf("%v.%v", q.X, q.Sel) }

func (s *Set) expr()                {}
func (s *Set) String() string       { panic("not implement") }
func (s *Set) Pos() *token.Position { panic("not implemented") }
func (s *Set) End() *token.Position { panic("not implemented") }
func (s *Set) Accept(vst Visitor)   { vst.VisitSet(s) }
func (s *Set) Type() types.Type     { return s.EType }

func (u *UnaryExpr) Pos() *token.Position { return u.OpPos }
func (u *UnaryExpr) End() *token.Position { return u.X.End() }
func (u *UnaryExpr) Type() types.Type     { return u.EType }
func (u *UnaryExpr) Accept(vst Visitor)   { vst.VisitUnaryExpr(u) }
func (u *UnaryExpr) expr()                {}
func (u *UnaryExpr) String() string       { return fmt.Sprintf("%v%v", u.Op, u.X) }

func (d *Designator) Pos() *token.Position { return d.QIdentPos }
func (d *Designator) End() (pos *token.Position) {
	if d.Selector != nil {
		pos = d.Selector.End()
	} else {
		pos = d.QualifiedIdent.End()
	}

	return
}
func (d *Designator) Type() types.Type   { return d.EType }
func (d *Designator) Accept(vst Visitor) { vst.VisitDesignator(d) }
func (d *Designator) expr()              {}
func (d *Designator) String() string {
	s := d.QualifiedIdent.String()
	if d.Selector != nil {
		switch d.Selector.(type) {
		case *DotOp:
		case *IndexOp:
		case *TypeGuard:
		}
	}

	return s
}

func (b *BadExpr) Pos() *token.Position { return b.From }
func (b *BadExpr) End() *token.Position { return b.To }
func (b *BadExpr) Accept(vst Visitor)   { panic("not implemented") }
func (b *BadExpr) Type() types.Type     { return nil }
func (b *BadExpr) expr()                {}
func (b *BadExpr) String() string       { panic("not implemented") }

// Selectors
// ---------------------

type Selector interface {
	Expression
	sel()
}

// DotOp
// ---------------------
type DotOp struct {
	Field *Ident
	EType types.Type
}

func (d *DotOp) Pos() *token.Position { return d.Field.Pos() }
func (d *DotOp) End() *token.Position { return d.Field.End() }
func (d *DotOp) String() string       { return fmt.Sprintf(".%s", d.Field) }
func (d *DotOp) Type() types.Type     { return d.EType }
func (d *DotOp) Accept(vst Visitor)   { vst.VisitDotOp(d) }
func (d *DotOp) expr()                {}
func (d *DotOp) sel()                 {}

// IndexOp
// ---------------------
type IndexOp struct {
	List  []Expression
	EType types.Type
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
func (id *IndexOp) Type() types.Type   { return id.EType }
func (id *IndexOp) Accept(vst Visitor) { vst.VisitIndexOp(id) }
func (id *IndexOp) expr()              {}
func (id *IndexOp) sel()               {}

// TypeGuard
// ---------------------
type TypeGuard struct {
	Typ   Expression
	EType types.Type
}

func (t *TypeGuard) Pos() *token.Position { panic("not implement") }
func (t *TypeGuard) End() *token.Position { panic("not implement") }
func (t *TypeGuard) String() string       { panic("not implement") }
func (t *TypeGuard) Type() types.Type     { return t.EType }
func (t *TypeGuard) Accept(vst Visitor)   { vst.VisitTypeGuard(t) }
func (t *TypeGuard) expr()                {}
func (t *TypeGuard) sel()                 {}

// PointerDeref
// ---------------------
type PointerDeref struct {
	EType types.Type
}

func (deref *PointerDeref) Pos() *token.Position { panic("not implement") }
func (deref *PointerDeref) End() *token.Position { panic("not implement") }
func (deref *PointerDeref) String() string       { panic("not implement") }
func (deref *PointerDeref) Type() types.Type     { return deref.EType }
func (deref *PointerDeref) Accept(vst Visitor)   { vst.VisitPointerDeref(deref) }
func (deref *PointerDeref) expr()                {}
func (deref *PointerDeref) sel()                 {}