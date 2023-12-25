package ast

import (
	"fmt"
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
