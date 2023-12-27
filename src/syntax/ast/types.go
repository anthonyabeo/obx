package ast

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type (
	BasicType struct {
		name string // e.g. integer, bool

		EType types.Type
	}

	ArrayType struct {
		Array    *token.Position
		LenList  *LenList
		ElemType Expression
		EType    types.Type
	}

	ProcType struct {
		Proc  *token.Position
		FP    *FormalParams
		EType types.Type
	}

	PointerType struct {
		Ptr   *token.Position
		Base  Expression
		EType types.Type
	}
)

func NewBasicType(name string) *BasicType {
	return &BasicType{name: name}
}

func (b *BasicType) Name() string         { return b.name }
func (b *BasicType) Pos() *token.Position { panic("not implemented") }
func (b *BasicType) End() *token.Position { panic("not implemented") }
func (b *BasicType) expr()                {}
func (b *BasicType) Type() types.Type     { return b.EType }
func (b *BasicType) String() string       { return b.name }
func (b *BasicType) Accept(vst Visitor)   { vst.VisitBasicType(b) }

func NewArray(pos *token.Position, lenList *LenList, elem Expression) *ArrayType {
	return &ArrayType{Array: pos, LenList: lenList, ElemType: elem}
}

func (a *ArrayType) expr()                {}
func (a *ArrayType) Pos() *token.Position { return a.Array }
func (a *ArrayType) End() *token.Position { return a.ElemType.End() }
func (a *ArrayType) String() string       { panic("not implemented") }
func (a *ArrayType) Accept(vst Visitor)   { vst.VisitArrayType(a) }
func (a *ArrayType) Type() types.Type     { return a.EType }

type LenList struct {
	Modifier token.Token
	List     []Expression
}

func (p *ProcType) expr()                {}
func (p *ProcType) Pos() *token.Position { panic("not implemented") }
func (p *ProcType) End() *token.Position { panic("not implemented") }
func (p *ProcType) String() string       { panic("not implemented") }
func (p *ProcType) Type() types.Type     { return p.EType }

func (p *PointerType) expr()                {}
func (p *PointerType) Pos() *token.Position { panic("not implemented") }
func (p *PointerType) End() *token.Position { panic("not implemented") }
func (p *PointerType) String() string       { panic("not implemented") }
func (p *PointerType) Type() types.Type     { return p.EType }
func (p *PointerType) Accept(vst Visitor)   { vst.VisitPointerType(p) }
