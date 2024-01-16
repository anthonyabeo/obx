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

	RecordType struct {
		Record   *token.Position
		BaseType Expression
		Fields   []*FieldList
		EType    types.Type
	}

	FieldList struct {
		IdList []*Ident
		Type   Expression
	}

	EnumType struct {
		Enum   *token.Position
		Consts []*Ident
		EType  types.Type
	}

	BadType struct {
		From *token.Position
		To   *token.Position
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
func (b *BasicType) Width() int {
	switch b.name {
	case "integer":
		return 4
	default:
		return 0
	}
}

func NewArray(pos *token.Position, lenList *LenList, elem Expression) *ArrayType {
	return &ArrayType{Array: pos, LenList: lenList, ElemType: elem}
}

func (a *ArrayType) expr()                {}
func (a *ArrayType) Pos() *token.Position { return a.Array }
func (a *ArrayType) End() *token.Position { return a.ElemType.End() }
func (a *ArrayType) String() string       { panic("not implemented") }
func (a *ArrayType) Accept(vst Visitor)   { vst.VisitArrayType(a) }
func (a *ArrayType) Type() types.Type     { return a.EType }
func (a *ArrayType) Width() int           { panic("implement me") }

type LenList struct {
	Modifier token.Token
	List     []Expression
}

func (p *ProcType) expr()                {}
func (p *ProcType) Pos() *token.Position { panic("not implemented") }
func (p *ProcType) End() *token.Position { panic("not implemented") }
func (p *ProcType) String() string       { panic("not implemented") }
func (p *ProcType) Type() types.Type     { return p.EType }
func (p *ProcType) Accept(vst Visitor)   { vst.VisitProcType(p) }
func (p *ProcType) Width() int           { panic("implement me") }

func (p *PointerType) expr()                {}
func (p *PointerType) Pos() *token.Position { panic("not implemented") }
func (p *PointerType) End() *token.Position { panic("not implemented") }
func (p *PointerType) String() string       { panic("not implemented") }
func (p *PointerType) Type() types.Type     { return p.EType }
func (p *PointerType) Accept(vst Visitor)   { vst.VisitPointerType(p) }
func (p *PointerType) Width() int           { panic("implement me") }

func (r *RecordType) expr()                {}
func (r *RecordType) Pos() *token.Position { panic("not implemented") }
func (r *RecordType) End() *token.Position { panic("not implemented") }
func (r *RecordType) String() string       { panic("not implemented") }
func (r *RecordType) Type() types.Type     { return r.EType }
func (r *RecordType) Accept(vst Visitor)   { vst.VisitRecordType(r) }
func (r *RecordType) Width() int           { panic("implement me") }

func (e *EnumType) expr()                {}
func (e *EnumType) Pos() *token.Position { return e.Enum }
func (e *EnumType) End() *token.Position { panic("not implemented") }
func (e *EnumType) String() string       { panic("not implemented") }
func (e *EnumType) Type() types.Type     { return e.EType }
func (e *EnumType) Accept(vst Visitor)   { vst.VisitEnumType(e) }
func (e *EnumType) Width() int           { panic("implement me") }

func (b *BadType) String() string       { panic("implement me") }
func (b *BadType) Pos() *token.Position { return b.From }
func (b *BadType) End() *token.Position { return b.To }
func (b *BadType) Accept(vst Visitor)   { panic("implement me") }
func (b *BadType) expr()                {}
func (b *BadType) Type() types.Type     { return nil }
func (b *BadType) Width() int           { panic("implement me") }