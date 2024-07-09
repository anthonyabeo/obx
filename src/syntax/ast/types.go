package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

type (
	NamedType struct {
		pos  *token.Position
		Name Expression

		EType types.Type
	}

	BasicType struct {
		pos  *token.Position
		name string // e.g. integer, bool

		EType types.Type
		IRTy  tacil.Type
	}

	ArrayType struct {
		Array    *token.Position
		LenList  *LenList
		ElemType Type

		EType types.Type
		IRTy  tacil.Type
	}

	ProcType struct {
		Proc *token.Position
		FP   *FormalParams

		EType types.Type
		IRTy  tacil.Type
	}

	PointerType struct {
		Ptr  *token.Position
		Base Type

		EType types.Type
		IRTy  tacil.Type
	}

	RecordType struct {
		Record   *token.Position
		BaseType Type
		Fields   []*FieldList

		EType types.Type
		IRTy  tacil.Type
	}

	FieldList struct {
		IdList []*Ident
		Type   Type
	}

	EnumType struct {
		Enum     *token.Position
		Variants []*Ident

		EType types.Type
		IRTy  tacil.Type
	}

	BadType struct {
		From *token.Position
		To   *token.Position
	}
)

func NewNamedType(pos *token.Position, name Expression) *NamedType {
	return &NamedType{pos: pos, Name: name}
}

func (n *NamedType) String() string       { return n.Name.String() }
func (n *NamedType) Pos() *token.Position { return n.pos }
func (n *NamedType) End() *token.Position { panic("implement me") }
func (n *NamedType) Accept(vst Visitor)   { vst.VisitNamedType(n) }
func (n *NamedType) Type() types.Type     { return n.EType }
func (n *NamedType) IRType() tacil.Type   { panic("implement me") }

func NewBasicType(pos *token.Position, name string) *BasicType {
	return &BasicType{name: name, pos: pos}
}

func (b *BasicType) Name() string         { return b.name }
func (b *BasicType) Pos() *token.Position { return b.pos }
func (b *BasicType) End() *token.Position { panic("not implemented") }
func (b *BasicType) Type() types.Type     { return b.EType }
func (b *BasicType) String() string       { return b.name }
func (b *BasicType) Accept(vst Visitor)   { vst.VisitBasicType(b) }
func (b *BasicType) IRType() tacil.Type   { return b.IRTy }

func NewArray(pos *token.Position, lenList *LenList, elem Type) *ArrayType {
	return &ArrayType{Array: pos, LenList: lenList, ElemType: elem}
}

func (a *ArrayType) Pos() *token.Position { return a.Array }
func (a *ArrayType) End() *token.Position { return a.ElemType.End() }
func (a *ArrayType) String() string {
	var ll []string
	if a.LenList != nil {
		for _, l := range a.LenList.List {
			ll = append(ll, l.String())
		}
	}

	return fmt.Sprintf("[%s]%s", strings.Join(ll, ", "), a.ElemType)
}
func (a *ArrayType) Accept(vst Visitor) { vst.VisitArrayType(a) }
func (a *ArrayType) Type() types.Type   { return a.EType }
func (a *ArrayType) IRType() tacil.Type { return a.IRTy }

type LenList struct {
	Modifier token.Token
	List     []Expression
}

func (p *ProcType) Pos() *token.Position { return p.Proc }
func (p *ProcType) End() *token.Position { panic("not implemented") }
func (p *ProcType) String() string       { panic("not implemented") }
func (p *ProcType) Type() types.Type     { return p.EType }
func (p *ProcType) Accept(vst Visitor)   { vst.VisitProcType(p) }
func (p *ProcType) IRType() tacil.Type   { return p.IRTy }

func (p *PointerType) Pos() *token.Position { return p.Ptr }
func (p *PointerType) End() *token.Position { panic("not implemented") }
func (p *PointerType) String() string       { return fmt.Sprintf("^%s", p.Base) }
func (p *PointerType) Type() types.Type     { return p.EType }
func (p *PointerType) Accept(vst Visitor)   { vst.VisitPointerType(p) }
func (p *PointerType) IRType() tacil.Type   { return p.IRTy }

func (r *RecordType) Pos() *token.Position { return r.Record }
func (r *RecordType) End() *token.Position { panic("not implemented") }
func (r *RecordType) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString("record")

	if r.BaseType != nil {
		t, _ := r.BaseType.(*NamedType)
		buf.WriteString(fmt.Sprintf("(%s)", t.Name))
	}

	var fields []string
	for _, field := range r.Fields {
		for i := 0; i < len(field.IdList); i++ {
			fields = append(fields, field.Type.String())
		}
	}
	buf.WriteString(fmt.Sprintf("{%s}", strings.Join(fields, "; ")))

	return buf.String()
}
func (r *RecordType) Type() types.Type   { return r.EType }
func (r *RecordType) Accept(vst Visitor) { vst.VisitRecordType(r) }
func (r *RecordType) IRType() tacil.Type { return r.IRTy }

func (e *EnumType) Pos() *token.Position { return e.Enum }
func (e *EnumType) End() *token.Position { panic("not implemented") }
func (e *EnumType) String() string       { panic("not implemented") }
func (e *EnumType) Type() types.Type     { return e.EType }
func (e *EnumType) Accept(vst Visitor)   { vst.VisitEnumType(e) }
func (e *EnumType) IRType() tacil.Type   { return e.IRTy }

func (b *BadType) String() string       { panic("implement me") }
func (b *BadType) Pos() *token.Position { return b.From }
func (b *BadType) End() *token.Position { return b.To }
func (b *BadType) Accept(vst Visitor)   { panic("implement me") }
func (b *BadType) expr()                {}
func (b *BadType) Type() types.Type     { return nil }
func (b *BadType) IRType() tacil.Type   { panic("implement me") }
