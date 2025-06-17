package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type (
	NamedType struct {
		Name   *QualifiedIdent
		Symbol Symbol

		StartOffset int
		EndOffset   int
	}

	BasicType struct {
		Kind token.Kind // e.g. integer, bool

		StartOffset int
		EndOffset   int
	}

	ArrayType struct {
		LenList  *LenList
		ElemType Type

		StartOffset int
		EndOffset   int
	}

	ProcedureType struct {
		FP *FormalParams

		StartOffset int
		EndOffset   int
	}

	PointerType struct {
		Base Type

		StartOffset int
		EndOffset   int
	}

	RecordType struct {
		Base   Type
		Fields []*FieldList
		Env    *RecordEnv

		StartOffset int
		EndOffset   int
	}

	FieldList struct {
		List []*IdentifierDef
		Type Type
		Env  *RecordEnv

		StartOffset int
		EndOffset   int
	}

	EnumType struct {
		Variants []string

		StartOffset int
		EndOffset   int
	}

	BadType struct {
		StartOffset int
		EndOffset   int
	}
)

func NewNamedType(name *QualifiedIdent, pos int, rng int) *NamedType {
	return &NamedType{Name: name, StartOffset: pos, EndOffset: rng}
}

func (n *NamedType) String() string         { return n.Name.String() }
func (n *NamedType) Accept(vst Visitor) any { return vst.VisitNamedType(n) }
func (n *NamedType) typ()                   {}
func (n *NamedType) Pos() int               { return n.StartOffset }
func (n *NamedType) End() int               { return n.EndOffset }
func (n *NamedType) Children() []Node       { return []Node{n.Name} }

func NewBasicType(kind token.Kind, pos int, rng int) *BasicType {
	return &BasicType{Kind: kind, StartOffset: pos, EndOffset: rng}
}

func (b *BasicType) Name() string           { return b.Kind.String() }
func (b *BasicType) String() string         { return b.Kind.String() }
func (b *BasicType) Accept(vst Visitor) any { return vst.VisitBasicType(b) }
func (b *BasicType) typ()                   {}
func (b *BasicType) Pos() int               { return b.StartOffset }
func (b *BasicType) End() int               { return b.EndOffset }
func (b *BasicType) Children() []Node       { return []Node{} }

func NewArray(lenList *LenList, elem Type, pos int, rng int) *ArrayType {
	return &ArrayType{LenList: lenList, ElemType: elem, StartOffset: pos, EndOffset: rng}
}

func (a *ArrayType) String() string {
	var ll []string
	if a.LenList != nil {
		for _, l := range a.LenList.List {
			ll = append(ll, l.String())
		}
	}

	return fmt.Sprintf("[%s]%s", strings.Join(ll, ", "), a.ElemType)
}
func (a *ArrayType) Accept(vst Visitor) any { return vst.VisitArrayType(a) }
func (a *ArrayType) typ()                   {}
func (a *ArrayType) Pos() int               { return a.StartOffset }
func (a *ArrayType) End() int               { return a.EndOffset }
func (a *ArrayType) Children() []Node       { return []Node{a.LenList, a.ElemType} }

type LenList struct {
	Modifier token.Kind
	List     []Expression

	StartOffset int
	EndOffset   int
}

func (l *LenList) Accept(vst Visitor) any { return vst.VisitLenList(l) }
func (l *LenList) String() string {
	var lengths []string
	for _, expr := range l.List {
		lengths = append(lengths, expr.String())
	}

	if l.Modifier != token.ILLEGAL {
		return fmt.Sprintf("%s[%s]", l.Modifier.String(), strings.Join(lengths, ", "))
	}
	return fmt.Sprintf("[%s]", strings.Join(lengths, ", "))
}
func (l *LenList) Pos() int { return l.StartOffset }
func (l *LenList) End() int { return l.EndOffset }
func (l *LenList) Children() []Node {
	children := make([]Node, len(l.List))
	for _, expression := range l.List {
		children = append(children, expression)
	}

	return children
}

func (p *ProcedureType) String() string {
	if p.FP == nil {
		return "procedure"
	}
	return fmt.Sprintf("procedure%s", p.FP.String())
}
func (p *ProcedureType) Accept(vst Visitor) any { return vst.VisitProcedureType(p) }
func (p *ProcedureType) typ()                   {}
func (p *ProcedureType) Pos() int               { return p.StartOffset }
func (p *ProcedureType) End() int               { return p.EndOffset }
func (p *ProcedureType) Children() []Node {
	if p.FP != nil {
		return []Node{p.FP}
	}
	return []Node{}
}

func (p *PointerType) String() string         { return fmt.Sprintf("^%s", p.Base) }
func (p *PointerType) Accept(vst Visitor) any { return vst.VisitPointerType(p) }
func (p *PointerType) typ()                   {}
func (p *PointerType) Pos() int               { return p.StartOffset }
func (p *PointerType) End() int               { return p.EndOffset }
func (p *PointerType) Children() []Node       { return []Node{p.Base} }

func NewRecordType(base Type, fields []*FieldList, env *RecordEnv) *RecordType {
	return &RecordType{Base: base, Fields: fields, Env: env}
}
func (r *RecordType) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString("record")

	if r.Base != nil {
		t, _ := r.Base.(*NamedType)
		buf.WriteString(fmt.Sprintf("(%s)", t.Name))
	}

	var fields []string
	for _, field := range r.Fields {
		for i := 0; i < len(field.List); i++ {
			fields = append(fields, field.Type.String())
		}
	}
	buf.WriteString(fmt.Sprintf("{%s}", strings.Join(fields, "; ")))

	return buf.String()
}
func (r *RecordType) Accept(vst Visitor) any { return vst.VisitRecordType(r) }
func (r *RecordType) typ()                   {}
func (r *RecordType) Pos() int               { return r.StartOffset }
func (r *RecordType) End() int               { return r.EndOffset }
func (r *RecordType) Children() []Node {
	children := make([]Node, 0)
	for _, field := range r.Fields {
		children = append(children, field)
	}

	if r.Base != nil {
		children = append(children, r.Base)
	}

	return children
}

func (f *FieldList) Accept(vst Visitor) any { return vst.VisitFieldList(f) }
func (f *FieldList) String() string {
	var fields []string
	for _, field := range f.List {
		fields = append(fields, field.String())
	}

	return fmt.Sprintf("%s: %s", strings.Join(fields, ", "), f.Type)
}
func (f *FieldList) Pos() int { return f.StartOffset }
func (f *FieldList) End() int { return f.EndOffset }
func (f *FieldList) Children() []Node {
	children := []Node{f.Type}
	for _, def := range f.List {
		children = append(children, def)
	}

	return children
}

func (e *EnumType) String() string         { return fmt.Sprintf("enum(%s)", strings.Join(e.Variants, ", ")) }
func (e *EnumType) Accept(vst Visitor) any { return vst.VisitEnumType(e) }
func (e *EnumType) typ()                   {}
func (e *EnumType) Pos() int               { return e.StartOffset }
func (e *EnumType) End() int               { return e.EndOffset }
func (e *EnumType) Children() []Node       { return []Node{} }

func (b *BadType) String() string         { return "<BadType>" }
func (b *BadType) Accept(vst Visitor) any { return vst.VisitBadType(b) }
func (b *BadType) typ()                   {}
func (b *BadType) Pos() int               { return b.StartOffset }
func (b *BadType) End() int               { return b.EndOffset }
func (b *BadType) Children() []Node       { return []Node{} }
