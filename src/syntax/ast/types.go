package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type (
	NamedType struct {
		Name *QualifiedIdent

		pos *report.Position
		rng *report.Range
	}

	BasicType struct {
		name string // e.g. integer, bool

		pos *report.Position
		rng *report.Range
	}

	ArrayType struct {
		LenList  *LenList
		ElemType Type

		Pos *report.Position
		Rng *report.Range
	}

	ProcedureType struct {
		FP *FormalParams

		Pos *report.Position
		Rng *report.Range
	}

	PointerType struct {
		Base Type

		Pos *report.Position
		Rng *report.Range
	}

	RecordType struct {
		Base   Type
		Fields []*FieldList
		Env    *RecordEnv

		Pos *report.Position
		Rng *report.Range
	}

	FieldList struct {
		List []*IdentifierDef
		Type Type

		Pos *report.Position
		Rng *report.Range
	}

	EnumType struct {
		Variants []string

		Pos *report.Position
		Rng *report.Range
	}

	BadType struct {
		Pos *report.Position
		Rng *report.Range
	}
)

func NewNamedType(name *QualifiedIdent, pos *report.Position, rng *report.Range) *NamedType {
	return &NamedType{Name: name, pos: pos, rng: rng}
}

func (n *NamedType) Width() int                 { panic("implement me") }
func (n *NamedType) String() string             { return n.Name.String() }
func (n *NamedType) Accept(vst Visitor)         { vst.VisitNamedType(n) }
func (n *NamedType) typ()                       {}
func (n *NamedType) Position() *report.Position { return n.pos }
func (n *NamedType) Range() *report.Range       { return n.rng }

func NewBasicType(name string, pos *report.Position, rng *report.Range) *BasicType {
	return &BasicType{name: name, pos: pos, rng: rng}
}

func (b *BasicType) Name() string       { return b.name }
func (b *BasicType) String() string     { return b.name }
func (b *BasicType) Accept(vst Visitor) { vst.VisitBasicType(b) }
func (b *BasicType) typ()               {}
func (b *BasicType) Width() int {
	switch b.name {
	case "byte", "int8", "bool", "char":
		return 1
	case "int16", "wchar":
		return 2
	case "int", "real", "set":
		return 4
	case "int64", "lreal":
		return 8
	default:
		panic(fmt.Sprintf("'%s' is not a basic type", b.name))
	}
}
func (b *BasicType) Position() *report.Position { return b.pos }
func (b *BasicType) Range() *report.Range       { return b.rng }

func NewArray(lenList *LenList, elem Type, pos *report.Position, rng *report.Range) *ArrayType {
	return &ArrayType{LenList: lenList, ElemType: elem, Pos: pos, Rng: rng}
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
func (a *ArrayType) Accept(vst Visitor)         { vst.VisitArrayType(a) }
func (a *ArrayType) typ()                       {}
func (a *ArrayType) Width() int                 { panic("implement me") }
func (a *ArrayType) Position() *report.Position { return a.Pos }
func (a *ArrayType) Range() *report.Range       { return a.Rng }

type LenList struct {
	Modifier token.Kind
	List     []Expression

	Pos *report.Position
	Rng *report.Range
}

func (p *ProcedureType) String() string {
	if p.FP == nil {
		return "procedure"
	}
	return fmt.Sprintf("procedure%s", p.FP.String())
}
func (p *ProcedureType) Accept(vst Visitor)         { vst.VisitProcType(p) }
func (p *ProcedureType) typ()                       {}
func (p *ProcedureType) Width() int                 { panic("implement me") }
func (p *ProcedureType) Position() *report.Position { return p.Pos }
func (p *ProcedureType) Range() *report.Range       { return p.Rng }

func (p *PointerType) String() string             { return fmt.Sprintf("^%s", p.Base) }
func (p *PointerType) Accept(vst Visitor)         { vst.VisitPointerType(p) }
func (p *PointerType) typ()                       {}
func (p *PointerType) Width() int                 { panic("implement me") }
func (p *PointerType) Position() *report.Position { return p.Pos }
func (p *PointerType) Range() *report.Range       { return p.Rng }

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
func (r *RecordType) Accept(vst Visitor)         { vst.VisitRecordType(r) }
func (r *RecordType) typ()                       {}
func (r *RecordType) Width() int                 { panic("implement me") }
func (r *RecordType) Position() *report.Position { return r.Pos }
func (r *RecordType) Range() *report.Range       { return r.Rng }

func (e *EnumType) String() string             { return fmt.Sprintf("enum(%s)", strings.Join(e.Variants, ", ")) }
func (e *EnumType) Accept(vst Visitor)         { vst.VisitEnumType(e) }
func (e *EnumType) typ()                       {}
func (e *EnumType) Width() int                 { panic("implement me") }
func (e *EnumType) Position() *report.Position { return e.Pos }
func (e *EnumType) Range() *report.Range       { return e.Rng }

func (b *BadType) String() string             { return "<BadType>" }
func (b *BadType) Accept(Visitor)             {}
func (b *BadType) typ()                       {}
func (b *BadType) Width() int                 { panic("implement me") }
func (b *BadType) Position() *report.Position { return b.Pos }
func (b *BadType) Range() *report.Range       { return b.Rng }
