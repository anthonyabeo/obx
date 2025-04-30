package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type (
	NamedType struct {
		Name Expression
	}

	BasicType struct {
		name string // e.g. integer, bool
	}

	ArrayType struct {
		LenList  *LenList
		ElemType Type
	}

	ProcType struct {
		FP *FormalParams
	}

	PointerType struct {
		Base Type
	}

	RecordType struct {
		Base   Type
		Fields []*FieldList
	}

	FieldList struct {
		List []*IdentifierDef
		Type Type
	}

	EnumType struct {
		Variants []string
	}

	BadType struct{}
)

func NewNamedType(name Expression) *NamedType {
	return &NamedType{Name: name}
}

func (n *NamedType) String() string     { return n.Name.String() }
func (n *NamedType) Accept(vst Visitor) { vst.VisitNamedType(n) }

func NewBasicType(name string) *BasicType {
	return &BasicType{name: name}
}

func (b *BasicType) Name() string       { return b.name }
func (b *BasicType) String() string     { return b.name }
func (b *BasicType) Accept(vst Visitor) { vst.VisitBasicType(b) }

func NewArray(lenList *LenList, elem Type) *ArrayType {
	return &ArrayType{LenList: lenList, ElemType: elem}
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
func (a *ArrayType) Accept(vst Visitor) { vst.VisitArrayType(a) }

type LenList struct {
	Modifier token.Token
	List     []Expression
}

func (p *ProcType) String() string     { panic("not implemented") }
func (p *ProcType) Accept(vst Visitor) { vst.VisitProcType(p) }

func (p *PointerType) String() string     { return fmt.Sprintf("^%s", p.Base) }
func (p *PointerType) Accept(vst Visitor) { vst.VisitPointerType(p) }

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
func (r *RecordType) Accept(vst Visitor) { vst.VisitRecordType(r) }

func (e *EnumType) String() string     { panic("not implemented") }
func (e *EnumType) Accept(vst Visitor) { vst.VisitEnumType(e) }

func (b *BadType) String() string { panic("implement me") }
func (b *BadType) Accept(Visitor) { panic("implement me") }
