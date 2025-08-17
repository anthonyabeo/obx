package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type ProcedureKind int

const (
	ProperProcedureKind ProcedureKind = iota
	FunctionProcedureKind
	TypeBoundProcedureKind
)

type (
	VariableDecl struct {
		IdentList []*Identifier
		Type      Type

		StartOffset int
		EndOffset   int
	}

	ConstantDecl struct {
		Name  *Identifier
		Value Expression

		StartOffset int
		EndOffset   int
	}

	TypeDecl struct {
		Name        *Identifier
		DenotedType Type

		StartOffset int
		EndOffset   int
	}

	// ProcedureDecl
	// -----------------------------------------------------
	ProcedureDecl struct {
		Kind    ProcedureKind
		Head    *ProcedureHeading
		Body    *ProcedureBody
		EndName string

		Env *LexicalScope

		StartOffset int
		EndOffset   int
	}

	ProcedureHeading struct {
		Rcv  *Receiver
		Name *Identifier
		FP   *FormalParams

		StartOffset int
		EndOffset   int
	}

	ProcedureBody struct {
		DeclSeq []Declaration
		StmtSeq []Statement

		StartOffset int
		EndOffset   int
	}

	// FPSection
	// ----------------------------------------------
	FPSection struct {
		Kind  token.Kind
		Names []*Identifier
		Type  Type

		StartOffset int
		EndOffset   int
	}

	FormalParams struct {
		Params  []*FPSection
		RetType Type

		StartOffset int
		EndOffset   int
	}

	Receiver struct {
		Kind token.Kind
		Name *Identifier
		Type Type

		StartOffset int
		EndOffset   int
	}

	BadDecl struct {
		StartOffset int
		EndOffset   int
	}
)

func (v *VariableDecl) decl() {}
func (v *VariableDecl) String() string {
	var list []string
	for _, id := range v.IdentList {
		list = append(list, id.Name)
	}

	return fmt.Sprintf("%s: %s", strings.Join(list, ", "), v.Type)
}
func (v *VariableDecl) Accept(vst Visitor) any { return vst.VisitVariableDecl(v) }
func (v *VariableDecl) Pos() int               { return v.StartOffset }
func (v *VariableDecl) End() int               { return v.EndOffset }
func (v *VariableDecl) Children() []Node {
	children := make([]Node, 0)
	children = append(children, v.Type)

	for _, def := range v.IdentList {
		children = append(children, def)
	}

	return children
}

func (c *ConstantDecl) decl()                  {}
func (c *ConstantDecl) String() string         { return fmt.Sprintf("%v = %v", c.Name, c.Value) }
func (c *ConstantDecl) Accept(vst Visitor) any { return vst.VisitConstantDecl(c) }
func (c *ConstantDecl) Pos() int               { return c.StartOffset }
func (c *ConstantDecl) End() int               { return c.EndOffset }
func (c *ConstantDecl) Children() []Node       { return []Node{c.Name, c.Value} }

func (t *TypeDecl) decl()                  {}
func (t *TypeDecl) String() string         { return fmt.Sprintf("%s = %s", t.Name, t.DenotedType) }
func (t *TypeDecl) Accept(vst Visitor) any { return vst.VisitTypeDecl(t) }
func (t *TypeDecl) Pos() int               { return t.StartOffset }
func (t *TypeDecl) End() int               { return t.EndOffset }
func (t *TypeDecl) Children() []Node       { return []Node{t.Name, t.DenotedType} }

func (p *ProcedureDecl) decl() {}
func (p *ProcedureDecl) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString(fmt.Sprintf("proc %s", p.Head.String()))
	if len(p.Body.DeclSeq) > 0 {
		buf.WriteString(p.Body.String())
	}

	return buf.String()
}
func (p *ProcedureDecl) Accept(vst Visitor) any { return vst.VisitProcedureDecl(p) }
func (p *ProcedureDecl) Pos() int               { return p.StartOffset }
func (p *ProcedureDecl) End() int               { return p.EndOffset }
func (p *ProcedureDecl) Children() []Node       { return []Node{p.Head, p.Body} }

func (p *ProcedureHeading) String() string {
	buf := new(bytes.Buffer)
	if p.Rcv != nil {
		buf.WriteString(fmt.Sprintf("%s ", p.Rcv.String()))
	}
	buf.WriteString(fmt.Sprintf("%s%s", p.Name, p.FP.String()))

	return buf.String()
}
func (p *ProcedureHeading) Accept(vst Visitor) any { return vst.VisitProcedureHeading(p) }
func (p *ProcedureHeading) decl()                  {}
func (p *ProcedureHeading) Pos() int               { return p.StartOffset }
func (p *ProcedureHeading) End() int               { return p.EndOffset }
func (p *ProcedureHeading) Children() []Node {
	children := []Node{p.Name}
	if p.Rcv != nil {
		children = append(children, p.Rcv)
	}

	if p.FP != nil {
		children = append(children, p.FP)
	}

	return children
}

func (p *ProcedureBody) String() string         { panic("not implemented") }
func (p *ProcedureBody) Pos() int               { return p.StartOffset }
func (p *ProcedureBody) End() int               { return p.EndOffset }
func (p *ProcedureBody) Accept(vst Visitor) any { return vst.VisitProcedureBody(p) }
func (p *ProcedureBody) Children() []Node {
	children := make([]Node, 0)
	for _, statement := range p.StmtSeq {
		children = append(children, statement)
	}

	for _, decl := range p.DeclSeq {
		children = append(children, decl)
	}

	return children
}

func (sec *FPSection) String() string {
	buf := new(bytes.Buffer)
	if sec.Kind != token.ILLEGAL {
		buf.WriteString(sec.Kind.String() + " ")
	}

	var names []string
	for _, name := range sec.Names {
		names = append(names, name.Name)
	}
	buf.WriteString(fmt.Sprintf("%s: %s", strings.Join(names, ", "), sec.Type))

	return buf.String()
}
func (sec *FPSection) Accept(vst Visitor) any { return vst.VisitFPSection(sec) }
func (sec *FPSection) Pos() int               { return sec.StartOffset }
func (sec *FPSection) End() int               { return sec.EndOffset }
func (sec *FPSection) Children() []Node {
	children := []Node{sec.Type}
	for _, name := range sec.Names {
		children = append(children, name)
	}

	return children
}

func (p *FormalParams) String() string {
	buf := new(bytes.Buffer)

	var params []string
	for _, fp := range p.Params {
		params = append(params, fp.String())
	}

	buf.WriteString(fmt.Sprintf("(%s)", strings.Join(params, "; ")))
	if p.RetType != nil {
		buf.WriteString(fmt.Sprintf(": %s", p.RetType.String()))
	}

	return buf.String()
}
func (p *FormalParams) Accept(vst Visitor) any { return vst.VisitFormalParams(p) }
func (p *FormalParams) Pos() int               { return p.StartOffset }
func (p *FormalParams) End() int               { return p.EndOffset }
func (p *FormalParams) Children() []Node {
	children := []Node{p.RetType}
	for _, param := range p.Params {
		children = append(children, param)
	}

	return children
}

func (r *Receiver) String() string {
	buf := new(bytes.Buffer)

	buf.WriteString("(")
	if r.Kind != token.ILLEGAL {
		buf.WriteString(fmt.Sprintf("%s ", r.Kind.String()))
	}
	buf.WriteString(fmt.Sprintf("%s: %s", r.Name, r.Type))
	buf.WriteString(")")

	return buf.String()
}
func (r *Receiver) Accept(vst Visitor) any { return vst.VisitReceiver(r) }
func (r *Receiver) Pos() int               { return r.StartOffset }
func (r *Receiver) End() int               { return r.EndOffset }
func (r *Receiver) Children() []Node {
	return []Node{r.Name, r.Type}
}

func (b *BadDecl) Accept(vst Visitor) any { return vst.VisitBadDecl(b) }
func (b *BadDecl) decl()                  {}
func (b *BadDecl) String() string         { return "<BadDecl>" }
func (b *BadDecl) Pos() int               { return b.StartOffset }
func (b *BadDecl) End() int               { return b.EndOffset }
func (b *BadDecl) Children() []Node       { return []Node{} }
