package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type (
	VariableDecl struct {
		IdentList []*IdentifierDef
		Type      Type
	}

	ConstantDecl struct {
		Name  *IdentifierDef
		Value Expression
	}

	TypeDecl struct {
		Name        *IdentifierDef
		DenotedType Type
	}

	// ProcedureDecl
	// -----------------------------------------------------
	ProcedureDecl struct {
		Head    *ProcedureHeading
		Body    *ProcedureBody
		EndName string
	}

	ProcedureHeading struct {
		Rcv  *Receiver
		Name *IdentifierDef
		FP   *FormalParams
	}

	ProcedureBody struct {
		DeclSeq []Declaration
		StmtSeq []Statement
	}

	// FPSection
	// ----------------------------------------------
	FPSection struct {
		Mod   token.Kind
		Names []string
		Type  Type
	}

	FormalParams struct {
		Params  []*FPSection
		RetType Type
	}

	Receiver struct {
		Mod  token.Kind
		Var  string
		Type Type
	}

	BadDecl struct{}
)

func (v *VariableDecl) decl() {}
func (v *VariableDecl) String() string {
	var list []string
	for _, id := range v.IdentList {
		list = append(list, id.Name)
	}

	return fmt.Sprintf("%s: %s", strings.Join(list, ", "), v.Type)
}
func (v *VariableDecl) Accept(vst Visitor) { vst.VisitVariableDecl(v) }

func (c *ConstantDecl) decl()              {}
func (c *ConstantDecl) String() string     { return fmt.Sprintf("%v = %v", c.Name, c.Value) }
func (c *ConstantDecl) Accept(vst Visitor) { vst.VisitConstantDecl(c) }

func (t *TypeDecl) decl()              {}
func (t *TypeDecl) String() string     { return fmt.Sprintf("%s = %s", t.Name, t.DenotedType) }
func (t *TypeDecl) Accept(vst Visitor) { vst.VisitTypeDecl(t) }

func (p *ProcedureDecl) decl() {}
func (p *ProcedureDecl) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString(fmt.Sprintf("proc %s", p.Head.String()))
	if len(p.Body.DeclSeq) > 0 {
		buf.WriteString(p.Body.String())
	}

	return buf.String()
}
func (p *ProcedureDecl) Accept(vst Visitor) { vst.VisitProcedureDecl(p) }

func (p *ProcedureHeading) String() string {
	buf := new(bytes.Buffer)
	if p.Rcv != nil {
		buf.WriteString(fmt.Sprintf("%s ", p.Rcv.String()))
	}
	buf.WriteString(fmt.Sprintf("%s%s", p.Name, p.FP.String()))

	return buf.String()
}
func (p *ProcedureHeading) Accept(vst Visitor) { vst.VisitProcedureHeading(p) }
func (p *ProcedureHeading) decl()              {}

func (p *ProcedureBody) String() string { panic("not implemented") }

func (sec *FPSection) String() string {
	buf := new(bytes.Buffer)
	if sec.Mod != token.ILLEGAL {
		buf.WriteString(sec.Mod.String())
	}

	var names []string
	for _, name := range sec.Names {
		names = append(names, name)
	}
	buf.WriteString(fmt.Sprintf("%s: %s", strings.Join(names, ", "), sec.Type))

	return buf.String()
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

func (r *Receiver) String() string {
	buf := new(bytes.Buffer)

	buf.WriteString("(")
	if r.Mod != token.ILLEGAL {
		buf.WriteString(fmt.Sprintf("%s ", r.Mod.String()))
	}
	buf.WriteString(fmt.Sprintf("%s: %s", r.Var, r.Type))
	buf.WriteString(")")

	return buf.String()
}

func (b *BadDecl) Accept(Visitor) {}
func (b *BadDecl) decl()          {}
func (b *BadDecl) String() string { return "<BadDecl>" }
