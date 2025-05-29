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

		StartOffset int
		EndOffset   int
	}

	ConstantDecl struct {
		Name  *IdentifierDef
		Value Expression

		StartOffset int
		EndOffset   int
	}

	TypeDecl struct {
		Name        *IdentifierDef
		DenotedType Type

		StartOffset int
		EndOffset   int
	}

	// ProcedureDecl
	// -----------------------------------------------------
	ProcedureDecl struct {
		Head    *ProcedureHeading
		Body    *ProcedureBody
		EndName string
		Env     *Environment

		StartOffset int
		EndOffset   int
	}

	ProcedureHeading struct {
		Rcv  *Receiver
		Name *IdentifierDef
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
		Mod   token.Kind
		Names []*IdentifierDef
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
		Mod  token.Kind
		Var  string
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

func (c *ConstantDecl) decl()                  {}
func (c *ConstantDecl) String() string         { return fmt.Sprintf("%v = %v", c.Name, c.Value) }
func (c *ConstantDecl) Accept(vst Visitor) any { return vst.VisitConstantDecl(c) }
func (c *ConstantDecl) Pos() int               { return c.StartOffset }
func (c *ConstantDecl) End() int               { return c.EndOffset }

func (t *TypeDecl) decl()                  {}
func (t *TypeDecl) String() string         { return fmt.Sprintf("%s = %s", t.Name, t.DenotedType) }
func (t *TypeDecl) Accept(vst Visitor) any { return vst.VisitTypeDecl(t) }
func (t *TypeDecl) Pos() int               { return t.StartOffset }
func (t *TypeDecl) End() int               { return t.EndOffset }

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

func (p *ProcedureBody) String() string         { panic("not implemented") }
func (p *ProcedureBody) Pos() int               { return p.StartOffset }
func (p *ProcedureBody) End() int               { return p.EndOffset }
func (p *ProcedureBody) Accept(vst Visitor) any { return vst.VisitProcedureBody(p) }

func (sec *FPSection) String() string {
	buf := new(bytes.Buffer)
	if sec.Mod != token.ILLEGAL {
		buf.WriteString(sec.Mod.String() + " ")
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
func (r *Receiver) Accept(vst Visitor) any { return vst.VisitReceiver(r) }
func (r *Receiver) Pos() int               { return r.StartOffset }
func (r *Receiver) End() int               { return r.EndOffset }

func (b *BadDecl) Accept(vst Visitor) any { return vst.VisitBadDecl(b) }
func (b *BadDecl) decl()                  {}
func (b *BadDecl) String() string         { return "<BadDecl>" }
func (b *BadDecl) Pos() int               { return b.StartOffset }
func (b *BadDecl) End() int               { return b.EndOffset }
