package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type (
	VariableDecl struct {
		IdentList []*IdentifierDef
		Type      Type

		Pos *report.Position
		Rng *report.Range
	}

	ConstantDecl struct {
		Name  *IdentifierDef
		Value Expression

		Pos *report.Position
		Rng *report.Range
	}

	TypeDecl struct {
		Name        *IdentifierDef
		DenotedType Type

		Pos *report.Position
		Rng *report.Range
	}

	// ProcedureDecl
	// -----------------------------------------------------
	ProcedureDecl struct {
		Head    *ProcedureHeading
		Body    *ProcedureBody
		EndName string
		Env     *Environment

		Pos *report.Position
		Rng *report.Range
	}

	ProcedureHeading struct {
		Rcv  *Receiver
		Name *IdentifierDef
		FP   *FormalParams

		Pos *report.Position
		Rng *report.Range
	}

	ProcedureBody struct {
		DeclSeq []Declaration
		StmtSeq []Statement

		Pos *report.Position
		Rng *report.Range
	}

	// FPSection
	// ----------------------------------------------
	FPSection struct {
		Mod   token.Kind
		Names []string
		Type  Type

		Pos *report.Position
		Rng *report.Range
	}

	FormalParams struct {
		Params  []*FPSection
		RetType Type

		Pos *report.Position
		Rng *report.Range
	}

	Receiver struct {
		Mod  token.Kind
		Var  string
		Type Type

		Pos *report.Position
		Rng *report.Range
	}

	BadDecl struct {
		Pos *report.Position
		Rng *report.Range
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
func (v *VariableDecl) Accept(vst Visitor) any     { return vst.VisitVariableDecl(v) }
func (v *VariableDecl) Position() *report.Position { return v.Pos }
func (v *VariableDecl) Range() *report.Range       { return v.Rng }

func (c *ConstantDecl) decl()                      {}
func (c *ConstantDecl) String() string             { return fmt.Sprintf("%v = %v", c.Name, c.Value) }
func (c *ConstantDecl) Accept(vst Visitor) any     { return vst.VisitConstantDecl(c) }
func (c *ConstantDecl) Position() *report.Position { return c.Pos }
func (c *ConstantDecl) Range() *report.Range       { return c.Rng }

func (t *TypeDecl) decl()                      {}
func (t *TypeDecl) String() string             { return fmt.Sprintf("%s = %s", t.Name, t.DenotedType) }
func (t *TypeDecl) Accept(vst Visitor) any     { return vst.VisitTypeDecl(t) }
func (t *TypeDecl) Position() *report.Position { return t.Pos }
func (t *TypeDecl) Range() *report.Range       { return t.Rng }

func (p *ProcedureDecl) decl() {}
func (p *ProcedureDecl) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString(fmt.Sprintf("proc %s", p.Head.String()))
	if len(p.Body.DeclSeq) > 0 {
		buf.WriteString(p.Body.String())
	}

	return buf.String()
}
func (p *ProcedureDecl) Accept(vst Visitor) any     { return vst.VisitProcedureDecl(p) }
func (p *ProcedureDecl) Position() *report.Position { return p.Pos }
func (p *ProcedureDecl) Range() *report.Range       { return p.Rng }

func (p *ProcedureHeading) String() string {
	buf := new(bytes.Buffer)
	if p.Rcv != nil {
		buf.WriteString(fmt.Sprintf("%s ", p.Rcv.String()))
	}
	buf.WriteString(fmt.Sprintf("%s%s", p.Name, p.FP.String()))

	return buf.String()
}
func (p *ProcedureHeading) Accept(vst Visitor) any     { return vst.VisitProcedureHeading(p) }
func (p *ProcedureHeading) decl()                      {}
func (p *ProcedureHeading) Position() *report.Position { return p.Pos }
func (p *ProcedureHeading) Range() *report.Range       { return p.Rng }

func (p *ProcedureBody) String() string             { panic("not implemented") }
func (p *ProcedureBody) Position() *report.Position { return p.Pos }
func (p *ProcedureBody) Range() *report.Range       { return p.Rng }
func (p *ProcedureBody) Accept(vst Visitor) any     { return vst.VisitProcedureBody(p) }

func (sec *FPSection) String() string {
	buf := new(bytes.Buffer)
	if sec.Mod != token.ILLEGAL {
		buf.WriteString(sec.Mod.String() + " ")
	}

	var names []string
	for _, name := range sec.Names {
		names = append(names, name)
	}
	buf.WriteString(fmt.Sprintf("%s: %s", strings.Join(names, ", "), sec.Type))

	return buf.String()
}
func (sec *FPSection) Accept(vst Visitor) any     { return vst.VisitFPSection(sec) }
func (sec *FPSection) Position() *report.Position { return sec.Pos }
func (sec *FPSection) Range() *report.Range       { return sec.Rng }

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
func (p *FormalParams) Accept(vst Visitor) any     { return vst.VisitFormalParams(p) }
func (p *FormalParams) Position() *report.Position { return p.Pos }
func (p *FormalParams) Range() *report.Range       { return p.Rng }

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
func (r *Receiver) Accept(vst Visitor) any     { return vst.VisitReceiver(r) }
func (r *Receiver) Position() *report.Position { return r.Pos }
func (r *Receiver) Range() *report.Range       { return r.Rng }

func (b *BadDecl) Accept(vst Visitor) any     { return vst.VisitBadDecl(b) }
func (b *BadDecl) decl()                      {}
func (b *BadDecl) String() string             { return "<BadDecl>" }
func (b *BadDecl) Position() *report.Position { return b.Pos }
func (b *BadDecl) Range() *report.Range       { return b.Rng }
