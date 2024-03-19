package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type (
	VarDecl struct {
		Var       *token.Position
		IdentList []*Ident
		Type      Type
	}

	ConstDecl struct {
		Const *token.Position
		Name  *Ident
		Value Expression
	}

	TypeDecl struct {
		Type        *token.Position
		Name        *Ident
		DenotedType Type
	}

	// ProcDecl
	// -----------------------------------------------------
	ProcDecl struct {
		Proc    *token.Position
		Head    *ProcHead
		Body    *ProcBody
		EndName *Ident
	}

	ProcHead struct {
		Rcv  *Receiver
		Name *Ident
		FP   *FormalParams
	}

	ProcBody struct {
		DeclSeq []Declaration
		StmtSeq []Statement
	}

	// FPSection
	// ----------------------------------------------
	FPSection struct {
		Mod   token.Token
		Names []*Ident
		Type  Type
	}

	FormalParams struct {
		Params  []*FPSection
		RetType Type
	}

	Receiver struct {
		Mod  token.Token
		Var  *Ident
		Type *Ident
	}

	BadDecl struct {
		From *token.Position
		To   *token.Position
	}
)

func (v *VarDecl) decl() {}
func (v *VarDecl) String() string {
	var list []string
	for _, id := range v.IdentList {
		list = append(list, id.String())
	}

	return fmt.Sprintf("%s: %s", strings.Join(list, ", "), v.Type)
}
func (v *VarDecl) Pos() *token.Position { return v.Var }
func (v *VarDecl) End() *token.Position { panic("not implemented") }
func (v *VarDecl) Accept(vst Visitor)   { vst.VisitVarDecl(v) }

func (c *ConstDecl) decl()                {}
func (c *ConstDecl) String() string       { return fmt.Sprintf("%v = %v", c.Name, c.Value) }
func (c *ConstDecl) Pos() *token.Position { return c.Const }
func (c *ConstDecl) End() *token.Position { return c.Value.End() }
func (c *ConstDecl) Accept(vst Visitor)   { vst.VisitConstDecl(c) }

func (t *TypeDecl) decl()                {}
func (t *TypeDecl) String() string       { return fmt.Sprintf("%s = %s", t.Name, t.DenotedType) }
func (t *TypeDecl) Pos() *token.Position { return t.Type }
func (t *TypeDecl) End() *token.Position { panic("not implemented") }
func (t *TypeDecl) Accept(vst Visitor)   { vst.VisitTypeDecl(t) }

func (p *ProcDecl) decl() {}
func (p *ProcDecl) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString(fmt.Sprintf("proc %s", p.Head.String()))
	if len(p.Body.DeclSeq) > 0 {
		buf.WriteString(p.Body.String())
	}

	return buf.String()
}
func (p *ProcDecl) Pos() *token.Position { return p.Proc }
func (p *ProcDecl) End() *token.Position { panic("not implemented") }
func (p *ProcDecl) Accept(vst Visitor)   { vst.VisitProcDecl(p) }

func (p *ProcHead) String() string {
	buf := new(bytes.Buffer)
	if p.Rcv != nil {
		buf.WriteString(fmt.Sprintf("%s ", p.Rcv.String()))
	}
	buf.WriteString(fmt.Sprintf("%s%s", p.Name, p.FP.String()))

	return buf.String()
}
func (p *ProcHead) Pos() *token.Position {
	if p.Rcv != nil {
		return p.Rcv.Var.NamePos
	}

	return p.Name.NamePos
}
func (p *ProcHead) End() *token.Position { panic("not implemented") }
func (p *ProcHead) Accept(vst Visitor)   { vst.VisitProcHead(p) }
func (p *ProcHead) decl()                {}

func (p *ProcBody) String() string { panic("not implemented") }

func (sec *FPSection) String() string {
	buf := new(bytes.Buffer)
	if sec.Mod != token.ILLEGAL {
		buf.WriteString(sec.Mod.String())
	}

	var names []string
	for _, name := range sec.Names {
		names = append(names, name.Name)
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
	buf.WriteString(fmt.Sprintf("%s: %s", r.Var.Name, r.Type))
	buf.WriteString(")")

	return buf.String()
}

func (b *BadDecl) Pos() *token.Position { return b.From }
func (b *BadDecl) End() *token.Position { return b.To }
func (b *BadDecl) Accept(Visitor)       { panic("unimplemented") }
func (b *BadDecl) decl()                {}
func (b *BadDecl) String() string       { panic("unimplemented") }
