package ast

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type (
	VarDecl struct {
		Var       *token.Position
		IdentList []*Ident
		Type      Expression
	}

	ConstDecl struct {
		Const *token.Position
		Name  *Ident
		Value Expression
	}

	TypeDecl struct {
		Type        *token.Position
		Name        *Ident
		DenotedType Expression
	}

	// ProcDecl
	// -----------------------------------------------------
	ProcDecl struct {
		Proc *token.Position
		Head *ProcHead
		Body *ProcBody
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
		Type  Expression
	}

	FormalParams struct {
		Params  []*FPSection
		RetType Expression
	}

	Receiver struct {
		Mod  token.Token
		Var  *Ident
		Type *Ident
	}
)

func (v *VarDecl) decl()   {}
func (c *ConstDecl) decl() {}
func (t *TypeDecl) decl()  {}
func (p *ProcDecl) decl()  {}

func (v *VarDecl) String() string      { return fmt.Sprintf("") }
func (c *ConstDecl) String() string    { return fmt.Sprintf("const %v = %v", c.Name, c.Value) }
func (t *TypeDecl) String() string     { panic("not implemented") }
func (p *ProcHead) String() string     { panic("not implemented") }
func (p *ProcBody) String() string     { panic("not implemented") }
func (p *ProcDecl) String() string     { panic("not implemented") }
func (sec *FPSection) String() string  { panic("not implemented") }
func (p *FormalParams) String() string { panic("not implemented") }
func (r *Receiver) String() string     { panic("not implemented") }

func (v *VarDecl) Pos() *token.Position      { return v.Var }
func (c *ConstDecl) Pos() *token.Position    { return c.Const }
func (t *TypeDecl) Pos() *token.Position     { return t.Type }
func (p *ProcHead) Pos() *token.Position     { panic("not implemented") }
func (p *ProcBody) Pos() *token.Position     { panic("not implemented") }
func (p *ProcDecl) Pos() *token.Position     { return p.Proc }
func (sec *FPSection) Pos() *token.Position  { panic("not implemented") }
func (p *FormalParams) Pos() *token.Position { panic("not implemented") }
func (r *Receiver) Pos() *token.Position     { panic("not implemented") }

func (v *VarDecl) End() *token.Position      { panic("not implemented") }
func (c *ConstDecl) End() *token.Position    { return c.Value.End() }
func (t *TypeDecl) End() *token.Position     { panic("not implemented") }
func (p *ProcHead) End() *token.Position     { panic("not implemented") }
func (p *ProcBody) End() *token.Position     { panic("not implemented") }
func (p *ProcDecl) End() *token.Position     { panic("not implemented") }
func (sec *FPSection) End() *token.Position  { panic("not implemented") }
func (p *FormalParams) End() *token.Position { panic("not implemented") }
func (r *Receiver) End() *token.Position     { panic("not implemented") }

func (v *VarDecl) Accept(vst Visitor)      { vst.VisitVarDecl(v) }
func (c *ConstDecl) Accept(vst Visitor)    { vst.VisitConstDecl(c) }
func (t *TypeDecl) Accept(vst Visitor)     { vst.VisitTypeDecl(t) }
func (p *ProcHead) Accept(vst Visitor)     { vst.VisitProcHead(p) }
func (p *ProcBody) Accept(vst Visitor)     { vst.VisitProcBody(p) }
func (p *ProcDecl) Accept(vst Visitor)     { vst.VisitProcDecl(p) }
func (sec *FPSection) Accept(vst Visitor)  { vst.VisitFPSection(sec) }
func (p *FormalParams) Accept(vst Visitor) { vst.VisitFormalParams(p) }
func (r *Receiver) Accept(vst Visitor)     { vst.VisitReceiver(r) }
