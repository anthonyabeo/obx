package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type Receiver struct {
	Mod  token.Token
	Var  *Ident
	Type *Ident
}

func (r *Receiver) Pos() *token.Position {
	panic("not implemented")
}

func (r *Receiver) End() *token.Position {
	panic("not implemented")
}

func (r *Receiver) String() string {
	return ""
}

func (r *Receiver) Accept(vst Visitor) {
	vst.VisitReceiver(r)
}

// ProcHead
// -----------------------------------------------------
type ProcHead struct {
	Rcv  *Receiver
	Name *Ident
	FP   *FormalParams
}

func (p *ProcHead) Pos() *token.Position {
	panic("not implemented")
}

func (p *ProcHead) End() *token.Position {
	panic("not implemented")
}

func (p *ProcHead) String() string {
	return ""
}

func (p *ProcHead) Accept(vst Visitor) {
	vst.VisitProcHead(p)
}

// ProcBody
// -----------------------------------------------------
type ProcBody struct {
	DeclSeq []Declaration
	StmtSeq []Statement
}

func (p *ProcBody) Pos() *token.Position {
	panic("not implemented")
}

func (p *ProcBody) End() *token.Position {
	panic("not implemented")
}

func (p *ProcBody) String() string {
	return ""
}

func (p *ProcBody) Accept(vst Visitor) {
	vst.VisitProcBody(p)
}

// ProcDecl
// ----------------------------------------------
type ProcDecl struct {
	Proc *token.Position
	Head *ProcHead
	Body *ProcBody
}

func (p *ProcDecl) Pos() *token.Position {
	return p.Proc
}

func (p *ProcDecl) End() *token.Position {
	panic("not implemented")
}

func (p *ProcDecl) Accept(vst Visitor) {
	vst.VisitProcDecl(p)
}

func (p *ProcDecl) decl() {}

func (p *ProcDecl) String() string {
	return ""
}

// FPSection
// ----------------------------------------------
type FPSection struct {
	Mod   token.Token
	Names []*Ident
	Type  Expression
}

func (sec *FPSection) Pos() *token.Position {
	panic("not implemented")
}

func (sec *FPSection) End() *token.Position {
	panic("not implemented")
}

func (sec *FPSection) String() string {
	return ""
}

func (sec *FPSection) Accept(vst Visitor) {
	vst.VisitFPSection(sec)
}

// FormalParams
// -------------------------------------------------
type FormalParams struct {
	Params  []*FPSection
	RetType Expression
}

func (p *FormalParams) Pos() *token.Position {
	panic("not implemented")
}

func (p *FormalParams) End() *token.Position {
	panic("not implemented")
}

func (p *FormalParams) String() string {
	return ""
}

func (p *FormalParams) Accept(vst Visitor) {
	vst.VisitFormalParams(p)
}
