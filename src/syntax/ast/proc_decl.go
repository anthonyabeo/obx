package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type Receiver struct {
	Mod  token.Token
	Var  *Ident
	Type *Ident
}

type ProcHead struct {
	Rcv  *Receiver
	Name *Ident
	FP   *FormalParams
}

type ProcBody struct {
	DeclSeq []Declaration
	StmtSeq []Statement
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

type FormalParams struct {
	Params  []*FPSection
	RetType Expression
}
