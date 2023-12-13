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

type ProcDecl struct {
	Head *ProcHead
	Body *ProcBody
}

func (p *ProcDecl) decl() {}

func (p *ProcDecl) String() string {
	return ""
}

type FPSection struct {
	Mod   token.Token
	Names []*Ident
	Type  Expression
}

type FormalParams struct {
	Params  []*FPSection
	RetType Expression
}
