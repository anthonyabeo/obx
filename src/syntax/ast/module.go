package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type Module struct {
	BeginName  *Ident
	EndName    *Ident
	MetaParams []*MetaSection
	ImportList []*Import
	DeclSeq    []Declaration
	StmtSeq    []Statement
}

func (m *Module) Accept(vst Visitor) { vst.VisitModule(m) }

type MetaSection struct {
	Mode    token.Token
	Ids     []*Ident
	TyConst Type
}
