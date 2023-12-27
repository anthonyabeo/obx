package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type Import struct {
	Import     *token.Position
	Alias      *Ident
	Name       *Ident
	ImportPath []*Ident
	Meta       []Expression
}

func (imp *Import) Pos() *token.Position { panic("not implemented") }
func (imp *Import) End() *token.Position { panic("not implemented") }
func (imp *Import) String() string       { panic("not implemented") }
func (imp *Import) Accept(vst Visitor)   { vst.VisitImport(imp) }
