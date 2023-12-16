package ast

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Ident struct {
	NamePos  *token.Position
	Name     string
	Exported bool
	EType    types.Type
}

func (id *Ident) Pos() *token.Position {
	return id.NamePos
}

func (id *Ident) End() *token.Position {
	return &token.Position{
		Filename: id.NamePos.Filename,
		Line:     id.NamePos.Line,
		Column:   id.NamePos.Column + len(id.Name),
	}
}

func (id *Ident) String() string {
	return id.Name
}

func (id *Ident) expr() {}

func (id *Ident) Accept(vst Visitor) {
	vst.VisitIdentifier(id)
}

func (id *Ident) Type() types.Type {
	return id.EType
}
