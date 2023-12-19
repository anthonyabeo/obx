package ast

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// IdentProps is a set of flags denoting the properties of an identifier
type IdentProps int

// Properties of an identifier
const (
	IsExported IdentProps = 1 << iota
	IsReadOnly
	IsPredeclared
)

type Ident struct {
	NamePos *token.Position
	Name    string
	IProps  IdentProps
	EType   types.Type
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

func (id *Ident) Props() IdentProps { return id.IProps }

func (id *Ident) Type() types.Type {
	return id.EType
}
