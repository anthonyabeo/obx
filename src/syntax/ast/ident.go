package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type Ident struct {
	NamePos  *token.Position
	Name     string
	Exported bool
}

func (i *Ident) Pos() *token.Position {
	return i.NamePos
}

func (i *Ident) End() *token.Position {
	return &token.Position{
		Filename: i.NamePos.Filename,
		Line:     i.NamePos.Line,
		Column:   i.NamePos.Column + len(i.Name),
	}
}

func (i *Ident) String() string {
	return i.Name
}

func (i *Ident) expr() {}
