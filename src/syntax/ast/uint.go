package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type UInt struct {
	LitPos *token.Position
	Value  string
}

func (i *UInt) Pos() *token.Position {
	return i.LitPos
}

func (i *UInt) End() *token.Position {
	return &token.Position{
		Filename: i.LitPos.Filename,
		Line:     i.LitPos.Line,
		Column:   i.LitPos.Column + len(i.Value),
	}
}

func (i *UInt) expr() {}
func (i *UInt) String() string {
	return i.Value
}
