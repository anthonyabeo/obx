package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type BadExpr struct {
	From *token.Position
	To   *token.Position
}

func (b *BadExpr) Pos() *token.Position {
	return b.From
}

func (b *BadExpr) End() *token.Position {
	return b.To
}

func (b *BadExpr) expr() {}
func (b *BadExpr) String() string {
	return ""
}
