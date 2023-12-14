package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type BinaryExpr struct {
	OpPos *token.Position
	X     Expression  // left operand
	Op    token.Token // operator
	Y     Expression  // right operand
}

func (b *BinaryExpr) expr() {}

func (b *BinaryExpr) Pos() *token.Position {
	return b.OpPos
}

func (b *BinaryExpr) End() *token.Position {
	return b.Y.End()
}

func (b *BinaryExpr) String() string {
	return fmt.Sprintf("%v %v %v", b.X, b.Op, b.Y)
}
