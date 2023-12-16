package ast

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type BinaryExpr struct {
	OpPos       *token.Position
	Left, Right Expression  // operands
	Op          token.Token // operator
	EType       types.Type
}

func (b *BinaryExpr) expr() {}

func (b *BinaryExpr) Pos() *token.Position {
	return b.OpPos
}

func (b *BinaryExpr) End() *token.Position {
	return b.Right.End()
}

func (b *BinaryExpr) String() string {
	return fmt.Sprintf("%v %v %v", b.Left, b.Op, b.Right)
}

func (b *BinaryExpr) Type() types.Type {
	return b.EType
}

func (b *BinaryExpr) Accept(vst Visitor) {
	vst.VisitBinaryExpr(b)
}
