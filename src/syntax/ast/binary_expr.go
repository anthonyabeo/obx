package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type BinaryExpr struct {
	X  Expression  // left operand
	Op token.Token // operator
	Y  Expression  // right operand
}

func (b *BinaryExpr) expr() {}

func (b *BinaryExpr) String() string {
	return fmt.Sprintf("%v %v %v", b.X, b.Op, b.Y)
}
