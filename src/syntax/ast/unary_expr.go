package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type UnaryExpr struct {
	Op token.Token
	X  Expression
}

func (u *UnaryExpr) expr() {}
func (u *UnaryExpr) String() string {
	return fmt.Sprintf("%v%v", u.Op, u.X)
}
