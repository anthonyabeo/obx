package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type UnaryExpr struct {
	OpPos *token.Position
	Op    token.Token
	X     Expression
	EType types.Type
}

func (u *UnaryExpr) Pos() *token.Position {
	return u.OpPos
}

func (u *UnaryExpr) End() *token.Position {
	return u.X.End()
}

func (u *UnaryExpr) Type() types.Type {
	return u.EType
}

func (u *UnaryExpr) Accept(vst Visitor) {
	vst.VisitUnaryExpr(u)
}

func (u *UnaryExpr) expr() {}
func (u *UnaryExpr) String() string {
	return fmt.Sprintf("%v%v", u.Op, u.X)
}
