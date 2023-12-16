package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type VarDecl struct {
	Var       *token.Position
	IdentList []*Ident
	Type      Expression
}

func (v *VarDecl) Pos() *token.Position {
	return v.Var
}

func (v *VarDecl) End() *token.Position {
	panic("not implemented")
}

func (v *VarDecl) Accept(vst Visitor) {
	vst.VisitVarDecl(v)
}

func (v *VarDecl) decl() {}

func (v *VarDecl) String() string {
	return fmt.Sprintf("")
}
