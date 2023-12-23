package ast

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// An ArrayType represents an array type AST node.
type ArrayType struct {
	Array    *token.Position
	lenList  *LenList
	elemType Expression
	EType    types.Type
}

func NewArray(pos *token.Position, lenList *LenList, elem Expression) *ArrayType {
	return &ArrayType{Array: pos, lenList: lenList, elemType: elem}
}

func (a *ArrayType) expr()                {}
func (a *ArrayType) Pos() *token.Position { panic("not implemented") }
func (a *ArrayType) End() *token.Position { panic("not implemented") }
func (a *ArrayType) String() string       { panic("not implemented") }
func (a *ArrayType) Accept(vst Visitor)   { vst.VisitArrayType(a) }
func (a *ArrayType) Type() types.Type     { return a.EType }

type LenList struct {
	Modifier token.Token
	List     []Expression
}
