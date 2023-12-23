package ast

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// An ArrayType represents an array type AST node.
type ArrayType struct {
	Array    *token.Position
	LenList  *LenList
	ElemType Expression
	EType    types.Type
}

func NewArray(pos *token.Position, lenList *LenList, elem Expression) *ArrayType {
	return &ArrayType{Array: pos, LenList: lenList, ElemType: elem}
}

func (a *ArrayType) expr()                {}
func (a *ArrayType) Pos() *token.Position { return a.Array }
func (a *ArrayType) End() *token.Position { return a.ElemType.End() }
func (a *ArrayType) String() string       { panic("not implemented") }
func (a *ArrayType) Accept(vst Visitor)   { vst.VisitArrayType(a) }
func (a *ArrayType) Type() types.Type     { return a.EType }

type LenList struct {
	Modifier token.Token
	List     []Expression
}
