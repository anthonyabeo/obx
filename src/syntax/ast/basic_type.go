package ast

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type BasicType struct {
	name string // e.g. integer, bool

	EType types.Type
}

func NewBasicType(name string) *BasicType {
	return &BasicType{name: name}
}

func (b *BasicType) Name() string         { return b.name }
func (b *BasicType) Pos() *token.Position { panic("not implemented") }
func (b *BasicType) End() *token.Position { panic("not implemented") }
func (b *BasicType) expr()                {}
func (b *BasicType) Type() types.Type     { return b.EType }
func (b *BasicType) String() string       { return b.name }
func (b *BasicType) Accept(vst Visitor) {
	vst.VisitBasicType(b)
}
