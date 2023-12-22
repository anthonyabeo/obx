package ast

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// A BasicLit node represents a literal of basic type.
type BasicLit struct {
	ValuePos *token.Position // literal position
	Kind     token.Token     // token.INT, token.REAL, token.HEXCHAR, or token.STRING
	Value    string          // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
	EType    types.Type
}

func (b *BasicLit) expr()                {}
func (b *BasicLit) Pos() *token.Position { return b.ValuePos }
func (b *BasicLit) End() *token.Position { panic("not implemented") }
func (b *BasicLit) String() string       { return b.Value }
func (b *BasicLit) Accept(vst Visitor)   { vst.VisitBasicLit(b) }
func (b *BasicLit) Type() types.Type     { return b.EType }
