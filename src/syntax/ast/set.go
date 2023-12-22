package ast

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Set struct {
	Elem  []Expression
	EType types.Type
}

func (s *Set) expr()                {}
func (s *Set) String() string       { panic("not implement") }
func (s *Set) Pos() *token.Position { panic("not implemented") }
func (s *Set) End() *token.Position { panic("not implemented") }
func (s *Set) Accept(vst Visitor)   { vst.VisitSet(s) }
func (s *Set) Type() types.Type     { return s.EType }
