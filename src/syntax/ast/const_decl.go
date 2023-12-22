package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type ConstDecl struct {
	Const *token.Position
	Name  *Ident
	Value Expression
}

func (c *ConstDecl) decl()                {}
func (c *ConstDecl) Pos() *token.Position { return c.Const }
func (c *ConstDecl) End() *token.Position { return c.Value.End() }
func (c *ConstDecl) Accept(vst Visitor)   { vst.VisitConstDecl(c) }
func (c *ConstDecl) String() string {
	return fmt.Sprintf("const %v = %v", c.Name, c.Value)
}
