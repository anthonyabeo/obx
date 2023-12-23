package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type (
	VarDecl struct {
		Var       *token.Position
		IdentList []*Ident
		Type      Expression
	}

	ConstDecl struct {
		Const *token.Position
		Name  *Ident
		Value Expression
	}

	TypeDecl struct {
		Type        *token.Position
		Name        *Ident
		DenotedType Expression
	}
)

func (v *VarDecl) decl()   {}
func (c *ConstDecl) decl() {}
func (t *TypeDecl) decl()  {}

func (v *VarDecl) String() string   { return fmt.Sprintf("") }
func (c *ConstDecl) String() string { return fmt.Sprintf("const %v = %v", c.Name, c.Value) }
func (t *TypeDecl) String() string  { panic("not implemented") }

func (v *VarDecl) Pos() *token.Position   { return v.Var }
func (c *ConstDecl) Pos() *token.Position { return c.Const }
func (t *TypeDecl) Pos() *token.Position  { return t.Type }

func (v *VarDecl) End() *token.Position   { panic("not implemented") }
func (c *ConstDecl) End() *token.Position { return c.Value.End() }
func (t *TypeDecl) End() *token.Position  { panic("not implemented") }

func (v *VarDecl) Accept(vst Visitor)   { vst.VisitVarDecl(v) }
func (c *ConstDecl) Accept(vst Visitor) { vst.VisitConstDecl(c) }
func (t *TypeDecl) Accept(vst Visitor)  { vst.VisitTypeDecl(t) }
