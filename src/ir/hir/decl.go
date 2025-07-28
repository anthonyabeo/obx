package hir

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/types"
)

type (
	VariableDecl struct {
		Name  string
		Type  types.Type
		Props ast.IdentProps
	}

	ConstDecl struct {
		Name  string
		Type  types.Type
		Value Expr
		Props ast.IdentProps
	}

	TypeDecl struct {
		Name  string
		Type  types.Type
		Props ast.IdentProps
	}

	ProcedureDecl struct {
		Name   string
		Params []*Param
		Result types.Type // nil if procedure has no return
		Locals []Decl
		Body   *CompoundStmt
		Props  ast.IdentProps
	}
)

func (*VariableDecl) decl()  {}
func (*ConstDecl) decl()     {}
func (*TypeDecl) decl()      {}
func (*ProcedureDecl) decl() {}

func (d *VariableDecl) String() string  { return fmt.Sprintf("%s: %s", d.Name, d.Type) }
func (d *ConstDecl) String() string     { return fmt.Sprintf("%s = %s", d.Name, d.Value) }
func (d *TypeDecl) String() string      { return fmt.Sprintf("%s = %s", d.Name, d.Type) }
func (d *ProcedureDecl) String() string { panic("not implemented") }
