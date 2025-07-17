package hir

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/types"
)

type (
	VarDecl struct {
		Name       string
		Type       types.Type
		IsExported bool
	}

	ConstDecl struct {
		Name       string
		Type       types.Type
		Value      Expr
		IsExported bool
	}

	TypeDecl struct {
		Name       string
		Type       types.Type
		IsExported bool
	}

	ProcedureDecl struct {
		Name       string
		Params     []*Param
		Result     types.Type // nil if procedure has no return
		Locals     []Decl
		Body       *CompoundStmt
		IsExported bool
	}
)

func (*VarDecl) isDecl()       {}
func (*ConstDecl) isDecl()     {}
func (*TypeDecl) isDecl()      {}
func (*ProcedureDecl) isDecl() {}

func (d *VarDecl) String() string       { return fmt.Sprintf("%s: %s", d.Name, d.Type) }
func (d *ConstDecl) String() string     { return fmt.Sprintf("%s = %s", d.Name, d.Value) }
func (d *TypeDecl) String() string      { return fmt.Sprintf("%s = %s", d.Name, d.Type) }
func (d *ProcedureDecl) String() string { panic("not implemented") }
