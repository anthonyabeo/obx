package mir

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/ir/hir"
)

type Decl interface {
	isDecl()
	String() string
}

type (
	ConstDecl struct {
		Name  string
		Value Value
	}

	VarDecl struct {
		Name string
		Type Type
	}

	TypeDecl struct {
		Name string
		Type Type
	}

	Procedure struct {
		Name   string
		Params []*Param
		Result Type // nil if procedure is a command
		Locals []Decl
		Blocks []*Block
	}

	Param struct {
		Name string
		Type Type
		Kind hir.ParamKind // "value", "VAR", "IN"
	}
)

func (*ConstDecl) isDecl() {}
func (*VarDecl) isDecl()   {}
func (*TypeDecl) isDecl()  {}
func (*Procedure) isDecl() {}

func (d *ConstDecl) String() string { return fmt.Sprintf("CONST %s: %s", d.Name, d.Value) }
func (d *VarDecl) String() string   { return fmt.Sprintf("VAR %s: %s", d.Name, d.Type) }
func (d *TypeDecl) String() string  { return fmt.Sprintf("TYPE %s: %s", d.Name, d.Type) }
func (d *Procedure) String() string { return FormatProcedure(d, "  ") }
