package mir

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/ir/hir"
)

type Decl interface {
	isDecl()
	String() string
}

type LocalKind int

const (
	Var LocalKind = iota
	Const
)

type Local struct {
	Name   string
	Kind   LocalKind
	Offset int
	Size   int
}

type (
	ConstDecl struct {
		Name  string
		Value Operand
		Type  Type
	}

	VarDecl struct {
		Name string
		Type Type
	}

	TypeDecl struct {
		Name string
		Type Type
	}

	Function struct {
		Name   string
		Params []*Param
		Result Type // nil if procedure is a command
		Locals []Local
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
func (*Function) isDecl()  {}

func (d *ConstDecl) String() string { return fmt.Sprintf("CONST %s: %s", d.Name, d.Value) }
func (d *VarDecl) String() string   { return fmt.Sprintf("VAR %s: %s", d.Name, d.Type) }
func (d *TypeDecl) String() string  { return fmt.Sprintf("TYPE %s: %s", d.Name, d.Type) }
func (d *Function) String() string  { return FormatFunction(d, "  ") }
