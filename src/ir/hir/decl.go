package hir

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/types"
)

type (
	Variable struct {
		Name       string
		Mangled    string
		Type       types.Type
		Size       int
		Offset     int
		IsExport   bool
		IsReadOnly bool
	}

	Constant struct {
		Name       string
		Mangled    string
		Type       types.Type
		Value      Expr
		Size       int
		Offset     int
		IsExport   bool
		IsReadOnly bool
	}

	Type struct {
		Name       string
		Mangled    string
		Type       types.Type
		IsExport   bool
		IsReadOnly bool
	}

	Function struct {
		Name        string
		Mangled     string
		Params      []*Param
		Result      types.Type // nil if procedure has no return
		Locals      []Decl
		Body        *CompoundStmt
		IsExport    bool
		IsReadOnly  bool
		IsTypeBound bool
	}
)

func (*Variable) decl() {}
func (*Constant) decl() {}
func (*Type) decl()     {}
func (*Function) decl() {}

func (d *Variable) String() string { return fmt.Sprintf("%s: %s", d.Name, d.Type) }
func (d *Constant) String() string { return fmt.Sprintf("%s = %s", d.Name, d.Value) }
func (d *Type) String() string     { return fmt.Sprintf("%s = %s", d.Name, d.Type) }
func (d *Function) String() string { panic("not implemented") }
