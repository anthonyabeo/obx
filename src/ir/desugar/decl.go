package desugar

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/sema/types"
)

type (
	Variable struct {
		Name        string
		Mangled     string
		Type        types.Type
		Size        int
		IsExport    bool
		IsReadOnly  bool
		StartOffset int
		EndOffset   int
	}

	Constant struct {
		Name        string
		Mangled     string
		Type        types.Type
		Value       Expr
		Size        int
		IsExport    bool
		IsReadOnly  bool
		StartOffset int
		EndOffset   int
	}

	Type struct {
		Name        string
		Mangled     string
		Type        types.Type
		IsExport    bool
		IsReadOnly  bool
		StartOffset int
		EndOffset   int
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
		StartOffset int
		EndOffset   int
		// FFI fields
		IsExternal bool
		IsVarArgs  bool
		DLLName    string
	}
)

func (*Variable) decl()            {}
func (d *Variable) Pos() int       { return d.StartOffset }
func (d *Variable) End() int       { return d.EndOffset }
func (d *Variable) String() string { return fmt.Sprintf("%s: %s", d.Name, d.Type) }

func (*Constant) decl()            {}
func (d *Constant) Pos() int       { return d.StartOffset }
func (d *Constant) End() int       { return d.EndOffset }
func (d *Constant) String() string { return fmt.Sprintf("%s = %s", d.Name, d.Value) }

func (*Type) decl()            {}
func (d *Type) Pos() int       { return d.StartOffset }
func (d *Type) End() int       { return d.EndOffset }
func (d *Type) String() string { return fmt.Sprintf("%s = %s", d.Name, d.Type) }

func (*Function) decl()            {}
func (f *Function) Pos() int       { return f.StartOffset }
func (f *Function) End() int       { return f.EndOffset }
func (f *Function) String() string { panic("not implemented") }
func (f *Function) FnName() string {
	name := f.Mangled
	if f.Mangled == "" {
		name = f.Name
	}

	return name
}
