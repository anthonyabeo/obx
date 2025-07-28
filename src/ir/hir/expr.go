package hir

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
)

type (
	Literal struct {
		Kind  token.Kind
		Value string
		Ty    types.Type
	}

	Variable struct {
		Name        string
		MangledName string
		Ty          types.Type
		Props       ast.IdentProps
	}

	Constant struct {
		Name        string
		MangledName string
		Ty          types.Type
		Props       ast.IdentProps
	}

	Procedure struct {
		Name        string
		MangledName string
		Ty          types.Type
		Props       ast.IdentProps
	}

	FieldAccess struct {
		Record Expr
		Field  string
		Ty     types.Type
	}

	IndexExpr struct {
		Array Expr
		Index []Expr
		Ty    types.Type
	}

	DerefExpr struct {
		Pointer Expr
		Ty      types.Type
	}

	TypeGuardExpr struct {
		Expr Expr
		Ty   types.Type
	}

	BinaryExpr struct {
		Left  Expr
		Right Expr
		Op    token.Kind
		Ty    types.Type
	}

	UnaryExpr struct {
		Operand Expr
		Op      token.Kind
		Ty      types.Type
	}

	FuncCallExpr struct {
		Proc *Procedure
		Args []Expr
		Ty   types.Type
	}

	SetExpr struct {
		Elems []Expr
		Ty    types.Type
	}

	RangeExpr struct {
		Low  Expr
		High Expr
		Ty   types.Type
	}
)

func (*Literal) expr()       {}
func (*BinaryExpr) expr()    {}
func (*UnaryExpr) expr()     {}
func (*FuncCallExpr) expr()  {}
func (*FieldAccess) expr()   {}
func (*IndexExpr) expr()     {}
func (*DerefExpr) expr()     {}
func (*TypeGuardExpr) expr() {}
func (*SetExpr) expr()       {}
func (*RangeExpr) expr()     {}
func (*Variable) expr()      {}
func (*Constant) expr()      {}
func (*Procedure) expr()     {}

func (e *Literal) Type() types.Type       { return e.Ty }
func (e *BinaryExpr) Type() types.Type    { return e.Ty }
func (e *UnaryExpr) Type() types.Type     { return e.Ty }
func (e *FuncCallExpr) Type() types.Type  { return e.Ty }
func (e *FieldAccess) Type() types.Type   { return e.Ty }
func (e *IndexExpr) Type() types.Type     { return e.Ty }
func (e *DerefExpr) Type() types.Type     { return e.Ty }
func (e *TypeGuardExpr) Type() types.Type { return e.Ty }
func (e *SetExpr) Type() types.Type       { return e.Ty }
func (e *RangeExpr) Type() types.Type     { return e.Ty }
func (e *Variable) Type() types.Type      { return e.Ty }
func (e *Constant) Type() types.Type      { return e.Ty }
func (e *Procedure) Type() types.Type     { return e.Ty }

func (e *Literal) String() string    { return fmt.Sprintf("%s(%v)", e.Kind, e.Value) }
func (e *BinaryExpr) String() string { return fmt.Sprintf("%s %s %s", e.Left, e.Op, e.Right) }
func (e *UnaryExpr) String() string  { return fmt.Sprintf("%s%s", e.Op, e.Operand) }
func (e *FuncCallExpr) String() string {
	var args []string
	for _, arg := range e.Args {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", e.Proc, strings.Join(args, ", "))
}
func (e *FieldAccess) String() string { return fmt.Sprintf("%s.%s", e.Record, e.Field) }
func (e *IndexExpr) String() string {
	var indices []string
	for _, index := range e.Index {
		indices = append(indices, fmt.Sprintf("[%s]", index))
	}

	return fmt.Sprintf("%s%s: %s", e.Array, strings.Join(indices, ""), e.Ty)
}
func (e *DerefExpr) String() string     { return fmt.Sprintf("%s^", e.Pointer) }
func (e *TypeGuardExpr) String() string { panic("not implemented") }
func (e *SetExpr) String() string       { panic("not implemented") }
func (e *RangeExpr) String() string     { panic("not implemented") }
func (e *Variable) String() string      { return e.Name }
func (e *Constant) String() string      { return e.Name }
func (e *Procedure) String() string     { return e.Name }
