package hir

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
)

type ParamKind int

const (
	ValueParam ParamKind = iota
	VarParam
	InParam
)

type (
	Literal struct {
		Kind     token.Kind
		Value    string
		SemaType types.Type
	}

	BinaryExpr struct {
		Left     Expr
		Right    Expr
		Op       token.Kind
		SemaType types.Type
	}

	UnaryExpr struct {
		Operand  Expr
		Op       token.Kind
		SemaType types.Type
	}

	SetExpr struct {
		Elems    []Expr
		SemaType types.Type
	}

	RangeExpr struct {
		Low      Expr
		High     Expr
		SemaType types.Type
	}

	FuncCall struct {
		Func    *FunctionRef
		Args    []Expr
		RetType types.Type
	}

	VariableRef struct {
		Name       string     // name of the variable
		Mangled    string     // mangled name for code emission
		SemaType   types.Type // variable type
		Module     string     // name of the module in which it declared/used
		IsExported bool
		IsReadOnly bool
		Size       int
	}

	ConstantRef struct {
		Name       string // name of the variable
		Mangled    string // mangled name for code emission
		Value      Expr
		SemaType   types.Type // variable type
		Module     string     // name of the module in which it declared/used
		IsExported bool
		IsReadOnly bool
		Offset     int
		Size       int
	}

	FunctionRef struct {
		Name       string     // name of the variable
		Mangled    string     // mangled name for code emission
		SemaType   types.Type // variable type
		Kind       ast.ProcedureKind
		Module     string // name of the module in which it declared/used
		IsExported bool
		IsReadOnly bool
		Offset     int
		Size       int
	}

	TypeRef struct {
		Name       string
		Mangled    string // mangled name for code emission
		UnderType  types.Type
		Module     string // name of the module in which it declared/used
		IsExported bool
		IsReadOnly bool
		Offset     int
		Size       int
	}

	FieldAccess struct {
		Record   Expr
		Field    string
		SemaType types.Type
	}

	IndexExpr struct {
		Array    Expr
		Index    []Expr
		SemaType types.Type
	}

	DerefExpr struct {
		Pointer  Expr
		SemaType types.Type
	}

	TypeGuardExpr struct {
		Expr     Expr
		Typ      types.Type
		SemaType types.Type
	}

	Param struct {
		Name string
		Kind ParamKind // Value, Var, In
		Typ  types.Type
	}
)

func (*Literal) expr()       {}
func (*BinaryExpr) expr()    {}
func (*UnaryExpr) expr()     {}
func (*FuncCall) expr()      {}
func (*FieldAccess) expr()   {}
func (*IndexExpr) expr()     {}
func (*DerefExpr) expr()     {}
func (*TypeGuardExpr) expr() {}
func (*SetExpr) expr()       {}
func (*RangeExpr) expr()     {}
func (*VariableRef) expr()   {}
func (*ConstantRef) expr()   {}
func (*FunctionRef) expr()   {}
func (*TypeRef) expr()       {}

func (e *Literal) Type() types.Type       { return e.SemaType }
func (e *BinaryExpr) Type() types.Type    { return e.SemaType }
func (e *UnaryExpr) Type() types.Type     { return e.SemaType }
func (e *FuncCall) Type() types.Type      { return e.RetType }
func (e *FieldAccess) Type() types.Type   { return e.SemaType }
func (e *IndexExpr) Type() types.Type     { return e.SemaType }
func (e *DerefExpr) Type() types.Type     { return e.SemaType }
func (e *TypeGuardExpr) Type() types.Type { return e.SemaType }
func (e *SetExpr) Type() types.Type       { return e.SemaType }
func (e *RangeExpr) Type() types.Type     { return e.SemaType }
func (e *VariableRef) Type() types.Type   { return e.SemaType }
func (e *ConstantRef) Type() types.Type   { return e.SemaType }
func (e *FunctionRef) Type() types.Type   { return e.SemaType }
func (e *TypeRef) Type() types.Type       { return e.UnderType }

func (e *Literal) String() string    { return fmt.Sprintf("%s(%v)", e.Kind, e.Value) }
func (e *BinaryExpr) String() string { return fmt.Sprintf("%s %s %s", e.Left, e.Op, e.Right) }
func (e *UnaryExpr) String() string  { return fmt.Sprintf("%s%s", e.Op, e.Operand) }
func (e *FuncCall) String() string {
	var args []string
	for _, arg := range e.Args {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", e.Func, strings.Join(args, ", "))
}
func (e *FieldAccess) String() string { return fmt.Sprintf("%s.%s", e.Record, e.Field) }
func (e *IndexExpr) String() string {
	var indices []string
	for _, index := range e.Index {
		indices = append(indices, fmt.Sprintf("[%s]", index))
	}

	return fmt.Sprintf("%s%s: %s", e.Array, strings.Join(indices, ""), e.SemaType)
}
func (e *DerefExpr) String() string     { return fmt.Sprintf("%s^", e.Pointer) }
func (e *TypeGuardExpr) String() string { panic("not implemented") }
func (e *SetExpr) String() string       { panic("not implemented") }
func (e *RangeExpr) String() string     { panic("not implemented") }
func (e *VariableRef) String() string   { return e.Name }
func (e *ConstantRef) String() string   { return e.Name }
func (e *FunctionRef) String() string   { return e.Name }
func (e *TypeRef) String() string       { return e.Name }

func (p *Param) expr()            {}
func (p *Param) Type() types.Type { return p.Typ }
func (p *Param) String() string   { return p.Name }
