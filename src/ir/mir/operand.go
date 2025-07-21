package mir

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/ir"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"strings"
)

type Operand interface {
	isOperand()
	String() string
}

type (
	Label struct {
		Name string
	}

	Variable struct {
		Name     string
		UniqName string // mangled name for uniqueness
		Type     Type
		Props    ast.IdentProps
	}

	Constant struct {
		Name     string
		UniqName string
		Type     Type
		Props    ast.IdentProps
	}

	Procedure struct {
		Name     string
		UniqName string
		Ty       Type
		Props    ast.IdentProps
	}

	FieldAccess struct {
		Record Operand
		Field  string
	}

	IndexExpr struct {
		Array Operand
		Index []Operand
	}

	DerefExpr struct {
		Pointer Operand
	}

	TypeGuardExpr struct {
		Expr Operand
		Type Type
	}

	IntConst struct {
		Value int64
	}

	BoolConst struct {
		Value bool
	}

	StringConst struct {
		Value string
	}

	SetConst struct {
		Elems []int
	}

	Nil struct{}

	Binary struct {
		Op    ir.Operator // "+", "-", "*", "/", etc.
		Left  Operand
		Right Operand
		Type  Type
	}

	Unary struct {
		Op   ir.Operator
		Expr Operand
		Type Type
	}

	FuncCall struct {
		Func Operand
		Args []Operand
		Type Type // return type of the function
	}

	Cmp struct {
		Op   ir.Operator
		X, Y Operand
	}
)

func (*Variable) isOperand()      {}
func (*Constant) isOperand()      {}
func (*Procedure) isOperand()     {}
func (*FieldAccess) isOperand()   {}
func (*IndexExpr) isOperand()     {}
func (*DerefExpr) isOperand()     {}
func (*IntConst) isOperand()      {}
func (*BoolConst) isOperand()     {}
func (*StringConst) isOperand()   {}
func (*SetConst) isOperand()      {}
func (*Nil) isOperand()           {}
func (*TypeGuardExpr) isOperand() {}
func (*Label) isOperand()         {}
func (*Binary) isOperand()        {}
func (*Unary) isOperand()         {}
func (*FuncCall) isOperand()      {}
func (*Cmp) isOperand()           {}

func (v *Variable) String() string      { return v.Name }
func (v *Constant) String() string      { return v.Name }
func (v *Procedure) String() string     { return v.Name }
func (v *FieldAccess) String() string   { return fmt.Sprintf("%v.%v", v.Record, v.Field) }
func (v *IndexExpr) String() string     { return fmt.Sprintf("%v[%v]", v.Array, v.Index) }
func (v *DerefExpr) String() string     { return fmt.Sprintf("%v^", v.Pointer) }
func (v *IntConst) String() string      { return fmt.Sprintf("%d", v.Value) }
func (v *BoolConst) String() string     { return fmt.Sprintf("%t", v.Value) }
func (v *StringConst) String() string   { return v.Value }
func (v *SetConst) String() string      { panic("not implemented") }
func (v *Nil) String() string           { return "nil" }
func (v *TypeGuardExpr) String() string { return fmt.Sprintf("%v(%v)", v.Expr, v.Type) }
func (v *Label) String() string         { return v.Name }
func (v *Binary) String() string {
	return fmt.Sprintf("%s %s %s", v.Left, v.Op, v.Right)
}
func (v *Unary) String() string { return fmt.Sprintf("%s %s", v.Op, v.Expr) }
func (v *FuncCall) String() string {
	var args []string
	for _, op := range v.Args {
		args = append(args, op.String())
	}

	return fmt.Sprintf("call %s(%s)", v.Func, strings.Join(args, ", "))
}
func (i *Cmp) String() string { return fmt.Sprintf("cmp %s %s, %s", i.Op, i.X, i.Y) }
