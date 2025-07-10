package mir

import (
	"fmt"
	"strings"
)

type Value interface {
	isValue()
	String() string
}

type (
	Variable struct {
		Name string
	}

	FieldAccess struct {
		Record Value
		Field  string
	}

	IndexExpr struct {
		Array Value
		Index []Value
	}

	DerefExpr struct {
		Pointer Value
	}

	TypeGuardExpr struct {
		Expr VarDecl
	}

	Binary struct {
		Op    string // "+", "-", "*", "/", etc.
		Left  Value
		Right Value
	}

	Unary struct {
		Op   string
		Expr Value
	}

	FuncCall struct {
		Func Value
		Args []Value
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

	CmpInst struct {
		X, Y Value
	}
)

func (*Variable) isValue()    {}
func (*FieldAccess) isValue() {}
func (*IndexExpr) isValue()   {}
func (*DerefExpr) isValue()   {}
func (*IntConst) isValue()    {}
func (*BoolConst) isValue()   {}
func (*StringConst) isValue() {}
func (*SetConst) isValue()    {}
func (*Binary) isValue()      {}
func (*Unary) isValue()       {}
func (*FuncCall) isValue()    {}
func (*Nil) isValue()         {}
func (*CmpInst) isValue()     {}

func (v *Variable) String() string    { return v.Name }
func (v *FieldAccess) String() string { return fmt.Sprintf("%v.%v", v.Record, v.Field) }
func (v *IndexExpr) String() string   { return fmt.Sprintf("%v[%v]", v.Array, v.Index) }
func (v *DerefExpr) String() string   { return fmt.Sprintf("%v^", v.Pointer) }
func (v *IntConst) String() string    { return fmt.Sprintf("%d", v.Value) }
func (v *BoolConst) String() string   { return fmt.Sprintf("%t", v.Value) }
func (v *StringConst) String() string { return v.Value }
func (v *SetConst) String() string    { panic("not implemented") }
func (v *Binary) String() string      { return fmt.Sprintf("%s %s %s", v.Left, v.Op, v.Right) }
func (v *Unary) String() string       { return fmt.Sprintf("%s %s", v.Op, v.Expr) }
func (v *FuncCall) String() string {
	var args []string
	for _, op := range v.Args {
		args = append(args, op.String())
	}

	return fmt.Sprintf("call %s(%s)", v.Func, strings.Join(args, ", "))
}
func (v *Nil) String() string     { return "nil" }
func (v *CmpInst) String() string { return fmt.Sprintf("cmp %s %s", v.X, v.Y) }
