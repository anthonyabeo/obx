package hir

import (
	"fmt"
	"strings"
)

type VarExpr struct {
	Name string
	Ty   Type
}

type FieldAccess struct {
	Record Expr
	Field  string
	Ty     Type
}

type IndexExpr struct {
	Array Expr
	Index []Expr
	Ty    Type
}

type DerefExpr struct {
	Pointer Expr
	Ty      Type
}

type TypeGuardExpr struct {
	Expr Expr
	Ty   Type
}

type BinaryExpr struct {
	Op    Op
	Left  Expr
	Right Expr
	Ty    Type
}

type UnaryExpr struct {
	Op Op
	E  Expr
	Ty Type
}

type FuncCallExpr struct {
	Proc Expr
	Args []Expr
	Ty   Type
}

type SetExpr struct {
	Elems []Expr
	Ty    Type
}

type RangeExpr struct {
	Low  Expr
	High Expr
	Ty   Type
}

func (*VarExpr) isExpr()       {}
func (*BinaryExpr) isExpr()    {}
func (*UnaryExpr) isExpr()     {}
func (*FuncCallExpr) isExpr()  {}
func (*FieldAccess) isExpr()   {}
func (*IndexExpr) isExpr()     {}
func (*DerefExpr) isExpr()     {}
func (*TypeGuardExpr) isExpr() {}
func (*SetExpr) isExpr()       {}
func (*RangeExpr) isExpr()     {}

func (e *VarExpr) Type() Type       { return e.Ty }
func (e *BinaryExpr) Type() Type    { return e.Ty }
func (e *UnaryExpr) Type() Type     { return e.Ty }
func (e *FuncCallExpr) Type() Type  { return e.Ty }
func (e *FieldAccess) Type() Type   { return e.Ty }
func (e *IndexExpr) Type() Type     { return e.Ty }
func (e *DerefExpr) Type() Type     { return e.Ty }
func (e *TypeGuardExpr) Type() Type { return e.Ty }
func (e *SetExpr) Type() Type       { return e.Ty }
func (e *RangeExpr) Type() Type     { return e.Ty }

func (e *VarExpr) String() string    { return fmt.Sprintf("%s", e.Name) }
func (e *BinaryExpr) String() string { return fmt.Sprintf("%s %s %s", e.Left, e.Op, e.Right) }
func (e *UnaryExpr) String() string  { return fmt.Sprintf("%s%s", e.Op, e.E) }
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
