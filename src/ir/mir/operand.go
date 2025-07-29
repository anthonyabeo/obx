package mir

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/ir"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type Operand interface {
	operand()
	String() string
	Type() Type
}

var (
	True  = IntegerConst{Value: 1, Signed: true, Bits: 1, Typ: Int1Type}
	False = IntegerConst{Value: 0, Signed: true, Bits: 1, Typ: Int1Type}
)

type (
	Temp struct {
		Name string
		Typ  Type
	}

	IntegerConst struct {
		Value  uint64
		Signed bool
		Bits   uint
		Typ    Type
	}

	FloatConst struct {
		Value float64
		Bits  uint
		Typ   Type
	}

	CharConst struct {
		Value []rune
		Typ   Type
	}

	StrConst struct {
		Value string
		Typ   Type
	}

	Ident struct {
		Name  string
		Kind  ast.SymbolKind
		Typ   Type
		Props ast.IdentProps
	}

	Binary struct {
		Op    ir.OpCode // "+", "-", "*", "/", etc.
		Left  Operand
		Right Operand
		Typ   Type
	}

	FuncCall struct {
		Callee  Operand
		Args    []Operand
		RetType Type
	}

	Variable struct {
		Name       string
		Typ        Type
		Size       int // number of bytes
		Offset     int
		IsExport   bool
		IsReadOnly bool
	}
)

func (Temp) operand()         {}
func (IntegerConst) operand() {}
func (FloatConst) operand()   {}
func (CharConst) operand()    {}
func (StrConst) operand()     {}
func (Ident) operand()        {}
func (*Binary) operand()      {}
func (*Variable) operand()    {}
func (*FuncCall) operand()    {}

func (o Temp) Type() Type         { return o.Typ }
func (o IntegerConst) Type() Type { return o.Typ }
func (o FloatConst) Type() Type   { return o.Typ }
func (o CharConst) Type() Type    { return o.Typ }
func (o StrConst) Type() Type     { return o.Typ }
func (o Ident) Type() Type        { return o.Typ }
func (o *Binary) Type() Type      { return o.Typ }
func (o *FuncCall) Type() Type    { return o.RetType }
func (o *Variable) Type() Type    { return o.Typ }

func (o Temp) String() string         { return o.Name }
func (o IntegerConst) String() string { return fmt.Sprintf("%d %s", o.Value, o.Typ) }
func (o FloatConst) String() string   { return fmt.Sprintf("%f %s", o.Value, o.Typ) }
func (o CharConst) String() string    { return fmt.Sprintf("%v %s", o.Value, o.Typ) }
func (o StrConst) String() string     { return fmt.Sprintf("%s %s", o.Value, o.Typ) }
func (o Ident) String() string        { return fmt.Sprintf("%s %s", o.Name, o.Typ) }
func (o *Binary) String() string {
	return fmt.Sprintf("%s %s %s: %s", o.Left, o.Op, o.Right, o.Typ)
}
func (o *Variable) String() string { return o.Name }
func (o *FuncCall) String() string {
	var args []string
	for _, op := range o.Args {
		args = append(args, fmt.Sprintf("%s %%%s", op.Type(), op.String()))
	}

	return fmt.Sprintf("call %s @%s(%s)", o.RetType, o.Callee, strings.Join(args, ", "))
}
