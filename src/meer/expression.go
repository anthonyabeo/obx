package meer

import (
	"fmt"
	"strings"
)

type Expression interface {
	expr()
	fmt.Stringer
}

type Constant interface {
	constant()
	Operand
}

type NamedOperand interface {
	Name() string
	BaseName() string
	SetName(string)
	Operand
}

type Operand interface {
	operand()
	fmt.Stringer
}

// Ident
// ----------------------------------------
type Ident struct {
	Id string
}

func CreateIdent(id string) *Ident { return &Ident{Id: id} }

func (id *Ident) expr()            {}
func (id *Ident) operand()         {}
func (id *Ident) Name() string     { return id.Id }
func (id *Ident) BaseName() string { panic("implement me") }
func (id *Ident) SetName(s string) { panic("implement me") }
func (id *Ident) Operand(i int) Operand {
	switch i {
	case 1:
		return id
	default:
		panic("invalid operand number")
	}
}
func (id *Ident) NumOperands() int { return 1 }
func (id *Ident) String() string   { return id.Id }

// BinaryOp
// ----------------------------------------
type BinaryOp struct {
	Op   Opcode
	X, Y Expression
}

func CreateBinaryOp(op Opcode, x, y Expression) *BinaryOp {
	return &BinaryOp{
		Op: op,
		X:  x,
		Y:  y,
	}
}

func (*BinaryOp) expr() {}
func (b *BinaryOp) Operand(i int) Expression {
	switch i {
	case 1:
		return b.X
	case 2:
		return b.Y
	default:
		panic("invalid operand number")
	}
}
func (b *BinaryOp) NumOperands() int { return 2 }
func (b *BinaryOp) String() string   { return fmt.Sprintf("%s %s %s", b.X, b.Op, b.Y) }

// UnaryOp
// ----------------------------------------
type UnaryOp struct {
	Op Opcode
	X  Expression
}

func CreateUnaryOp(op Opcode, x Expression) *UnaryOp {
	return &UnaryOp{
		Op: op,
		X:  x,
	}
}

func (*UnaryOp) expr() {}
func (b *UnaryOp) Operand(i int) Expression {
	switch i {
	case 1:
		return b.X
	default:
		panic("invalid operand number")
	}
}
func (b *UnaryOp) NumOperands() int { return 1 }
func (b *UnaryOp) String() string   { return fmt.Sprintf("%s %s", b.Op, b.X) }

// CmpInst
// -------------------------
type CmpInst struct {
	Pred Opcode
	X, Y Expression
}

func CreateCmpInst(pred Opcode, x, y Expression) *CmpInst {
	return &CmpInst{
		Pred: pred,
		X:    x,
		Y:    y,
	}
}

func (*CmpInst) expr() {}
func (c *CmpInst) Operand(i int) Expression {
	switch i {
	case 1:
		return c.X
	case 2:
		return c.Y
	default:
		panic(fmt.Sprintf("invalid operand number"))
	}
}
func (c *CmpInst) NumOperands() int { return 2 }
func (c *CmpInst) String() string   { return fmt.Sprintf("(%s %s %s)", c.X, c.Pred, c.Y) }

// FuncCallInst ...
// --------------------
type FuncCallInst struct {
	Op     Opcode
	Callee Operand
	Args   []Operand
}

func CreateFuncCall(callee Operand, args []Operand) *FuncCallInst {
	return &FuncCallInst{
		Op:     Call,
		Callee: callee,
		Args:   args,
	}
}

func (c FuncCallInst) expr() {}
func (c FuncCallInst) Operand(idx int) Operand {
	if idx < 1 || idx > c.NumOperands() {
		panic(fmt.Sprintf("[internal] invalid index '%d' for instruction '%s'", idx, c))
	}

	if idx == 1 {
		return c.Callee
	}

	return c.Args[idx-2]
}
func (c FuncCallInst) NumOperands() int { return 1 + len(c.Args) }
func (c FuncCallInst) String() string {
	var args []string
	for _, op := range c.Args {
		args = append(args, op.String())
	}

	return fmt.Sprintf("call %s(%s)", c.Callee, strings.Join(args, ", "))
}
