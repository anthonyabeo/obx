package tacil

import (
	"container/list"
	"fmt"
	"strings"

	"strconv"
)

// ConstantInt ...
// --------------------
type ConstantInt struct {
	value  uint64
	signed bool
	ty     Type
}

func NewConstantInt(ty Type, value uint64, signed bool, name string) *ConstantInt {
	if name == "" {
		name = strconv.Itoa(int(value))
	}

	uses := list.New()
	uses.Init()

	return &ConstantInt{value, signed, ty}
}

func (c ConstantInt) expr()              {}
func (c ConstantInt) Name() string       { return strconv.Itoa(int(c.value)) }
func (c ConstantInt) SetName(string)     {}
func (c ConstantInt) HasName() bool      { return false }
func (c ConstantInt) Operand(i int) Expr { panic("constants have no operands") }
func (c ConstantInt) NumOperands() int   { return 0 }
func (c ConstantInt) String() string     { return fmt.Sprintf("%s %d", c.ty, c.value) }

// Temp
// --------------------------------------
type Temp struct {
	name string
}

func NewTemp(name string) *Temp {
	return &Temp{
		name: name,
	}
}

func (t *Temp) expr()              {}
func (t *Temp) Name() string       { return t.name }
func (t *Temp) SetName(s string)   { t.name = s }
func (t *Temp) HasName() bool      { return true }
func (t *Temp) NumOperands() int   { return 0 }
func (t *Temp) Operand(i int) Expr { panic("temp has no operands") }
func (t *Temp) String() string     { return t.name }
func (t *Temp) Type() Type         { panic("") }

// BinaryOp
// ----------------------------------------
type BinaryOp struct {
	Op   Opcode
	X, Y Expr
}

func NewBinaryOp(op Opcode, x, y Expr) *BinaryOp {
	return &BinaryOp{
		Op: op,
		X:  x,
		Y:  y,
	}
}

func (*BinaryOp) expr()            {}
func (b *BinaryOp) Name() string   { panic("") }
func (b *BinaryOp) SetName(string) {}
func (b *BinaryOp) HasName() bool  { return false }
func (b *BinaryOp) Operand(i int) Expr {
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
func (b *BinaryOp) String() string   { return fmt.Sprintf("%s %s, %s", b.Op, b.X, b.Y) }

// Cmp
// -------------------------
type Cmp struct {
	Pred Opcode
	X, Y Expr
}

func CreateCmp(pred Opcode, x, y Expr) *Cmp {
	return &Cmp{
		Pred: pred,
		X:    x,
		Y:    y,
	}
}

func (*Cmp) expr()            {}
func (c *Cmp) Name() string   { return "" }
func (c *Cmp) SetName(string) {}
func (c *Cmp) HasName() bool  { return false }
func (c *Cmp) Operand(i int) Expr {
	switch i {
	case 1:
		return c.X
	case 2:
		return c.Y
	default:
		panic(fmt.Sprintf("invalid operand number"))
	}
}
func (c *Cmp) NumOperands() int { return 2 }
func (c *Cmp) String() string   { return fmt.Sprintf("icmp %s %s, %s", c.Pred, c.X, c.Y) }

// PHINode
// --------------------------
type PHINode struct {
	Op               Opcode
	Incoming         []PHINodeIncoming
	numIncomingPaths uint
}

func CreateEmptyPHINode() *PHINode {
	return &PHINode{Op: Phi}
}

func CreatePHINode(ty Type, numIncomingPaths uint, name string) *PHINode {
	uses := list.New()
	uses.Init()

	return &PHINode{
		Op:               Phi,
		numIncomingPaths: numIncomingPaths,
	}
}

func (phi *PHINode) expr()            {}
func (phi *PHINode) Name() string     { panic("phi nodes have no names") }
func (phi *PHINode) SetName(string)   {}
func (phi *PHINode) HasName() bool    { return false }
func (phi *PHINode) Operand(int) Expr { panic("") }
func (phi *PHINode) NumOperands() int { return 0 }
func (phi *PHINode) String() string {
	var incs []string
	for _, inc := range phi.Incoming {
		incs = append(incs, inc.String())
	}

	return fmt.Sprintf("phi %s", strings.Join(incs, ", "))
}
func (phi *PHINode) AddIncoming(v Expr, blk *BasicBlock) {
	phi.Incoming = append(phi.Incoming, PHINodeIncoming{v, blk})
	phi.numIncomingPaths++
}

type PHINodeIncoming struct {
	V   Expr
	Blk *BasicBlock
}

func (p PHINodeIncoming) String() string {
	return fmt.Sprintf("[ %s, %%%s ]", p.V, p.Blk.Name())
}

// FuncCallInstr ...
// --------------------
type FuncCallInstr struct {
	Op     Opcode
	Callee Expr
	Args   []Expr
}

func CreateFuncCall(callee Expr, args []Expr) *FuncCallInstr {
	return &FuncCallInstr{
		Op:     Call,
		Callee: callee,
		Args:   args,
	}
}

func (c FuncCallInstr) expr() {}
func (c FuncCallInstr) Operand(idx int) Expr {
	if idx < 1 || idx > c.NumOperands() {
		panic(fmt.Sprintf("[internal] invalid index '%d' for instruction '%s'", idx, c))
	}

	if idx == 1 {
		return c.Callee
	}

	return c.Args[idx-2]
}
func (c FuncCallInstr) Name() string     { return c.Callee.Name() }
func (c FuncCallInstr) SetName(string)   {}
func (c FuncCallInstr) HasName() bool    { panic("not implemented") }
func (c FuncCallInstr) NumOperands() int { return 1 + len(c.Args) }
func (c FuncCallInstr) String() string {
	var args []string
	for _, op := range c.Args {
		args = append(args, op.Name())
	}

	return fmt.Sprintf("%s = call %s(%s)", c.Name(), c.Callee.Name(), strings.Join(args, ", "))
}
