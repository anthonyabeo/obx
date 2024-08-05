package tacil

import (
	"fmt"
	"strconv"
	"strings"
)

// ConstantBool ...
// --------------------
type ConstantBool struct {
	Value int
	Ty    *Int1
}

func NewConstBool(value int) *ConstantBool {
	return &ConstantBool{
		Value: value,
		Ty:    Int1Type,
	}
}
func (c ConstantBool) expr() {}
func (c ConstantBool) Name() string {
	if c.Value != 1 && c.Value != 0 {
		panic("value of boolean type must be 0 or 1")
	}

	if c.Value == 1 {
		return "true"
	}

	return "false"
}
func (c ConstantBool) BaseName() string {
	if c.Value != 1 && c.Value != 0 {
		panic("value of boolean type must be 0 or 1")
	}

	if c.Value == 1 {
		return "true"
	}

	return "false"
}
func (c ConstantBool) SetName(string) { panic("implement me") }
func (c ConstantBool) HasName() bool  { return false }
func (c ConstantBool) Operand(i int) Expr {
	if i != 1 {
		panic("operand index of ConstantBool must be 1")
	}

	return c
}
func (c ConstantBool) NumOperands() int { return 1 }
func (c ConstantBool) Type() Type       { return c.Ty }
func (c ConstantBool) String() string {
	//TODO implement me
	panic("implement me")
}

// ConstantInt ...
// --------------------
type ConstantInt struct {
	Value  uint64
	Signed bool
	Ty     Type
}

func NewConstantInt(ty Type, value uint64, signed bool) *ConstantInt {
	return &ConstantInt{value, signed, ty}
}

func (c ConstantInt) expr()            {}
func (c ConstantInt) Name() string     { return strconv.Itoa(int(c.Value)) }
func (c ConstantInt) BaseName() string { return strconv.Itoa(int(c.Value)) }
func (c ConstantInt) SetName(string)   {}
func (c ConstantInt) HasName() bool    { return false }
func (c ConstantInt) Operand(i int) Expr {
	if i != 1 {
		panic("operand index of Temp must be 1")
	}

	return c
}
func (c ConstantInt) NumOperands() int { return 1 }
func (c ConstantInt) Type() Type       { return c.Ty }
func (c ConstantInt) String() string   { return fmt.Sprintf("%d", c.Value) }

// Temp
// --------------------------------------
type Temp struct {
	name     string
	baseName string
	ty       Type
}

func NewTemp(name string, ty Type) *Temp {
	return &Temp{
		name:     name,
		baseName: name,
		ty:       ty,
	}
}

func (t *Temp) expr()            {}
func (t *Temp) Name() string     { return t.name }
func (t *Temp) BaseName() string { return t.baseName }
func (t *Temp) SetName(s string) { t.name = s }
func (t *Temp) HasName() bool    { return true }
func (t *Temp) NumOperands() int { return 1 }
func (t *Temp) Operand(i int) Expr {
	if i != 1 {
		panic("operand index of Temp must be 1")
	}

	return t
}
func (t *Temp) String() string { return t.name }
func (t *Temp) Type() Type     { return t.ty }

// BinaryOp
// ----------------------------------------
type BinaryOp struct {
	Op   Opcode
	X, Y Expr
	ty   Type
}

func NewBinaryOp(ty Type, op Opcode, x, y Expr) *BinaryOp {
	return &BinaryOp{
		Op: op,
		X:  x,
		Y:  y,
		ty: ty,
	}
}

func (*BinaryOp) expr()              {}
func (b *BinaryOp) Name() string     { panic("") }
func (b *BinaryOp) BaseName() string { panic("") }
func (b *BinaryOp) SetName(string)   {}
func (b *BinaryOp) HasName() bool    { return false }
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
func (b *BinaryOp) Type() Type       { return b.ty }
func (b *BinaryOp) String() string   { return fmt.Sprintf("%s %s %s", b.X, b.Op, b.Y) }

// Cmp
// -------------------------
type Cmp struct {
	Pred Opcode
	X, Y Expr
	ty   Type
}

func CreateCmp(pred Opcode, x, y Expr) *Cmp {
	return &Cmp{
		Pred: pred,
		X:    x,
		Y:    y,
	}
}

func (*Cmp) expr()              {}
func (c *Cmp) Name() string     { return "" }
func (c *Cmp) BaseName() string { return "" }
func (c *Cmp) SetName(string)   {}
func (c *Cmp) HasName() bool    { return false }
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
func (c *Cmp) Type() Type       { return c.ty }
func (c *Cmp) String() string   { return fmt.Sprintf("(%s %s %s)", c.X, c.Pred, c.Y) }

// PHINode
// --------------------------
type PHINode struct {
	Op               Opcode
	Incoming         []PHINodeIncoming
	numIncomingPaths uint
	ty               Type
}

func CreateEmptyPHINode() *PHINode {
	return &PHINode{Op: Phi}
}

func CreatePHINode(numIncomingPaths uint) *PHINode {
	return &PHINode{
		Op:               Phi,
		numIncomingPaths: numIncomingPaths,
	}
}

func (phi *PHINode) expr()            {}
func (phi *PHINode) Name() string     { panic("phi nodes have no names") }
func (phi *PHINode) BaseName() string { panic("phi nodes have no names") }
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
	if phi.ty == nil {
		phi.ty = v.Type()
	}
	phi.Incoming = append(phi.Incoming, PHINodeIncoming{v, blk})
	phi.numIncomingPaths++
}
func (phi *PHINode) Type() Type { return phi.ty }

type PHINodeIncoming struct {
	V   Expr
	Blk *BasicBlock
}

func (p PHINodeIncoming) String() string {
	return fmt.Sprintf("[ %s, %%%s ]", p.V, p.Blk.Name())
}

// FuncCall ...
// --------------------
type FuncCall struct {
	Op     Opcode
	Callee Expr
	Args   []Expr
	ty     Type
}

func CreateFuncCall(callee Expr, args []Expr) *FuncCall {
	return &FuncCall{
		Op:     Call,
		Callee: callee,
		Args:   args,
	}
}

func (c FuncCall) expr() {}
func (c FuncCall) Operand(idx int) Expr {
	if idx < 1 || idx > c.NumOperands() {
		panic(fmt.Sprintf("[internal] invalid index '%d' for instruction '%s'", idx, c))
	}

	if idx == 1 {
		return c.Callee
	}

	return c.Args[idx-2]
}
func (c FuncCall) Name() string     { return c.Callee.Name() }
func (c FuncCall) BaseName() string { return c.Callee.Name() }
func (c FuncCall) SetName(string)   {}
func (c FuncCall) HasName() bool    { panic("not implemented") }
func (c FuncCall) NumOperands() int { return 1 + len(c.Args) }
func (c FuncCall) Type() Type       { return c.ty }
func (c FuncCall) String() string {
	var args []string
	for _, op := range c.Args {
		args = append(args, op.Name())
	}

	return fmt.Sprintf("%s = call %s(%s)", c.Name(), c.Callee.Name(), strings.Join(args, ", "))
}

// Load
// --------------------------
type Load struct {
	Op   Opcode
	Addr Expr
}

func CreateLoad(addr Expr) *Load {
	return &Load{
		Op:   Ld,
		Addr: addr,
	}
}

func (l Load) expr()            {}
func (l Load) Name() string     { panic("implement me") }
func (l Load) BaseName() string { panic("implement me") }
func (l Load) SetName(string)   { panic("implement me") }
func (l Load) HasName() bool    { return false }
func (l Load) Operand(i int) Expr {
	if i != 1 {
		panic(fmt.Sprintf("invalid index '%d' for load operand", i))
	}

	return l.Addr
}
func (l Load) NumOperands() int { return 1 }
func (l Load) Type() Type       { return l.Addr.Type() }
func (l Load) String() string   { return fmt.Sprintf("%s %s", l.Op, l.Addr) }
