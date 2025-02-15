package meer

import (
	"fmt"
	"strings"
)

type Expression interface {
	expr()
	Type() Type
	SetType(Type)
	NumOperands() int
	Operand(i int) Expression
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
	Expression
}

type Operand interface {
	operand()
	fmt.Stringer
}

// Ident
// ----------------------------------------
type Ident struct {
	Id       string
	Ty       Type
	baseName string
}

func CreateIdent(id string, ty Type) *Ident { return &Ident{Id: id, baseName: id, Ty: ty} }

func (id *Ident) expr()            {}
func (id *Ident) operand()         {}
func (id *Ident) Type() Type       { return id.Ty }
func (id *Ident) SetType(t Type)   { id.Ty = t }
func (id *Ident) Name() string     { return id.Id }
func (id *Ident) BaseName() string { return id.baseName }
func (id *Ident) SetName(s string) { id.Id = s }
func (id *Ident) Operand(i int) Expression {
	switch i {
	case 1:
		return id
	default:
		panic("invalid operand number")
	}
}
func (id *Ident) NumOperands() int { return 1 }
func (id *Ident) String() string   { return fmt.Sprintf("%s", id.Id) }

// BinaryOp
// ----------------------------------------
type BinaryOp struct {
	Op   Opcode
	X, Y Expression
	Ty   Type
}

func CreateBinaryOp(op Opcode, x, y Expression, ty Type) *BinaryOp {
	return &BinaryOp{
		Op: op,
		X:  x,
		Y:  y,
		Ty: ty,
	}
}

func (*BinaryOp) expr()            {}
func (b *BinaryOp) Type() Type     { return b.Ty }
func (b *BinaryOp) SetType(t Type) { b.Ty = t }
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
	Ty Type
}

func CreateUnaryOp(op Opcode, x Expression, ty Type) *UnaryOp {
	return &UnaryOp{
		Op: op,
		X:  x,
		Ty: ty,
	}
}

func (*UnaryOp) expr()            {}
func (u *UnaryOp) Type() Type     { return u.Ty }
func (u *UnaryOp) SetType(t Type) { u.Ty = t }
func (u *UnaryOp) Operand(i int) Expression {
	switch i {
	case 1:
		return u.X
	default:
		panic("invalid operand number")
	}
}
func (u *UnaryOp) NumOperands() int { return 1 }
func (u *UnaryOp) String() string   { return fmt.Sprintf("%s %s", u.Op, u.X) }

// CmpInst
// -------------------------
type CmpInst struct {
	Pred Opcode
	X, Y Expression
	Ty   Type
}

func CreateCmpOp(pred Opcode, x, y Expression, ty Type) *CmpInst {
	return &CmpInst{
		Pred: pred,
		X:    x,
		Y:    y,
		Ty:   ty,
	}
}

func (*CmpInst) expr()            {}
func (c *CmpInst) Type() Type     { return c.Ty }
func (c *CmpInst) SetType(t Type) { c.Ty = t }
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
func (c *CmpInst) String() string   { return fmt.Sprintf("%s %s %s", c.X, c.Pred, c.Y) }

// FuncCallInst ...
// --------------------
type FuncCallInst struct {
	Op     Opcode
	Callee *Ident
	Args   []Expression
	Ty     Type
}

func CreateFuncCall(callee *Ident, args []Expression) *FuncCallInst {
	return &FuncCallInst{
		Op:     Call,
		Callee: callee,
		Args:   args,
	}
}

func (c FuncCallInst) expr()          {}
func (c FuncCallInst) Type() Type     { return c.Ty }
func (c FuncCallInst) SetType(t Type) { c.Ty = t }
func (c FuncCallInst) Operand(idx int) Expression {
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

// PHINode
// --------------------------
type PHINode struct {
	Op               Opcode
	Incoming         []PHINodeIncoming
	numIncomingPaths int
	ty               Type
}

func CreateEmptyPHINode() *PHINode {
	return &PHINode{Op: Phi}
}

func CreatePHINode(numIncomingPaths int) *PHINode {
	return &PHINode{
		Op:               Phi,
		numIncomingPaths: numIncomingPaths,
	}
}

func (phi *PHINode) expr() {}
func (phi *PHINode) Operand(i int) Expression {
	if i < 0 || i > phi.numIncomingPaths {
		return nil
	}

	return phi.Incoming[i-1].V
}
func (phi *PHINode) NumOperands() int { return phi.numIncomingPaths }
func (phi *PHINode) String() string {
	var incs []string
	for _, inc := range phi.Incoming {
		incs = append(incs, inc.String())
	}

	return fmt.Sprintf("phi %s", strings.Join(incs, ", "))
}
func (phi *PHINode) AddIncoming(v NamedOperand, blk *BasicBlock) {
	if phi.ty == nil {
		phi.ty = v.Type()
	}
	phi.Incoming = append(phi.Incoming, PHINodeIncoming{v, blk})
	phi.numIncomingPaths++
}
func (phi *PHINode) Type() Type     { return phi.ty }
func (phi *PHINode) SetType(t Type) { phi.ty = t }

type PHINodeIncoming struct {
	V   NamedOperand
	Blk *BasicBlock
}

func (p PHINodeIncoming) String() string {
	return fmt.Sprintf("[ %s, %%%s ]", p.V, p.Blk.Name())
}
