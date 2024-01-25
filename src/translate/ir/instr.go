package ir

import (
	"fmt"
	"strings"
)

type Instruction interface {
	Opcode() Opcode
	IsTerm() bool
	IsBinaryOp() bool
	String() string
}

// CallInstr ...
// --------------------
type CallInstr struct {
	op     Opcode
	Proc   string
	Args   []Operand
	Result Operand
}

func (c CallInstr) IsTerm() bool {
	return termop_begin < c.op && c.op < termop_end
}

func (c CallInstr) IsBinaryOp() bool {
	return binop_end < c.op && c.op < binop_end
}

func (c CallInstr) Opcode() Opcode {
	return c.op
}

func (c CallInstr) String() string {
	var args []string
	for _, op := range c.Args {
		args = append(args, op.String())
	}

	return fmt.Sprintf("%s = call %s(%s)", c.Result, c.Proc, strings.Join(args, ", "))
}

// CmpInstr ...
// --------------------------
type CmpInstr struct {
	Cond   Opcode
	X, Y   Operand
	Result Operand
}

func (c CmpInstr) IsTerm() bool {
	return termop_begin < c.Cond && c.Cond < termop_end
}

func (c CmpInstr) IsBinaryOp() bool {
	return binop_begin < c.Cond && c.Cond < binop_end
}

func (c CmpInstr) Opcode() Opcode { return c.Cond }

func (c CmpInstr) String() string {
	return fmt.Sprintf("%s = cmp %s %s, %s", c.Result, c.Cond, c.X, c.Y)
}

func CreateCmp(cond Opcode, left, right Operand, name string) *CmpInstr {
	if name == "" {
		// TODO Make next temp more global?
		// name = tmp

	}

	return &CmpInstr{Cond: cond, X: left, Y: right, Result: Register{Name: name, OpKind: KRegister}}
}

// BinaryOp ...
// --------------------
type BinaryOp struct {
	op     Opcode
	Left   Operand
	Right  Operand
	Result Operand
}

func (b BinaryOp) Opcode() Opcode {
	return b.op
}

func (b BinaryOp) IsTerm() bool {
	return termop_begin < b.op && b.op < termop_end
}

func (b BinaryOp) IsBinaryOp() bool {
	return binop_begin < b.op && b.op < binop_end
}

func (b BinaryOp) String() string {
	return fmt.Sprintf("%s = %s %s, %s", b.Result, b.op, b.Left, b.Right)
}

func CreateAdd(left, right Operand, name string) *BinaryOp {
	if name == "" {
		// TODO Make next temp more global?
		// name = tmp

	}

	return &BinaryOp{op: Add, Left: left, Right: right, Result: Register{Name: name, OpKind: KRegister}}
}

// LoadInst ...
// ----------------
type LoadInst struct {
	op  Opcode
	src Operand
	res Operand
}

func (l LoadInst) Opcode() Opcode {
	return l.op
}

func (l LoadInst) IsTerm() bool {
	return termop_begin < l.op && l.op < termop_end
}

func (l LoadInst) IsBinaryOp() bool {
	return binop_begin < l.op && l.op < binop_end
}

func (l LoadInst) String() string {
	return fmt.Sprintf("%s = load %s", l.res, l.src)
}

func CreateLoad(src Operand, name string) *LoadInst {
	if name == "" {
		// TODO Make next temp more global?
		// name = tmp
	}

	return &LoadInst{op: Load, src: src, res: Register{Name: name, OpKind: KRegister}}
}

// StoreInst ...
// -------------------
type StoreInst struct {
	op    Opcode
	value Operand
	dst   Operand
}

func (s StoreInst) Opcode() Opcode {
	//TODO implement me
	panic("implement me")
}

func (s StoreInst) IsTerm() bool {
	return termop_begin < s.op && s.op < termop_end
}

func (s StoreInst) IsBinaryOp() bool {
	return binop_begin < s.op && s.op < binop_end
}

func (s StoreInst) String() string {
	return fmt.Sprintf("store %s, %s", s.value, s.dst)
}

func CreateStore(value, dst Operand) *StoreInst {
	return &StoreInst{
		op:    Store,
		value: value,
		dst:   dst,
	}
}
