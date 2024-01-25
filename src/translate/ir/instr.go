package ir

import (
	"fmt"
	"strings"
)

type Instruction interface {
	Opcode() Opcode
	IsTerm() bool
	IsBinaryOp() bool
	IsMemOp() bool
	IsOtherOp() bool
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

func (c CallInstr) IsTerm() bool     { return termop_begin < c.op && c.op < termop_end }
func (c CallInstr) IsBinaryOp() bool { return binop_end < c.op && c.op < binop_end }
func (c CallInstr) IsOtherOp() bool  { return other_op_begin < c.op && c.op < other_op_end }
func (c CallInstr) IsMemOp() bool    { return memop_begin < c.op && c.op < memop_end }

func (c CallInstr) Opcode() Opcode { return c.op }

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
	cond Opcode
	x, y Operand
	res  Operand
}

func (c CmpInstr) IsTerm() bool     { return termop_begin < c.cond && c.cond < termop_end }
func (c CmpInstr) IsBinaryOp() bool { return binop_begin < c.cond && c.cond < binop_end }
func (c CmpInstr) IsOtherOp() bool  { return other_op_begin < c.cond && c.cond < other_op_end }
func (c CmpInstr) IsMemOp() bool    { return memop_begin < c.cond && c.cond < memop_end }

func (c CmpInstr) Opcode() Opcode { return c.cond }

func (c CmpInstr) String() string {
	return fmt.Sprintf("%s = cmp %s %s, %s", c.res, c.cond, c.x, c.y)
}

func CreateCmp(cond Opcode, left, right Operand, name string) *CmpInstr {
	if name == "" {
		// TODO Make next temp more global?
		// name = tmp

	}

	return &CmpInstr{cond: cond, x: left, y: right, res: Register{Name: name, OpKind: KRegister}}
}

// BinaryOp ...
// --------------------
type BinaryOp struct {
	op    Opcode
	left  Operand
	right Operand
	res   Operand
}

func (b BinaryOp) Opcode() Opcode { return b.op }

func (b BinaryOp) IsTerm() bool     { return termop_begin < b.op && b.op < termop_end }
func (b BinaryOp) IsBinaryOp() bool { return binop_begin < b.op && b.op < binop_end }
func (b BinaryOp) IsOtherOp() bool  { return other_op_begin < b.op && b.op < other_op_end }
func (b BinaryOp) IsMemOp() bool    { return memop_begin < b.op && b.op < memop_end }

func (b BinaryOp) String() string {
	return fmt.Sprintf("%s = %s %s, %s", b.res, b.op, b.left, b.right)
}

func CreateAdd(left, right Operand, name string) *BinaryOp {
	if name == "" {
		// TODO Make next temp more global?
		// name = tmp

	}

	return &BinaryOp{op: Add, left: left, right: right, res: Register{Name: name, OpKind: KRegister}}
}

// LoadInst ...
// ----------------
type LoadInst struct {
	op  Opcode
	src Operand
	res Operand
}

func (l LoadInst) Opcode() Opcode { return l.op }

func (l LoadInst) IsTerm() bool     { return termop_begin < l.op && l.op < termop_end }
func (l LoadInst) IsBinaryOp() bool { return binop_begin < l.op && l.op < binop_end }
func (l LoadInst) IsOtherOp() bool  { return other_op_begin < l.op && l.op < other_op_end }
func (l LoadInst) IsMemOp() bool    { return memop_begin < l.op && l.op < memop_end }

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

func (s StoreInst) Opcode() Opcode { return s.op }

func (s StoreInst) IsTerm() bool     { return termop_begin < s.op && s.op < termop_end }
func (s StoreInst) IsBinaryOp() bool { return binop_begin < s.op && s.op < binop_end }
func (s StoreInst) IsOtherOp() bool  { return other_op_begin < s.op && s.op < other_op_end }
func (s StoreInst) IsMemOp() bool    { return memop_begin < s.op && s.op < memop_end }

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
