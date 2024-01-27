package ir

import (
	"fmt"
	"strings"
)

type Instruction interface {
	Value
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
	op   Opcode
	proc string
	args []Value
	ty   Type
	name string
}

func (c CallInstr) Type() Type          { return c.ty }
func (c CallInstr) Name() string        { return c.name }
func (c CallInstr) SetName(name string) { c.name = name }
func (c CallInstr) HasName() bool       { return c.name != "" }
func (c CallInstr) IsTerm() bool        { return termop_begin < c.op && c.op < termop_end }
func (c CallInstr) IsBinaryOp() bool    { return binop_end < c.op && c.op < binop_end }
func (c CallInstr) IsOtherOp() bool     { return other_op_begin < c.op && c.op < other_op_end }
func (c CallInstr) IsMemOp() bool       { return memop_begin < c.op && c.op < memop_end }
func (c CallInstr) Opcode() Opcode      { return c.op }

func (c CallInstr) String() string {
	var args []string
	for _, op := range c.args {
		args = append(args, fmt.Sprintf("%s %s", op.Type(), op.Name()))
	}

	return fmt.Sprintf("%s = call %s %s(%s)", c.name, c.ty, c.proc, strings.Join(args, ", "))
}

func CreateCall(typ Type, fun string, args []Value, name string) *CallInstr {
	if name == "" {
		name = NextTemp()
	}

	name = "%" + name
	return &CallInstr{ty: typ, proc: fun, args: args, name: name}
}

// CmpInstr ...
// --------------------------
type CmpInstr struct {
	cond Opcode
	x, y Value
	ty   Type
	name string
}

func (c CmpInstr) Type() Type          { return c.ty }
func (c CmpInstr) Name() string        { return c.name }
func (c CmpInstr) SetName(name string) { c.name = name }
func (c CmpInstr) HasName() bool       { return c.name != "" }
func (c CmpInstr) IsTerm() bool        { return termop_begin < c.cond && c.cond < termop_end }
func (c CmpInstr) IsBinaryOp() bool    { return binop_begin < c.cond && c.cond < binop_end }
func (c CmpInstr) IsOtherOp() bool     { return other_op_begin < c.cond && c.cond < other_op_end }
func (c CmpInstr) IsMemOp() bool       { return memop_begin < c.cond && c.cond < memop_end }

func (c CmpInstr) Opcode() Opcode { return c.cond }

func (c CmpInstr) String() string {
	return fmt.Sprintf("%s = cmp %s %s %s, %s", c.name, c.cond, c.ty, c.x.Name(), c.y.Name())
}

func CreateCmp(ty Type, cond Opcode, x, y Value, name string) *CmpInstr {
	if name == "" {
		name = NextTemp()
	}

	name = "%" + name

	return &CmpInstr{cond, x, y, ty, name}
}

// BinaryOp ...
// --------------------
type BinaryOp struct {
	op     Opcode
	left   Value
	right  Value
	evalTy Type
	name   string
}

func (b BinaryOp) Type() Type          { return b.evalTy }
func (b BinaryOp) Name() string        { return b.name }
func (b BinaryOp) SetName(name string) { b.name = name }
func (b BinaryOp) HasName() bool       { return b.name != "" }
func (b BinaryOp) Opcode() Opcode      { return b.op }
func (b BinaryOp) IsTerm() bool        { return termop_begin < b.op && b.op < termop_end }
func (b BinaryOp) IsBinaryOp() bool    { return binop_begin < b.op && b.op < binop_end }
func (b BinaryOp) IsOtherOp() bool     { return other_op_begin < b.op && b.op < other_op_end }
func (b BinaryOp) IsMemOp() bool       { return memop_begin < b.op && b.op < memop_end }
func (b BinaryOp) String() string {
	return fmt.Sprintf("%s = %s %s %s, %s", b.name, b.op, b.evalTy, b.left.Name(), b.right.Name())
}

func CreateAdd(ty Type, left, right Value, name string) *BinaryOp {
	if name == "" {
		name = NextTemp()
	}

	name = "%" + name

	return &BinaryOp{evalTy: ty, op: Add, left: left, right: right, name: name}
}

// LoadInst ...
// ----------------
type LoadInst struct {
	op   Opcode
	src  Value
	typ  Type
	name string
}

func (l LoadInst) Type() Type          { return l.typ }
func (l LoadInst) Name() string        { return l.name }
func (l LoadInst) SetName(name string) { l.name = name }
func (l LoadInst) HasName() bool       { return l.name != "" }
func (l LoadInst) Opcode() Opcode      { return l.op }
func (l LoadInst) IsTerm() bool        { return termop_begin < l.op && l.op < termop_end }
func (l LoadInst) IsBinaryOp() bool    { return binop_begin < l.op && l.op < binop_end }
func (l LoadInst) IsOtherOp() bool     { return other_op_begin < l.op && l.op < other_op_end }
func (l LoadInst) IsMemOp() bool       { return memop_begin < l.op && l.op < memop_end }

func (l LoadInst) String() string {
	var srcStr string

	if l.src.HasName() {
		srcStr = fmt.Sprintf("%s %s", l.src.Type(), l.src.Name())
	} else {
		srcStr = fmt.Sprintf("%s %s", l.src.Type(), "%"+NextTemp())
	}

	return fmt.Sprintf("%s = load %s, %s", l.name, l.typ, srcStr)
}

func CreateLoad(typ Type, src Value, name string) *LoadInst {
	return &LoadInst{typ: typ, op: Load, src: src, name: name}
}

// StoreInst ...
// -------------------
type StoreInst struct {
	op    Opcode
	value Value
	dst   Value
}

func (s StoreInst) Type() Type       { return nil }
func (s StoreInst) Name() string     { return "" }
func (s StoreInst) SetName(string)   {}
func (s StoreInst) HasName() bool    { return false }
func (s StoreInst) Opcode() Opcode   { return s.op }
func (s StoreInst) IsTerm() bool     { return termop_begin < s.op && s.op < termop_end }
func (s StoreInst) IsBinaryOp() bool { return binop_begin < s.op && s.op < binop_end }
func (s StoreInst) IsOtherOp() bool  { return other_op_begin < s.op && s.op < other_op_end }
func (s StoreInst) IsMemOp() bool    { return memop_begin < s.op && s.op < memop_end }

func (s StoreInst) String() string {
	var (
		valueStr string
		dstStr   string
	)

	tmp := NextTemp()

	if s.value.HasName() {
		valueStr = fmt.Sprintf("%s %s", s.value.Type(), s.value.Name())
	} else {
		valueStr = fmt.Sprintf("%s %%%s", s.value.Type(), tmp)
	}

	if s.dst.HasName() {
		dstStr = fmt.Sprintf("%s %s", s.dst.Type(), s.dst.Name())
	} else {
		dstStr = fmt.Sprintf("%s %%%s", s.dst.Type(), tmp)
	}

	return fmt.Sprintf("store %s, %s", valueStr, dstStr)
}

func CreateStore(value, dst Value) *StoreInst {
	return &StoreInst{
		op:    Store,
		value: value,
		dst:   dst,
	}
}

// AllocaInst ...
// ---------------------
type AllocaInst struct {
	op      Opcode
	numElem int
	align   int
	allocTy Type
	evalTy  Type
	name    string
}

func CreateAlloca(ty Type, numElems int, align int, name string) *AllocaInst {
	alloc := &AllocaInst{op: Alloca, allocTy: ty, numElem: 1, name: name, evalTy: CreatePointerType(ty)}
	if numElems > 1 {
		alloc.numElem = numElems
	}

	if align > (1 << 32) {
		alloc.align = 1 << 32
	}

	return alloc
}

func (a AllocaInst) Opcode() Opcode      { return a.op }
func (a AllocaInst) IsTerm() bool        { return termop_begin < a.op && a.op < termop_end }
func (a AllocaInst) IsBinaryOp() bool    { return binop_begin < a.op && a.op < binop_end }
func (a AllocaInst) IsMemOp() bool       { return memop_begin < a.op && a.op < memop_end }
func (a AllocaInst) IsOtherOp() bool     { return other_op_begin < a.op && a.op < other_op_end }
func (a AllocaInst) Type() Type          { return a.evalTy }
func (a AllocaInst) Name() string        { return "%" + a.name }
func (a AllocaInst) SetName(name string) { a.name = name }
func (a AllocaInst) HasName() bool       { return a.name != "" }
func (a AllocaInst) String() string {
	s := fmt.Sprintf("%%%s = alloca %s", a.name, a.allocTy)
	if a.numElem > 1 {
		s += fmt.Sprintf(", i32 %d", a.numElem)
	}

	if a.align > 0 && a.align < (1<<32) {
		s += fmt.Sprintf(", align %d", a.align)
	}

	return s
}
