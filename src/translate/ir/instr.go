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

	// Module() *Module
	// Parent() *BasicBlock
	// Function() *Function
	// InsertBefore(Instruction)
	// InsertAfter(Instruction)

	String() string
}

// CallInstr ...
// --------------------
type CallInstr struct {
	op     Opcode
	callee Value
	args   []Value
	fty    *FunctionType
	name   string
}

func (c CallInstr) Type() Type          { return c.fty }
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

	return fmt.Sprintf("%s = call %s %s(%s)", c.name, c.fty.retTy, c.callee.Name(), strings.Join(args, ", "))
}

func CreateCall(fty *FunctionType, callee Value, args []Value, name string) *CallInstr {
	if name == "" {
		name = NextTemp()
	}
	name = "%" + name

	return &CallInstr{Call, callee, args, fty, name}
}

// ICmpInstr ...
// --------------------------
type ICmpInstr struct {
	pred   Opcode
	x, y   Value
	evalTy Type
	opdTy  Type
	name   string
}

func (c ICmpInstr) Type() Type          { return c.evalTy }
func (c ICmpInstr) Name() string        { return c.name }
func (c ICmpInstr) SetName(name string) { c.name = name }
func (c ICmpInstr) HasName() bool       { return c.name != "" }
func (c ICmpInstr) IsTerm() bool        { return termop_begin < c.pred && c.pred < termop_end }
func (c ICmpInstr) IsBinaryOp() bool    { return binop_begin < c.pred && c.pred < binop_end }
func (c ICmpInstr) IsOtherOp() bool     { return other_op_begin < c.pred && c.pred < other_op_end }
func (c ICmpInstr) IsMemOp() bool       { return memop_begin < c.pred && c.pred < memop_end }
func (c ICmpInstr) Opcode() Opcode      { return c.pred }
func (c ICmpInstr) String() string {
	return fmt.Sprintf("%s = icmp %s %s %s, %s", c.name, c.pred, c.opdTy, c.x.Name(), c.y.Name())
}

func CreateICmp(ty Type, cond Opcode, x, y Value, name string) *ICmpInstr {
	if name == "" {
		name = NextTemp()
	}
	name = "%" + name

	return &ICmpInstr{cond, x, y, Int1Type, ty, name}
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
	ptr  Value
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
	srcStr := fmt.Sprintf("%s %s", l.ptr.Type(), l.ptr.Name())
	return fmt.Sprintf("%s = load %s, %s", l.name, l.typ, srcStr)
}

func CreateLoad(typ Type, src Value, name string) *LoadInst {
	if name == "" {
		name = NextTemp()
	}
	name = "%" + name

	return &LoadInst{typ: typ, op: Load, ptr: src, name: name}
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
	valueStr := fmt.Sprintf("%s %s", s.value.Type(), s.value.Name())
	dstStr := fmt.Sprintf("%s %s", s.dst.Type(), s.dst.Name())

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
	if name == "" {
		name = NextTemp()
	}
	name = "%" + name

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
func (a AllocaInst) Name() string        { return a.name }
func (a AllocaInst) SetName(name string) { a.name = name }
func (a AllocaInst) HasName() bool       { return a.name != "" }
func (a AllocaInst) String() string {
	s := fmt.Sprintf("%s = alloca %s", a.name, a.allocTy)
	if a.numElem > 1 {
		s += fmt.Sprintf(", i32 %d", a.numElem)
	}

	if a.align > 0 && a.align < (1<<32) {
		s += fmt.Sprintf(", align %d", a.align)
	}

	return s
}
func (a AllocaInst) AllocatedTy() Type { return a.allocTy }

// ReturnInst ...
// ---------------------
type ReturnInst struct {
	op    Opcode
	ty    Type
	value Value
}

func CreateRet(ty Type, value Value) *ReturnInst {
	return &ReturnInst{
		op:    Ret,
		ty:    ty,
		value: value,
	}
}

func CreateRetVoid() *ReturnInst {
	return &ReturnInst{
		op: Ret,
		ty: VoidType,
	}
}

func (r ReturnInst) Type() Type       { return r.ty }
func (r ReturnInst) Name() string     { return "" }
func (r ReturnInst) SetName(s string) {}
func (r ReturnInst) HasName() bool    { return false }
func (r ReturnInst) String() string {
	s := fmt.Sprintf("%s %s", r.op, r.ty)
	if r.value != nil {
		s += " " + r.value.Name()
	}

	return s
}
func (r ReturnInst) Opcode() Opcode   { return r.op }
func (r ReturnInst) IsTerm() bool     { return termop_begin < r.op && r.op < termop_end }
func (r ReturnInst) IsBinaryOp() bool { return binop_begin < r.op && r.op < binop_end }
func (r ReturnInst) IsMemOp() bool    { return memop_begin < r.op && r.op < memop_end }
func (r ReturnInst) IsOtherOp() bool  { return other_op_begin < r.op && r.op < other_op_end }

// BranchInst ...
// ---------------------
type BranchInst struct {
	op      Opcode
	cond    Value
	ifTrue  *BasicBlock
	ifFalse *BasicBlock

	numSucc int
}

func CreateCondBrInst(cond Value, ifTrue, ifFalse *BasicBlock) *BranchInst {
	return &BranchInst{
		Br,
		cond,
		ifTrue,
		ifFalse,
		2,
	}
}

func CreateBr(dst *BasicBlock) *BranchInst {
	return &BranchInst{
		Br,
		nil,
		dst,
		nil,
		1,
	}
}

func (b BranchInst) Type() Type               { return nil }
func (b BranchInst) Name() string             { return "" }
func (b BranchInst) SetName(string)           {}
func (b BranchInst) HasName() bool            { return false }
func (b BranchInst) Opcode() Opcode           { return b.op }
func (b BranchInst) IsTerm() bool             { return termop_begin < b.op && b.op < termop_end }
func (b BranchInst) IsBinaryOp() bool         { return binop_begin < b.op && b.op < binop_end }
func (b BranchInst) IsMemOp() bool            { return memop_begin < b.op && b.op < memop_end }
func (b BranchInst) IsOtherOp() bool          { return other_op_begin < b.op && b.op < other_op_end }
func (b BranchInst) NumSuccessors() int       { return b.numSucc }
func (b BranchInst) IsConditional() bool      { return b.cond != nil && b.numSucc == 2 }
func (b BranchInst) SetSuccessor(*BasicBlock) {}
func (b BranchInst) SetCond(cond Value)       { b.cond = cond }
func (b BranchInst) Cond() Value              { return b.cond }
func (b BranchInst) String() string {
	var s string

	if b.IsConditional() {
		s = fmt.Sprintf("br %s %s, label %%%s, label %%%s", b.cond.Type(), b.cond.Name(), b.ifTrue.name, b.ifFalse.name)
	} else {
		s = fmt.Sprintf("br label %%%s", b.ifTrue.name)
	}

	return s
}
