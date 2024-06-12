package ir

import (
	"container/list"
	"fmt"
	"strings"
)

type Instruction interface {
	User
	Opcode() Opcode

	IsTerm() bool
	IsBinaryOp() bool
	IsMemOp() bool
	IsOtherOp() bool
	IsBitBinOp() bool
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

	useList *list.List
}

func (c CallInstr) NumUses() int { return c.useList.Len() }
func (c CallInstr) NumOperands() int {
	//TODO implement me
	panic("implement me")
}
func (c CallInstr) Operand(i int) Value {
	//TODO implement me
	panic("implement me")
}
func (c CallInstr) OperandList() *list.List { return c.useList }
func (c CallInstr) Type() Type              { return c.fty }
func (c CallInstr) Name() string            { return "%" + c.name }
func (c CallInstr) SetName(name string)     { c.name = name }
func (c CallInstr) HasName() bool           { return c.name != "" }
func (c CallInstr) IsTerm() bool            { return termop_begin < c.op && c.op < termop_end }
func (c CallInstr) IsBinaryOp() bool        { return binop_end < c.op && c.op < binop_end }
func (c CallInstr) IsOtherOp() bool         { return other_op_begin < c.op && c.op < other_op_end }
func (c CallInstr) IsMemOp() bool           { return memop_begin < c.op && c.op < memop_end }
func (c CallInstr) IsBitBinOp() bool        { return bit_binop_begin < c.op && c.op < bit_binop_end }
func (c CallInstr) Opcode() Opcode          { return c.op }
func (c CallInstr) String() string {
	var args []string
	for _, op := range c.args {
		args = append(args, fmt.Sprintf("%s %s", op.Type(), op.Name()))
	}

	return fmt.Sprintf("%s = call %s %s(%s)", c.Name(), c.fty.retTy, c.callee.Name(), strings.Join(args, ", "))
}

func CreateCall(fty *FunctionType, callee Value, args []Value, name string) *CallInstr {
	if name == "" {
		name = NextTemp()
	}

	uses := list.New()
	uses.Init()

	return &CallInstr{Call, callee, args, fty, name, uses}
}

// ICmpInstr ...
// --------------------------
type ICmpInstr struct {
	pred   Opcode
	x, y   Value
	evalTy Type
	opdTy  Type
	name   string

	useList *list.List
}

func (c ICmpInstr) NumUses() int     { return c.useList.Len() }
func (c ICmpInstr) NumOperands() int { return 2 }
func (c ICmpInstr) Operand(idx int) Value {
	if idx < 1 || idx > 2 {
		panic(fmt.Sprintf("[internal] invalid index '%d' for instruction '%s'", idx, c))
	}

	if idx == 1 {
		return c.x
	}

	return c.y
}
func (c ICmpInstr) OperandList() *list.List { return c.useList }
func (c ICmpInstr) Type() Type              { return c.evalTy }
func (c ICmpInstr) Name() string            { return "%" + c.name }
func (c ICmpInstr) SetName(name string)     { c.name = name }
func (c ICmpInstr) HasName() bool           { return c.name != "" }
func (c ICmpInstr) IsTerm() bool            { return termop_begin < c.pred && c.pred < termop_end }
func (c ICmpInstr) IsBinaryOp() bool        { return binop_begin < c.pred && c.pred < binop_end }
func (c ICmpInstr) IsOtherOp() bool         { return other_op_begin < c.pred && c.pred < other_op_end }
func (c ICmpInstr) IsMemOp() bool           { return memop_begin < c.pred && c.pred < memop_end }
func (c ICmpInstr) IsBitBinOp() bool        { return bit_binop_begin < c.pred && c.pred < bit_binop_end }
func (c ICmpInstr) Opcode() Opcode          { return c.pred }
func (c ICmpInstr) String() string {
	return fmt.Sprintf("%s = icmp %s %s %s, %s", c.Name(), c.pred, c.opdTy, c.x.Name(), c.y.Name())
}

func CreateICmp(ty Type, cond Opcode, x, y Value, name string) *ICmpInstr {
	if name == "" {
		name = NextTemp()
	}

	uses := list.New()
	uses.Init()

	return &ICmpInstr{cond, x, y, Int1Type, ty, name, uses}
}

// BinaryOp ...
// --------------------
type BinaryOp struct {
	op     Opcode
	left   Value
	right  Value
	evalTy Type
	name   string

	useList *list.List
}

func (b BinaryOp) NumUses() int            { return b.useList.Len() }
func (b BinaryOp) OperandList() *list.List { return b.useList }
func (b BinaryOp) NumOperands() int        { return 2 }
func (b BinaryOp) Operand(idx int) Value {
	if idx < 1 || idx > 2 {
		panic(fmt.Sprintf("[internal] invalid index '%d' for instruction '%s'", idx, b))
	}

	if idx == 1 {
		return b.left
	}

	return b.right
}
func (b BinaryOp) Type() Type          { return b.evalTy }
func (b BinaryOp) Name() string        { return "%" + b.name }
func (b BinaryOp) SetName(name string) { b.name = name }
func (b BinaryOp) HasName() bool       { return b.name != "" }
func (b BinaryOp) Opcode() Opcode      { return b.op }
func (b BinaryOp) IsTerm() bool        { return termop_begin < b.op && b.op < termop_end }
func (b BinaryOp) IsBinaryOp() bool    { return binop_begin < b.op && b.op < binop_end }
func (b BinaryOp) IsOtherOp() bool     { return other_op_begin < b.op && b.op < other_op_end }
func (b BinaryOp) IsMemOp() bool       { return memop_begin < b.op && b.op < memop_end }
func (b BinaryOp) IsBitBinOp() bool    { return bit_binop_begin < b.op && b.op < bit_binop_end }
func (b BinaryOp) String() string {
	return fmt.Sprintf("%s = %s %s %s, %s", b.Name(), b.op, b.evalTy, b.left.Name(), b.right.Name())
}

func CreateAdd(ty Type, left, right Value, name string) *BinaryOp {
	if name == "" {
		name = NextTemp()
	}

	uses := list.New()
	uses.Init()

	return &BinaryOp{evalTy: ty, op: Add, left: left, right: right, name: name, useList: uses}
}

func CreateSub(ty Type, left, right Value, name string) *BinaryOp {
	if name == "" {
		name = NextTemp()
	}

	uses := list.New()
	uses.Init()

	return &BinaryOp{evalTy: ty, op: Sub, left: left, right: right, name: name, useList: uses}
}

func CreateXOR(ty Type, left, right Value, name string) *BinaryOp {
	if name == "" {
		name = NextTemp()
	}

	uses := list.New()
	uses.Init()

	return &BinaryOp{evalTy: ty, op: Xor, left: left, right: right, name: name, useList: uses}
}

// LoadInst ...
// ----------------
type LoadInst struct {
	op   Opcode
	ptr  Value
	typ  Type
	name string

	useList *list.List
}

func (l LoadInst) NumUses() int     { return l.useList.Len() }
func (l LoadInst) NumOperands() int { return 1 }
func (l LoadInst) Operand(idx int) Value {
	if idx != 1 {
		panic(fmt.Sprintf("[internal] invalid index '%d' for instruction '%s'", idx, l))
	}

	return l.ptr
}
func (l LoadInst) OperandList() *list.List { return l.useList }
func (l LoadInst) Type() Type              { return l.typ }
func (l LoadInst) Name() string            { return "%" + l.name }
func (l LoadInst) SetName(name string)     { l.name = name }
func (l LoadInst) HasName() bool           { return l.name != "" }
func (l LoadInst) Opcode() Opcode          { return l.op }
func (l LoadInst) IsTerm() bool            { return termop_begin < l.op && l.op < termop_end }
func (l LoadInst) IsBinaryOp() bool        { return binop_begin < l.op && l.op < binop_end }
func (l LoadInst) IsOtherOp() bool         { return other_op_begin < l.op && l.op < other_op_end }
func (l LoadInst) IsMemOp() bool           { return memop_begin < l.op && l.op < memop_end }
func (l LoadInst) IsBitBinOp() bool        { return bit_binop_begin < l.op && l.op < bit_binop_end }
func (l LoadInst) String() string {
	srcStr := fmt.Sprintf("%s %s", l.ptr.Type(), l.ptr.Name())
	return fmt.Sprintf("%s = load %s, %s", l.Name(), l.typ, srcStr)
}

func CreateLoad(typ Type, src Value, name string) *LoadInst {
	if name == "" {
		name = NextTemp()
	}

	uses := list.New()
	uses.Init()

	return &LoadInst{typ: typ, op: Load, ptr: src, name: name, useList: uses}
}

// StoreInst ...
// -------------------
type StoreInst struct {
	op    Opcode
	value Value
	dst   Value

	useList *list.List
}

func (s StoreInst) NumUses() int     { return s.useList.Len() }
func (s StoreInst) NumOperands() int { return 2 }
func (s StoreInst) Operand(idx int) Value {
	if idx < 1 || idx > 2 {
		panic(fmt.Sprintf("[internal] invalid index '%d' for instruction '%s'", idx, s))
	}

	if idx == 1 {
		return s.value
	}

	return s.dst
}
func (s StoreInst) OperandList() *list.List { return s.useList }
func (s StoreInst) Type() Type              { return nil }
func (s StoreInst) Name() string            { return "" }
func (s StoreInst) SetName(string)          {}
func (s StoreInst) HasName() bool           { return false }
func (s StoreInst) Opcode() Opcode          { return s.op }
func (s StoreInst) IsTerm() bool            { return termop_begin < s.op && s.op < termop_end }
func (s StoreInst) IsBinaryOp() bool        { return binop_begin < s.op && s.op < binop_end }
func (s StoreInst) IsOtherOp() bool         { return other_op_begin < s.op && s.op < other_op_end }
func (s StoreInst) IsMemOp() bool           { return memop_begin < s.op && s.op < memop_end }
func (s StoreInst) IsBitBinOp() bool        { return bit_binop_begin < s.op && s.op < bit_binop_end }
func (s StoreInst) String() string {
	valueStr := fmt.Sprintf("%s %s", s.value.Type(), s.value.Name())
	dstStr := fmt.Sprintf("%s %s", s.dst.Type(), s.dst.Name())

	return fmt.Sprintf("store %s, %s", valueStr, dstStr)
}

func CreateStore(value, dst Value) *StoreInst {
	uses := list.New()
	uses.Init()

	return &StoreInst{
		op:      Store,
		value:   value,
		dst:     dst,
		useList: uses,
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

	useList *list.List
}

func CreateAlloca(ty Type, numElems int, align int, name string) *AllocaInst {
	if name == "" {
		name = NextTemp()
	}

	uses := list.New()
	uses.Init()

	alloc := &AllocaInst{
		op: Alloca, allocTy: ty, numElem: 1, name: name,
		evalTy: CreatePointerType(ty), useList: uses}

	if numElems > 1 {
		alloc.numElem = numElems
	}

	if align > (1 << 32) {
		alloc.align = 1 << 32
	}

	return alloc
}

func (a AllocaInst) NumUses() int            { return a.useList.Len() }
func (a AllocaInst) NumOperands() int        { return 0 }
func (a AllocaInst) Operand(int) Value       { panic("alloca instruction has no operands") }
func (a AllocaInst) OperandList() *list.List { return a.useList }
func (a AllocaInst) Opcode() Opcode          { return a.op }
func (a AllocaInst) IsTerm() bool            { return termop_begin < a.op && a.op < termop_end }
func (a AllocaInst) IsBinaryOp() bool        { return binop_begin < a.op && a.op < binop_end }
func (a AllocaInst) IsMemOp() bool           { return memop_begin < a.op && a.op < memop_end }
func (a AllocaInst) IsOtherOp() bool         { return other_op_begin < a.op && a.op < other_op_end }
func (a AllocaInst) IsBitBinOp() bool        { return bit_binop_begin < a.op && a.op < bit_binop_end }
func (a AllocaInst) Type() Type              { return a.evalTy }
func (a AllocaInst) Name() string            { return "%" + a.name }
func (a AllocaInst) SetName(name string)     { a.name = name }
func (a AllocaInst) HasName() bool           { return a.name != "" }
func (a AllocaInst) String() string {
	s := fmt.Sprintf("%s = alloca %s", a.Name(), a.allocTy)
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

	useList *list.List
}

func CreateRet(ty Type, value Value) *ReturnInst {
	uses := list.New()
	uses.Init()

	return &ReturnInst{
		op:      Ret,
		ty:      ty,
		value:   value,
		useList: uses,
	}
}

func CreateRetVoid() *ReturnInst {
	uses := list.New()
	uses.Init()

	return &ReturnInst{
		op:      Ret,
		ty:      VoidType,
		useList: uses,
	}
}

func (r ReturnInst) NumUses() int     { return r.useList.Len() }
func (r ReturnInst) NumOperands() int { return 1 }
func (r ReturnInst) Operand(idx int) Value {
	if idx != 1 {
		panic("[internal] return instruction has 1 operand")
	}

	return r.value
}
func (r ReturnInst) OperandList() *list.List { return r.useList }
func (r ReturnInst) Type() Type              { return r.ty }
func (r ReturnInst) Name() string            { return "" }
func (r ReturnInst) SetName(s string)        {}
func (r ReturnInst) HasName() bool           { return false }
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
func (r ReturnInst) IsBitBinOp() bool { return bit_binop_begin < r.op && r.op < bit_binop_end }

// BranchInst ...
// ---------------------
type BranchInst struct {
	op      Opcode
	cond    Value
	IfTrue  *BasicBlock
	IfFalse *BasicBlock

	numSucc int

	useList *list.List
}

func CreateCondBrInst(cond Value, ifTrue, ifFalse *BasicBlock) *BranchInst {
	uses := list.New()
	uses.Init()

	return &BranchInst{
		Br,
		cond,
		ifTrue,
		ifFalse,
		2,
		uses,
	}
}

func CreateBr(dst *BasicBlock) *BranchInst {
	uses := list.New()
	uses.Init()

	return &BranchInst{
		Br,
		nil,
		dst,
		nil,
		1,
		uses,
	}
}

func (b BranchInst) NumUses() int { return b.useList.Len() }
func (b BranchInst) NumOperands() int {
	if b.IsConditional() {
		return 1
	}
	return 0
}
func (b BranchInst) Operand(idx int) Value {
	if !b.IsConditional() {
		panic("[internal] unconditional branch has no operands")
	}

	if idx != 1 {
		panic("[internal] conditional branch has only one operand")
	}

	return b.cond
}
func (b BranchInst) OperandList() *list.List  { return b.useList }
func (b BranchInst) Type() Type               { return nil }
func (b BranchInst) Name() string             { return "" }
func (b BranchInst) SetName(string)           {}
func (b BranchInst) HasName() bool            { return false }
func (b BranchInst) Opcode() Opcode           { return b.op }
func (b BranchInst) IsTerm() bool             { return termop_begin < b.op && b.op < termop_end }
func (b BranchInst) IsBinaryOp() bool         { return binop_begin < b.op && b.op < binop_end }
func (b BranchInst) IsMemOp() bool            { return memop_begin < b.op && b.op < memop_end }
func (b BranchInst) IsOtherOp() bool          { return other_op_begin < b.op && b.op < other_op_end }
func (b BranchInst) IsBitBinOp() bool         { return bit_binop_begin < b.op && b.op < bit_binop_end }
func (b BranchInst) NumSuccessors() int       { return b.numSucc }
func (b BranchInst) IsConditional() bool      { return b.cond != nil && b.numSucc == 2 }
func (b BranchInst) SetSuccessor(*BasicBlock) {}
func (b BranchInst) SetCond(cond Value)       { b.cond = cond }
func (b BranchInst) Cond() Value              { return b.cond }
func (b BranchInst) String() string {
	var s string

	if b.IsConditional() {
		s = fmt.Sprintf("br %s %s, label %%%s, label %%%s", b.cond.Type(), b.cond.Name(), b.IfTrue.name, b.IfFalse.name)
	} else {
		s = fmt.Sprintf("br label %%%s", b.IfTrue.name)
	}

	return s
}

// PHINode ...
// ---------------------
type PHINode struct {
	op               Opcode
	ty               Type
	name             string
	incoming         []PHINodeIncoming
	numIncomingPaths uint

	useList *list.List
}

func CreateEmptyPHINode(name string) *PHINode {
	if name == "" {
		name = NextTemp()
	}

	return &PHINode{
		op:   Phi,
		name: name,
	}
}

func CreatePHINode(ty Type, numIncomingPaths uint, name string) *PHINode {
	if name == "" {
		name = NextTemp()
	}

	uses := list.New()
	uses.Init()

	return &PHINode{
		op:               Phi,
		ty:               ty,
		name:             name,
		numIncomingPaths: numIncomingPaths,
		useList:          uses,
	}
}

func (phi *PHINode) NumUses() int     { return phi.useList.Len() }
func (phi *PHINode) NumOperands() int { return len(phi.incoming) }
func (phi *PHINode) Operand(idx int) Value {
	if idx < 0 || idx > len(phi.incoming) {
		panic("[internal] invalid index for operand")
	}

	return phi.incoming[idx].V
}
func (phi *PHINode) OperandList() *list.List { return phi.useList }
func (phi *PHINode) Type() Type              { return phi.ty }
func (phi *PHINode) Name() string            { return "%" + phi.name }
func (phi *PHINode) SetName(name string)     { phi.name = name }
func (phi *PHINode) HasName() bool           { return phi.name != "" }
func (phi *PHINode) String() string {
	var incs []string
	for _, inc := range phi.incoming {
		incs = append(incs, inc.String())
	}

	return fmt.Sprintf("%s = phi %s %s", phi.Name(), phi.ty, strings.Join(incs, ", "))
}
func (phi *PHINode) Opcode() Opcode   { return phi.op }
func (phi *PHINode) IsTerm() bool     { return termop_begin < phi.op && phi.op < termop_end }
func (phi *PHINode) IsBinaryOp() bool { return binop_begin < phi.op && phi.op < binop_end }
func (phi *PHINode) IsMemOp() bool    { return memop_begin < phi.op && phi.op < memop_end }
func (phi *PHINode) IsOtherOp() bool  { return other_op_begin < phi.op && phi.op < other_op_end }
func (phi *PHINode) IsBitBinOp() bool { return bit_binop_begin < phi.op && phi.op < bit_binop_end }
func (phi *PHINode) AddIncoming(v Value, blk *BasicBlock) {
	phi.incoming = append(phi.incoming, PHINodeIncoming{v, blk})
}

type PHINodeIncoming struct {
	V   Value
	Blk *BasicBlock
}

func (p PHINodeIncoming) String() string {
	return fmt.Sprintf("[ %s, %%%s ]", p.V.Name(), p.Blk.name)
}
