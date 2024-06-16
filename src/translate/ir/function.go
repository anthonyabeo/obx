package ir

import (
	"bytes"
	"container/list"
	"fmt"
)

type LinkageKind int

const (
	Internal LinkageKind = iota
	External
)

// Function ...
// -----------------------------
type Function struct {
	name   string
	link   LinkageKind
	ty     *FunctionType
	cfg    *ControlFlowGraph
	module *Module

	useList *list.List
}

func CreateFunction(ty *FunctionType, link LinkageKind, name string, module *Module) *Function {
	F := module.GetOrInsertFunction(name, ty, link)
	return F
}

func CreatePreDeclaredFunction(ty *FunctionType, link LinkageKind, name string, module *Module) *Function {
	uses := list.New()
	uses.Init()

	return &Function{
		name:    name,
		link:    link,
		ty:      ty,
		cfg:     NewCFG(),
		module:  module,
		useList: uses,
	}
}

func (f *Function) SetOperand(i int, v Value) { panic("unimplemented") }
func (f *Function) AddUse(v Value)            { f.useList.PushBack(v) }
func (f *Function) NumOperands() int          { return f.useList.Len() }
func (f *Function) Operand(int) Value         { panic("[internal] functions have no operands") }
func (f *Function) OperandList() *list.List   { return f.useList }
func (f *Function) Parent() *Module           { return f.module }
func (f *Function) NumUses() int              { return f.useList.Len() }
func (f *Function) Type() Type                { return f.ty }
func (f *Function) Name() string              { return f.name }
func (f *Function) SetName(name string)       { f.name = name }
func (f *Function) HasName() bool             { return f.name != "" }
func (f *Function) String() string {
	buf := &bytes.Buffer{}

	buf.WriteString(fmt.Sprintf("define %s @%s() {\n", f.ty.retTy, f.name))
	for _, bb := range f.cfg.Nodes {
		buf.WriteString(bb.String())
	}
	buf.WriteString("}")

	return buf.String()
}
func (f *Function) HasInternalLinkage() bool { return f.link == Internal }
func (f *Function) HasExternalLinkage() bool { return f.link == External }
func (f *Function) SymbolTable()             {}
func (f *Function) CFG() *ControlFlowGraph   { return f.cfg }

// Argument ...
// ---------------------
type Argument struct {
	name   string
	ty     *FunctionType
	parent *Function
}

func CreateArgument(ty *FunctionType, parent *Function, name string) *Argument {
	return &Argument{
		name,
		ty,
		parent,
	}
}

func (a Argument) Type() Type          { return a.ty }
func (a Argument) Name() string        { return a.name }
func (a Argument) SetName(name string) { a.name = name }
func (a Argument) HasName() bool       { return a.name != "" }
func (a Argument) String() string      { panic("implement me") }
func (a Argument) Parent() *Function   { return a.parent }

// BasicBlock ...
// -------------------------
type BasicBlock struct {
	name   string
	ty     *LabelType
	parent *Function
	instr  *list.List
	Phi    []*PHINode
}

func NewBasicBlock(name string) *BasicBlock {
	return &BasicBlock{name: name}
}

func CreateBasicBlock(name string, parent *Function) *BasicBlock {
	if name == "" {
		name = NextTemp()
	}

	instr := list.New()
	instr.Init()
	blk := &BasicBlock{
		name:   name,
		ty:     &LabelType{name: name},
		parent: parent,
		instr:  instr,
	}

	parent.cfg.Nodes[name] = blk

	return blk
}

func (b *BasicBlock) Type() Type          { return b.ty }
func (b *BasicBlock) Name() string        { return b.name }
func (b *BasicBlock) SetName(name string) { b.name = name }
func (b *BasicBlock) HasName() bool       { return b.name != "" }
func (b *BasicBlock) String() string {
	s := fmt.Sprintf("%%%s:\n\t", b.name)

	l := b.instr
	for inst := l.Front(); inst != nil; {
		if inst.Next() != nil {
			s += fmt.Sprintf("%s\n\t", inst.Value)
		} else {
			s += fmt.Sprintf("%s\n\n", inst.Value)
		}

		inst = inst.Next()
	}

	return s
}
func (b *BasicBlock) Parent() *Function                 { return b.parent }
func (b *BasicBlock) Instr() *list.List                 { return b.instr }
func (b *BasicBlock) InsertInstrBegin(inst Instruction) { b.instr.InsertBefore(inst, b.instr.Front()) }
func (b *BasicBlock) RemoveInstr(rem *list.Element)     { b.instr.Remove(rem) }
