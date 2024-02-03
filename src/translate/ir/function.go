package ir

import (
	"container/list"
	"fmt"
)

type LinkageKind int

const (
	Internal LinkageKind = iota
	External
)

// BasicBlockListType defines the type of the list of basic blocks
// of a Function. It is a map of strings to BasicBlocks
type BasicBlockListType map[string]*BasicBlock

// AddNewBasicBlock adds a new BasicBlock and updates the successors
// and predecessors accordingly to maintain the CFG.
func (bb BasicBlockListType) AddNewBasicBlock(BBName string, BB *BasicBlock, Successors, Predecessors []string) {
	for _, s := range Successors {
		blk, found := bb[s]
		if !found {
			panic(fmt.Sprintf("[internal]: Basic Block named '%s' does not exist", s))
		}

		BB.AddSuccessors(blk)
	}

	for _, pred := range Predecessors {
		blk, found := bb[pred]
		if !found {
			panic(fmt.Sprintf("[internal]: Basic Block named '%s' does not exist", pred))
		}

		BB.AddPredecessors(blk)
	}

	bb[BBName] = BB
}

func (bb BasicBlockListType) Block(name string) *BasicBlock { return bb[name] }

// Function ...
// -----------------------------
type Function struct {
	name   string
	link   LinkageKind
	ty     *FunctionType
	blocks BasicBlockListType
	module *Module
}

func CreateFunction(ty *FunctionType, link LinkageKind, name string, module *Module) *Function {
	F := module.GetOrInsertFunction(name, ty, link)
	return F
}

func (f Function) Type() Type               { return f.ty }
func (f Function) Name() string             { return f.name }
func (f Function) SetName(name string)      { f.name = name }
func (f Function) HasName() bool            { return f.name != "" }
func (f Function) String() string           { panic("implement me") }
func (f Function) HasInternalLinkage() bool { return f.link == Internal }
func (f Function) HasExternalLinkage() bool { return f.link == External }
func (f Function) EntryBlock() *BasicBlock  { return f.blocks.Block("entry") }
func (f Function) AddNewBlock(name string, BB *BasicBlock, Successors, Predecessors []string) {
	f.blocks.AddNewBasicBlock(name, BB, Successors, Predecessors)
}
func (f Function) Blocks() BasicBlockListType { return f.blocks }
func (f Function) SymbolTable()               {}

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
	instr  list.List

	suc  []*BasicBlock
	pred []*BasicBlock
}

func CreateBasicBlock(name string, parent *Function) *BasicBlock {
	return &BasicBlock{
		name:   name,
		ty:     &LabelType{name: name},
		parent: parent,
		instr:  list.List{},
	}
}

func (b BasicBlock) Type() Type          { return b.ty }
func (b BasicBlock) Name() string        { return b.name }
func (b BasicBlock) SetName(name string) { b.name = name }
func (b BasicBlock) HasName() bool       { return b.name != "" }
func (b BasicBlock) String() string      { panic("implement me") }
func (b BasicBlock) Parent() *Function   { return b.parent }
func (b BasicBlock) AddSuccessors(successors ...*BasicBlock) {
	b.suc = append(b.suc, successors...)
}
func (b BasicBlock) AddPredecessors(predecessors ...*BasicBlock) {
	b.pred = append(b.pred, predecessors...)
}
func (b BasicBlock) Instr() list.List { return b.instr }
