package ir

import "container/list"

type LinkageKind int

const (
	Internal LinkageKind = iota
	External
)

// Function ...
// -----------------------------
type Function struct {
	name string
	link LinkageKind
	ty   *FunctionType
	// BB []BasicBlock
}

func CreateFunction(ty *FunctionType, link LinkageKind, name string /*, module *Module*/) *Function {
	return &Function{
		name,
		link,
		ty,
	}
}

func (f Function) Type() Type               { return f.ty }
func (f Function) Name() string             { return f.name }
func (f Function) SetName(name string)      { f.name = name }
func (f Function) HasName() bool            { return f.name != "" }
func (f Function) String() string           { panic("implement me") }
func (f Function) HasInternalLinkage() bool { return f.link == Internal }
func (f Function) HasExternalLinkage() bool { return f.link == External }

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
