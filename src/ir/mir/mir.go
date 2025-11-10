package mir

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/asm"
)

// Program ...
type Program struct {
	Modules []*Module
}

// Module is the high level compilation unit
type Module struct {
	Name    string
	IsEntry bool
	Globals map[string]*GlobalVariable
	Consts  map[string]Constant
	Funcs   []*Function
	Env     *SymbolTable
	Asm     *asm.Module
}

type Function struct {
	FnName   string
	Result   Type
	Exported bool
	Params   []Value // formal parameters
	Locals   []Value // local variable, constant, procedure declarations
	IsLeaf   bool

	Blocks  map[int]*Block // basic blocks (in order, but can build CFG)
	Entry   *Block         // pointer to entry block
	Exit    *Block
	SSAInfo *SSAInfo
	Dom     *DominatorTree

	Constants map[string]Constant
	Env       *SymbolTable // symbol table for this function

	Asm *asm.Function
}

func (fn *Function) Name() string     { return fn.FnName }
func (fn *Function) BaseName() string { return fn.FnName }
func (fn *Function) String() string   { return "" }
func (fn *Function) Type() Type       { panic("implement me") }
func (fn *Function) IsMem() bool      { return false }

func NewFunction(name string, export bool, ret Type, env *SymbolTable) *Function {
	return &Function{
		FnName:    name,
		Result:    ret,
		Env:       env,
		IsLeaf:    true,
		Blocks:    make(map[int]*Block),
		Params:    make([]Value, 0),
		Constants: make(map[string]Constant),
		Dom:       NewDominatorTree(),
		SSAInfo:   NewSSAInfo(),
	}
}

func (fn *Function) GetBlock(name string) *Block {
	for _, block := range fn.Blocks {
		if block.Label == name {
			return block
		}
	}
	return nil
}

func (fn *Function) RemoveBlock(target *Block) {
	delete(fn.Blocks, target.ID)
}

// DFSOrder returns the blocks in depth-first order starting from entry.
func (fn *Function) DFSOrder() []int {
	visited := make(map[int]bool)
	order := []int{}

	var dfs func(b *Block)
	dfs = func(b *Block) {
		if visited[b.ID] {
			return
		}
		visited[b.ID] = true
		order = append(order, b.ID)
		for _, succ := range b.Succs {
			dfs(succ)
		}
	}

	if len(fn.Blocks) > 0 {
		dfs(fn.Entry)
	}
	return order
}

// ReversePostOrder returns blocks in reverse postorder from entry.
func (fn *Function) ReversePostOrder() []int {
	visited := make(map[int]bool)
	postorder := []int{}

	var dfs func(b *Block)
	dfs = func(b *Block) {
		if visited[b.ID] {
			return
		}
		visited[b.ID] = true
		for _, succ := range b.Succs {
			dfs(succ)
		}
		postorder = append(postorder, b.ID)
	}

	if len(fn.Blocks) > 0 {
		dfs(fn.Entry)
	}

	// reverse
	rpo := make([]int, len(postorder))
	for i := range postorder {
		rpo[len(postorder)-1-i] = postorder[i]
	}
	return rpo
}

func (fn *Function) SortedBlocks() []*Block {
	blocks := make([]*Block, 0, len(fn.Blocks))
	for i := fn.Entry.ID; i <= fn.Exit.ID; i++ {
		if blk, ok := fn.Blocks[i]; ok {
			blocks = append(blocks, blk)
		}
	}
	return blocks
}

func (fn *Function) OutputDOT() string {
	var sb strings.Builder
	sb.WriteString("digraph CFG {\n")
	for _, block := range fn.SortedBlocks() {
		label := fmt.Sprintf("%s:\\l", block.Label)
		for _, instr := range block.Instrs {
			label += instr.String() + "\\l"
		}
		sb.WriteString(fmt.Sprintf("  %q [shape=box,label=\"%s\"];\n", block.Label, label))
		for _, succ := range block.Succs {
			sb.WriteString(fmt.Sprintf("  %q -> %q;\n", block.Label, succ.Label))
		}
	}
	sb.WriteString("}\n")
	return sb.String()
}

var GlobalEnv = &SymbolTable{
	Parent:  nil,
	Symbols: make(map[string]Value),
}

func init() {
	// Initialize built-in functions

	GlobalEnv.Define("printf", &Function{
		FnName:   "printf",
		Result:   Int32Type,
		Exported: true,
		Params: []Value{
			&Param{Ident: "format", Typ: StringType{}},
			&Param{Ident: "args", Typ: Int32Type},
		},
	})
}

type SymbolTable struct {
	Parent  *SymbolTable
	Symbols map[string]Value
}

// NewSymbolTable creates a new symbol table with an optional parent.
func NewSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{
		Parent:  parent,
		Symbols: make(map[string]Value),
	}
}

// Define adds a symbol to the current table.
func (st *SymbolTable) Define(name string, val Value) {
	st.Symbols[name] = val
}

// Lookup searches for a symbol, checking parents up to the module.
func (st *SymbolTable) Lookup(name string) (Value, bool) {
	for table := st; table != nil; table = table.Parent {
		if val, ok := table.Symbols[name]; ok {
			return val, true
		}
	}
	return nil, false
}
