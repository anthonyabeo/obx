package mir

import (
	"fmt"
	"sort"
	"strings"
)

// Program ...
type Program struct {
	Modules []*Module
}

// Module is the high level compilation unit
type Module struct {
	Name    string
	IsEntry bool
	Globals map[string]*Global
	Funcs   []*Function
}

type Function struct {
	Name   string
	Result Type
	Params map[string]Value // formal parameters
	Locals []Value          // local variable, constant, procedure declarations

	Blocks map[int]*Block // basic blocks (in order, but can build CFG)
	Entry  *Block         // pointer to entry block

	TempMap map[string]*Temp // optional: for SSA or debug
	SSAInfo *SSAInfo
	DomTree *DominatorTree

	Constants map[string]*Const
}

func NewFunction(name string, ret Type) *Function {
	return &Function{
		Name:   name,
		Result: ret,
		Blocks: make(map[int]*Block),
		Params: make(map[string]Value),
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
	out := make(map[int]*Block)
	for _, b := range fn.Blocks {
		if b != target {
			out[b.ID] = b
		}
	}

	fn.Blocks = out
}

func (fn *Function) OutputDOT() string {
	var keys []int
	for id := range fn.Blocks {
		keys = append(keys, id)
	}
	sort.Ints(keys)

	var sb strings.Builder
	sb.WriteString("digraph CFG {\n")
	for _, key := range keys {
		block := fn.Blocks[key]
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
