package asm

import (
	"fmt"
	"os"
	"os/exec"
	"strings"

	"github.com/anthonyabeo/obx/modgraph"
)

type Module struct {
	Name    string
	Funcs   []*Function
	Globals map[string]*Symbol
}

type Constant struct {
	Name  string
	Value any
	Type  Type
}

type Function struct {
	Name     string
	Params   []*Symbol
	Constant []Constant
	Result   Type
	Exported bool
	IsLeaf   bool
	Blocks   []*Block
	Entry    *Block
	Exit     *Block

	CalleeRegsUed []string // caller-saved registers used in this function
	Locals        map[string]Symbol
	Spills        map[string]SpillInfo
	HasCalls      bool // whether this function makes calls
}

func (fn *Function) OutputDOT() {
	var sb strings.Builder
	sb.WriteString("digraph CFG {\n")
	for _, block := range fn.Blocks {
		//block := fn.Blocks[key]
		label := fmt.Sprintf("%s:\\l", block.Label)
		for _, instr := range block.Instr {
			label += instr.String() + "\\l"
		}
		sb.WriteString(fmt.Sprintf("  %q [shape=box,label=\"%s\"];\n", block.Label, label))
		for _, succ := range block.Succ {
			sb.WriteString(fmt.Sprintf("  %q -> %q;\n", block.Label, succ.Label))
		}
	}
	sb.WriteString("}\n")

	dot := sb.String()

	Root, err := modgraph.FindProjectRoot()
	if err != nil {
		panic(err)
	}

	dotFile := fmt.Sprintf("%s/out/%s.asm.dot", Root, fn.Name)
	pngFile := fmt.Sprintf("%s/out/%s.asm.png", Root, fn.Name)

	if err = os.WriteFile(dotFile, []byte(dot), 0644); err != nil {
		panic(err)
	}

	cmd := exec.Command("dot", "-Tpng", dotFile, "-o", pngFile)
	if err := cmd.Run(); err != nil {
		panic(err)
	} else {
		fmt.Printf("Generated %s\n", pngFile)
	}
}

// DFSOrder returns the blocks in depth-first order starting from entry.
func (fn *Function) DFSOrder() []*Block {
	visited := make(map[int]bool)
	order := []*Block{}

	var dfs func(b *Block)
	dfs = func(b *Block) {
		if visited[b.ID] {
			return
		}
		visited[b.ID] = true
		order = append(order, b)
		for _, succ := range b.Succ {
			dfs(succ)
		}
	}

	if len(fn.Blocks) > 0 {
		dfs(fn.Entry)
	}
	return order
}

func (fn *Function) ReversePostOrder() []*Block {
	visited := make(map[int]bool)
	order := []*Block{}

	var dfs func(b *Block)
	dfs = func(b *Block) {
		if visited[b.ID] {
			return
		}
		visited[b.ID] = true
		for _, succ := range b.Succ {
			dfs(succ)
		}
		order = append(order, b)
	}

	if len(fn.Blocks) > 0 {
		dfs(fn.Entry)
	}

	// reverse the order to get reverse post-order
	for i, j := 0, len(order)-1; i < j; i, j = i+1, j-1 {
		order[i], order[j] = order[j], order[i]
	}

	return order
}

type Block struct {
	ID    int
	Label string
	Instr []*Instr
	Term  *Instr
	Succ  []*Block
	Pred  []*Block
}

func NewBlock(id int, label string) *Block {
	return &Block{ID: id, Label: label}
}
