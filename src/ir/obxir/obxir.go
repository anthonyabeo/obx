package obxir

import (
	"fmt"
	"sort"
	"strings"

	"github.com/anthonyabeo/obx/src/codegen/asm"
)

// ─── Program ──────────────────────────────────────────────────────────────

// Program is the top-level container for all compiled modules.
type Program struct {
	Modules []*Module
}

// ─── Module ───────────────────────────────────────────────────────────────

// Module is the IR representation of one OBX compilation unit.
type Module struct {
	Name    string
	IsEntry bool
	Globals map[string]*GlobalVariable
	Consts  map[string]Constant
	Funcs   []*Function
	Env     *SymbolTable
	Asm     *asm.Module
}

// ─── Function ─────────────────────────────────────────────────────────────

// Function is the IR representation of one procedure/function body.
type Function struct {
	FnName    string
	Result    Type
	Exported  bool
	Variadic  bool
	IsBuiltin bool
	Params    []Value
	Locals    []Value
	IsLeaf    bool

	Blocks  map[int]*Block
	Entry   *Block
	Exit    *Block
	SSAInfo *SSAInfo
	Dom     *DominatorTree

	Constants map[string]Constant
	Env       *SymbolTable

	Asm *asm.Function
}

func (fn *Function) Name() string     { return fn.FnName }
func (fn *Function) BaseName() string { return fn.FnName }
func (fn *Function) String() string   { return "" }
func (fn *Function) Type() Type       { panic("Function.Type(): call Result instead") }
func (fn *Function) IsMem() bool      { return false }

func NewFunction(name string, exported bool, ret Type, env *SymbolTable) *Function {
	return &Function{
		FnName:    name,
		Result:    ret,
		Exported:  exported,
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
	for _, b := range fn.Blocks {
		if b.Label == name {
			return b
		}
	}
	return nil
}

func (fn *Function) RemoveBlock(target *Block) {
	delete(fn.Blocks, target.ID)
}

// DFSOrder returns block IDs in depth-first order starting from entry.
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

// ReversePostOrder returns block IDs in reverse post-order.
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

	rpo := make([]int, len(postorder))
	for i := range postorder {
		rpo[len(postorder)-1-i] = postorder[i]
	}
	return rpo
}

// SortedBlocks returns all blocks ordered by their numeric ID.
func (fn *Function) SortedBlocks() []*Block {
	ids := make([]int, 0, len(fn.Blocks))
	for id := range fn.Blocks {
		ids = append(ids, id)
	}
	sort.Ints(ids)
	blocks := make([]*Block, 0, len(ids))
	for _, id := range ids {
		blocks = append(blocks, fn.Blocks[id])
	}
	return blocks
}

// OutputDOT returns a colorful Graphviz DOT representation of the function's CFG.
//
// Block colour legend:
//
//	entry  – green  (#d4f4dd / #2e7d32)
//	exit   – red    (#ffd6d6 / #b71c1c)
//	join   – gold   (#fff9c4 / #f57f17)   (≥2 predecessors)
//	branch – blue   (#dceefb / #1565c0)   (≥2 successors)
//	normal – white  (#ffffff / #555555)
func (fn *Function) OutputDOT() string {
	var sb strings.Builder

	// ── Graph-level styling ───────────────────────────────────────────────
	sb.WriteString("digraph CFG {\n")
	sb.WriteString("  graph [fontname=\"Helvetica\" bgcolor=\"#fafafa\" pad=\"0.4\" ranksep=\"0.6\"];\n")
	sb.WriteString("  node  [fontname=\"Courier New\" fontsize=11 style=filled penwidth=1.5];\n")
	sb.WriteString("  edge  [fontname=\"Helvetica\" fontsize=10 penwidth=1.2];\n\n")

	// ── Per-block colour (fill, border, font) ────────────────────────────
	type style struct{ fill, border, font string }
	blockStyle := func(b *Block) style {
		switch {
		case fn.Entry != nil && b.ID == fn.Entry.ID:
			return style{"#d4f4dd", "#2e7d32", "#1b5e20"} // entry – green
		case fn.Exit != nil && b.ID == fn.Exit.ID:
			return style{"#ffd6d6", "#b71c1c", "#7f0000"} // exit  – red
		case len(b.Preds) >= 2:
			return style{"#fff9c4", "#f57f17", "#4e342e"} // join  – gold
		case len(b.Succs) >= 2:
			return style{"#dceefb", "#1565c0", "#0d3c61"} // branch – blue
		default:
			return style{"#ffffff", "#555555", "#222222"} // normal – white
		}
	}

	// ── Nodes ─────────────────────────────────────────────────────────────
	for _, b := range fn.SortedBlocks() {
		var lbl strings.Builder
		lbl.WriteString(fmt.Sprintf("<B>%s</B>", b.Label))
		for _, instr := range b.Instrs {
			line := instr.String()
			line = strings.ReplaceAll(line, "&", "&amp;")
			line = strings.ReplaceAll(line, "<", "&lt;")
			line = strings.ReplaceAll(line, ">", "&gt;")
			lbl.WriteString(fmt.Sprintf("<BR ALIGN=\"LEFT\"/>  %s", line))
		}
		lbl.WriteString("<BR ALIGN=\"LEFT\"/>")

		s := blockStyle(b)
		sb.WriteString(fmt.Sprintf(
			"  %q [shape=box fillcolor=%q color=%q fontcolor=%q label=<%s>];\n",
			b.Label, s.fill, s.border, s.font, lbl.String(),
		))
	}

	sb.WriteString("\n")

	// ── Edges ─────────────────────────────────────────────────────────────
	for _, b := range fn.SortedBlocks() {
		var cond *CondBrInst
		if b.Term != nil {
			cond, _ = b.Term.(*CondBrInst)
		}
		isExit := fn.Exit != nil && b.ID == fn.Exit.ID

		for _, succ := range b.Succs {
			var attrs string
			switch {
			case isExit:
				attrs = ` [style=dashed color="#999999"]`
			case cond != nil && succ.Label == cond.TrueLabel:
				attrs = ` [label="T" color="#2e7d32" fontcolor="#2e7d32" penwidth=1.8]`
			case cond != nil && succ.Label == cond.FalseLabel:
				attrs = ` [label="F" color="#b71c1c" fontcolor="#b71c1c" penwidth=1.8]`
			default:
				attrs = ` [color="#555555"]`
			}
			sb.WriteString(fmt.Sprintf("  %q -> %q%s;\n", b.Label, succ.Label, attrs))
		}
	}

	sb.WriteString("}\n")
	return sb.String()
}

// ─── SymbolTable ──────────────────────────────────────────────────────────

// SymbolTable is a lexically-scoped name → Value mapping.
type SymbolTable struct {
	Parent  *SymbolTable
	Symbols map[string]Value
}

func NewSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{
		Parent:  parent,
		Symbols: make(map[string]Value),
	}
}

func (st *SymbolTable) Define(name string, val Value) {
	st.Symbols[name] = val
}

func (st *SymbolTable) Lookup(name string) (Value, bool) {
	for t := st; t != nil; t = t.Parent {
		if val, ok := t.Symbols[name]; ok {
			return val, true
		}
	}
	return nil, false
}

