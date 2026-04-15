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

// ExternDecl records a single foreign function that must be declared in the
// output assembly as an external symbol (e.g. .extern printf).
type ExternDecl struct {
	CName   string // C symbol name (no Oberon mangling)
	DLLName string // library name from the dll attribute
}

// Module is the IR representation of one OBX compilation unit.
type Module struct {
	Name     string
	IsEntry  bool
	Globals  map[string]*GlobalVariable
	Consts   map[string]Constant
	Funcs    []*Function
	Env      *SymbolTable
	Asm      *asm.Module
	Externals []ExternDecl // foreign symbols referenced by this module
}

// ─── Function ─────────────────────────────────────────────────────────────

// Function is the IR representation of one procedure/function body.
type Function struct {
	FnName    string
	Result    Type
	Exported  bool
	Variadic  bool
	IsBuiltin bool
	IsExternal bool // true for foreign functions declared in extern DEFINITIONs
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

// OutputDOT returns a dark-themed Graphviz DOT representation of the function's CFG.
//
// Block colour legend (dark theme):
//
//	entry  – green  (#1a3a27 / #4ade80 / #bbf7d0)
//	exit   – red    (#3a1a1a / #ef4444 / #fecaca)
//	join   – amber  (#2d2507 / #f59e0b / #fef3c7)   (≥2 predecessors)
//	branch – blue   (#0f1e3a / #60a5fa / #dbeafe)   (≥2 successors)
//	normal – slate  (#1e2234 / #64748b / #e2e8f0)
func (fn *Function) OutputDOT() string {
	var sb strings.Builder

	// ── Graph-level styling ───────────────────────────────────────────────
	sb.WriteString("digraph CFG {\n")
	sb.WriteString("  graph [fontname=\"Helvetica\" bgcolor=\"#1a1d27\" pad=\"0.5\" ranksep=\"0.7\" nodesep=\"0.4\"];\n")
	sb.WriteString("  node  [fontname=\"Courier New\" fontsize=11 style=filled penwidth=1.5];\n")
	sb.WriteString("  edge  [fontname=\"Helvetica\" fontsize=10 penwidth=1.2];\n\n")

	// ── Per-block colour (fill, border, font) ────────────────────────────
	type style struct{ fill, border, font string }
	blockStyle := func(b *Block) style {
		switch {
		case fn.Entry != nil && b.ID == fn.Entry.ID:
			return style{"#1a3a27", "#4ade80", "#bbf7d0"} // entry – green
		case fn.Exit != nil && b.ID == fn.Exit.ID:
			return style{"#3a1a1a", "#ef4444", "#fecaca"} // exit  – red
		case len(b.Preds) >= 2:
			return style{"#2d2507", "#f59e0b", "#fef3c7"} // join  – amber
		case len(b.Succs) >= 2:
			return style{"#0f1e3a", "#60a5fa", "#dbeafe"} // branch – blue
		default:
			return style{"#1e2234", "#64748b", "#e2e8f0"} // normal – slate
		}
	}

	// ── Nodes ─────────────────────────────────────────────────────────────
	//
	// We use Graphviz HTML-like labels (full.render.js required) so that the
	// block name can be rendered in bold and each instruction on its own
	// left-aligned line.  The entire content is wrapped in an explicit
	// <FONT COLOR="…"> tag because some renderers do not propagate the
	// node-level fontcolor to unstyled text that follows a </B> close.
	for _, b := range fn.SortedBlocks() {
		s := blockStyle(b)

		var lbl strings.Builder
		lbl.WriteString(fmt.Sprintf(`<FONT COLOR="%s"><B>%s</B>`, s.font, b.Label))
		for _, instr := range b.Instrs {
			line := instr.String()
			line = strings.ReplaceAll(line, "&", "&amp;")
			line = strings.ReplaceAll(line, "<", "&lt;")
			line = strings.ReplaceAll(line, ">", "&gt;")
			lbl.WriteString("<BR ALIGN=\"LEFT\"/>  " + line)
		}
		lbl.WriteString("<BR ALIGN=\"LEFT\"/></FONT>")

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
				attrs = ` [style=dashed color="#475569"]`
			case cond != nil && succ.Label == cond.TrueLabel:
				attrs = ` [label="T" color="#4ade80" fontcolor="#4ade80" penwidth=1.8]`
			case cond != nil && succ.Label == cond.FalseLabel:
				attrs = ` [label="F" color="#ef4444" fontcolor="#ef4444" penwidth=1.8]`
			default:
				attrs = ` [color="#64748b"]`
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

