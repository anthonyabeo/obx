package core

import (
	"fmt"
	"sort"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/desugar"
)

// BasicBlock and Function containers.

// Block is a basic block: a list of instructions with an explicit terminator.
type Block struct {
	ID     int
	Label  string
	Instrs []Instr
	Term   Terminator // should equal last instruction in Instrs

	// fast lookup maps
	Preds map[int]*Block
	Succs map[int]*Block

	// ordering of preds/succs for deterministic iteration (stores block IDs)
	PredOrder []int
	SuccOrder []int

	Parent *Function // the function this block belongs to
}

// AddSucc links s as a successor of b (and records ordering). It does not
// update the corresponding pred on s; callers should maintain both sides.
func (b *Block) AddSucc(s *Block) {
	if b.Succs == nil {
		b.Succs = make(map[int]*Block)
	}
	if _, ok := b.Succs[s.ID]; ok {
		return
	}
	b.Succs[s.ID] = s
	b.SuccOrder = append(b.SuccOrder, s.ID)
}

// AddPred links p as a predecessor of b (and records ordering). It does not
// update the corresponding succ on p; callers should maintain both sides.
func (b *Block) AddPred(p *Block) {
	if b.Preds == nil {
		b.Preds = make(map[int]*Block)
	}
	if _, ok := b.Preds[p.ID]; ok {
		return
	}
	b.Preds[p.ID] = p
	b.PredOrder = append(b.PredOrder, p.ID)
}

// SortedSuccs returns successors in deterministic order: if SuccOrder is populated, it is used;
// otherwise the map keys are sorted and returned.
func (b *Block) SortedSuccs() []*Block {
	var out []*Block
	if len(b.SuccOrder) > 0 {
		for _, id := range b.SuccOrder {
			if s, ok := b.Succs[id]; ok {
				out = append(out, s)
			}
		}
		return out
	}
	// fallback: gather keys and sort
	ids := make([]int, 0, len(b.Succs))
	for id := range b.Succs {
		ids = append(ids, id)
	}
	if len(ids) == 0 {
		return nil
	}
	sort.Ints(ids)
	for _, id := range ids {
		out = append(out, b.Succs[id])
	}
	return out
}

// SortedPreds similar to SortedSuccs.
func (b *Block) SortedPreds() []*Block {
	var out []*Block
	if len(b.PredOrder) > 0 {
		for _, id := range b.PredOrder {
			if p, ok := b.Preds[id]; ok {
				out = append(out, p)
			}
		}
		return out
	}
	ids := make([]int, 0, len(b.Preds))
	for id := range b.Preds {
		ids = append(ids, id)
	}
	if len(ids) == 0 {
		return nil
	}
	sort.Ints(ids)
	for _, id := range ids {
		out = append(out, b.Preds[id])
	}
	return out
}

// Function is a collection of blocks with an entry block and signature.
type Function struct {
	FnName string
	Params []*Temp
	// ParamKinds mirrors the incoming HIR param kinds (ValueParam / VarParam / InParam)
	// and is indexed in the same order as Params. Kept minimal so downstream
	// lowering/passes can inspect parameter semantics.
	ParamKinds []desugar.ParamKind
	Result     Type
	Entry      *Block
	Exit       *Block
	Blocks     map[int]*Block
	SymTab     SymbolTable // function-local symbol table (params + alloca defs)
}

// GetBlock finds a block by label; returns nil if not found.
func (f *Function) GetBlock(label string) *Block {
	for _, b := range f.Blocks {
		if b.Label == label {
			return b
		}
	}
	return nil
}

// RemoveBlock removes b from the function's block map.
// It does not update predecessor/successor lists of other blocks; callers are
// responsible for maintaining CFG consistency before calling RemoveBlock.
func (f *Function) RemoveBlock(b *Block) {
	delete(f.Blocks, b.ID)
}

func (f *Function) RemoveEdge(from *Block, to *Block) {
	delete(from.Succs, to.ID)
	delete(to.Preds, from.ID)

	removeID := func(ids []int, target int) []int {
		for i, id := range ids {
			if id == target {
				return append(ids[:i], ids[i+1:]...)
			}
		}
		return ids
	}

	// update ordering slices
	from.SuccOrder = removeID(from.SuccOrder, to.ID)
	to.PredOrder = removeID(to.PredOrder, from.ID)
}

// DFSOrder returns block IDs in depth-first order starting from entry.
func (f *Function) DFSOrder() []int {
	visited := make(map[int]bool)
	dfsorder := []int{}

	var dfs func(b *Block)
	dfs = func(b *Block) {
		if visited[b.ID] {
			return
		}
		visited[b.ID] = true
		dfsorder = append(dfsorder, b.ID)
		for _, succ := range b.SortedSuccs() {
			dfs(succ)
		}
	}
	if len(f.Blocks) > 0 && f.Entry != nil {
		dfs(f.Entry)
	}
	return dfsorder
}

// ReversePostOrder returns block IDs in reverse post-order.
func (f *Function) ReversePostOrder() []int {
	visited := make(map[int]bool)
	postorder := []int{}

	var dfs func(b *Block)
	dfs = func(b *Block) {
		if visited[b.ID] {
			return
		}
		visited[b.ID] = true
		for _, succ := range b.SortedSuccs() {
			dfs(succ)
		}
		postorder = append(postorder, b.ID)
	}

	if len(f.Blocks) > 0 && f.Entry != nil {
		dfs(f.Entry)
	}

	// reverse postorder
	for i, j := 0, len(postorder)-1; i < j; i, j = i+1, j-1 {
		postorder[i], postorder[j] = postorder[j], postorder[i]
	}
	return postorder
}

// SortedBlocks returns blocks in deterministic order.
func (f *Function) SortedBlocks() []*Block {
	var out []*Block
	ids := make([]int, 0, len(f.Blocks))
	for id := range f.Blocks {
		ids = append(ids, id)
	}
	sort.Ints(ids)
	for _, id := range ids {
		out = append(out, f.Blocks[id])
	}
	return out
}

// OutputDOT returns a dark-themed Graphviz DOT representation of the function's CFG.
//
// Block color legend (dark theme):
//
//	entry  – green  (#1a3a27 / #4ade80 / #bbf7d0)
//	exit   – red    (#3a1a1a / #ef4444 / #fecaca)
//	join   – amber  (#2d2507 / #f59e0b / #fef3c7)   (≥2 predecessors)
//	branch – blue   (#0f1e3a / #60a5fa / #dbeafe)   (≥2 successors)
//	normal – slate  (#1e2234 / #64748b / #e2e8f0)
func (f *Function) OutputDOT() string {
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
		case f.Entry != nil && b.ID == f.Entry.ID:
			return style{"#1a3a27", "#4ade80", "#bbf7d0"} // entry – green
		case f.Exit != nil && b.ID == f.Exit.ID:
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
	for _, b := range f.SortedBlocks() {
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
	for _, b := range f.SortedBlocks() {
		var cond *CondBrInst
		if b.Term != nil {
			cond, _ = b.Term.(*CondBrInst)
		}
		isExit := f.Exit != nil && b.ID == f.Exit.ID

		// compute deterministic iteration order for successors
		succs := b.SortedSuccs()
		for _, succ := range succs {
			// base attribute string (without surrounding brackets) so we can
			// append constraint=false for back-edges when needed.
			var attrBody string
			switch {
			case isExit:
				attrBody = `style=dashed color="#475569"`
			case cond != nil && succ.Label == cond.TrueLabel:
				attrBody = `label="T" color="#4ade80" fontcolor="#4ade80" penwidth=1.8`
			case cond != nil && succ.Label == cond.FalseLabel:
				attrBody = `label="F" color="#ef4444" fontcolor="#ef4444" penwidth=1.8`
			default:
				attrBody = `color="#64748b"`
			}

			// simple back-edge heuristic: successor appears earlier in DFS order
			// than the current block.
			dfsIdx := map[int]int{}
			order := f.DFSOrder()
			for i, id := range order {
				dfsIdx[id] = i
			}
			if dfsIdx[succ.ID] <= dfsIdx[b.ID] {
				attrBody += " constraint=false"
			}

			sb.WriteString(fmt.Sprintf("  %q -> %q [%s];\n", b.Label, succ.Label, attrBody))
		}
	}

	sb.WriteString("}\n")
	return sb.String()
}
