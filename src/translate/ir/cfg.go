package ir

import (
	"bytes"
	"fmt"
	"strings"
)

// BitVector
// -----------------------------
type BitVector byte

// SetOfBBs
// -----------------------------------------------------------------------
type SetOfBBs map[string]*BasicBlock

func (s SetOfBBs) Add(blocks ...*BasicBlock) {
	for _, blk := range blocks {
		if _, exists := s[blk.name]; !exists {
			s[blk.name] = blk
		}
	}
}
func (s SetOfBBs) Contains(blk string) bool { return s[blk] != nil }
func (s SetOfBBs) Empty() bool              { return len(s) == 0 }
func (s SetOfBBs) String() string {
	buf := &bytes.Buffer{}
	buf.WriteString("{")
	for bb := range s {
		buf.WriteString(bb)
		buf.WriteString(", ")
	}

	buf.WriteString("}")
	return buf.String()
}
func (s SetOfBBs) Union(other SetOfBBs) SetOfBBs {
	set := SetOfBBs{}
	for _, BB := range s {
		set.Add(BB)
	}
	for _, BB := range other {
		set.Add(BB)
	}

	return set
}
func (s SetOfBBs) Intersection(other SetOfBBs) SetOfBBs {
	intersect := SetOfBBs{}
	if len(s) > len(other) {
		s, other = other, s
	}
	for name, BB := range s {
		if _, exists := other[name]; exists {
			intersect[name] = BB
		}
	}

	return intersect
}
func (s SetOfBBs) Equal(other SetOfBBs) bool {
	if len(s) != len(other) {
		return false
	}

	for name := range s {
		if _, exist := other[name]; !exist {
			return false
		}
	}

	return true
}
func (s SetOfBBs) Len() int { return len(s) }
func (s SetOfBBs) Remove(blk string) SetOfBBs {
	delete(s, blk)
	return s
}
func (s SetOfBBs) Pop() *BasicBlock {
	var v *BasicBlock

	for key, value := range s {
		v = value
		delete(s, key)
		break
	}

	return v
}

// ControlFlowGraph
// ----------------------------------------------------------------
type ControlFlowGraph struct {
	Entry, Exit *BasicBlock
	Nodes       SetOfBBs
	Succ        map[string][]string
	Pred        map[string][]string
}

func NewCFG() *ControlFlowGraph {
	return &ControlFlowGraph{
		Nodes: map[string]*BasicBlock{},
		Succ:  map[string][]string{},
		Pred:  map[string][]string{},
	}
}

func (cfg *ControlFlowGraph) AddSucc(BlkName string, Successors ...*BasicBlock) {
	for _, succ := range Successors {
		cfg.Succ[BlkName] = append(cfg.Succ[BlkName], succ.name)
	}
}

func (cfg *ControlFlowGraph) AddPred(BlkName string, Predecessors ...*BasicBlock) {
	for _, pred := range Predecessors {
		cfg.Pred[BlkName] = append(cfg.Pred[BlkName], pred.name)
	}
}

func (cfg *ControlFlowGraph) IsJoinNode(blk string) bool {
	return len(cfg.Succ[blk]) > 1
}

func (cfg *ControlFlowGraph) IsBrNode(blk string) bool {
	return len(cfg.Pred[blk]) > 1
}

func (cfg *ControlFlowGraph) String() string {
	var nodes, succ, pred []string
	for _, blk := range cfg.Nodes {
		nodes = append(nodes, blk.name)
	}

	for name, bb := range cfg.Succ {
		var blks []string
		for _, blk := range bb {
			blks = append(blks, blk)
		}

		succ = append(succ, fmt.Sprintf("\n\t\t\t%s: %s", name, blks))
	}

	for name, bb := range cfg.Pred {
		var blks []string
		for _, blk := range bb {
			blks = append(blks, blk)
		}

		pred = append(pred, fmt.Sprintf("\n\t\t\t%s: %s", name, blks))
	}

	buf := &bytes.Buffer{}
	buf.WriteString("flowgraph = {\n\t")
	buf.WriteString(fmt.Sprintf("\tNodes = {%s},\n\t", strings.Join(nodes, ", ")))
	buf.WriteString(fmt.Sprintf("\tSucc = {%s\n\t\t},\n\t", strings.Join(succ, ", ")))
	buf.WriteString(fmt.Sprintf("\tPred = {%s\n\t\t},\n\t", strings.Join(pred, ", ")))
	buf.WriteString("}")

	return buf.String()
}

func (cfg *ControlFlowGraph) PostOrder() []string {
	visited := map[string]bool{}
	order := make([]string, 0)

	cfg.dfs("entry", visited, &order)
	return order
}

func (cfg *ControlFlowGraph) dfs(node string, visited map[string]bool, order *[]string) {
	visited[node] = true
	for _, succ := range cfg.Succ[node] {
		if _, found := visited[succ]; !found {
			cfg.dfs(succ, visited, order)
		}
	}

	*order = append(*order, node)
}

func (cfg *ControlFlowGraph) ReversePostOrder() []string {
	pOrder := cfg.PostOrder()
	var revOrder []string
	for i := len(pOrder) - 1; i >= 0; i-- {
		revOrder = append(revOrder, pOrder[i])
	}

	return revOrder
}
