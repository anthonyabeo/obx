package ir

import (
	"bytes"
	"fmt"
	"strings"
)

type SetOfBBs map[string]bool

func (s SetOfBBs) Add(blk string) {
	if _, exists := s[blk]; !exists {
		s[blk] = true
	}
}
func (s SetOfBBs) Contains(blk *BasicBlock) bool { return s[blk.Name()] }
func (s SetOfBBs) Empty() bool                   { return len(s) == 0 }
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

type ControlFlowGraph struct {
	Entry, Exit *BasicBlock
	Nodes       map[string]*BasicBlock
	Succ        map[string][]*BasicBlock
	Pred        map[string][]*BasicBlock
}

func NewCFG() *ControlFlowGraph {
	return &ControlFlowGraph{
		Nodes: map[string]*BasicBlock{},
		Succ:  map[string][]*BasicBlock{},
		Pred:  map[string][]*BasicBlock{},
	}
}

func (cfg *ControlFlowGraph) AddNewNode() {

}

func (cfg *ControlFlowGraph) AddSucc(BlkName string, Successors ...*BasicBlock) {
	for _, succ := range Successors {
		cfg.Succ[BlkName] = append(cfg.Succ[BlkName], succ)
	}
}

func (cfg *ControlFlowGraph) AddPred(BlkName string, Predecessors ...*BasicBlock) {
	for _, pred := range Predecessors {
		cfg.Pred[BlkName] = append(cfg.Pred[BlkName], pred)
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
			blks = append(blks, blk.name)
		}

		succ = append(succ, fmt.Sprintf("\n\t\t\t%s: %s", name, blks))
	}

	for name, bb := range cfg.Pred {
		var blks []string
		for _, blk := range bb {
			blks = append(blks, blk.name)
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
