package ir

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/adt"
)

// BitVector
// -----------------------------
type BitVector byte

// ControlFlowGraph
// ----------------------------------------------------------------
type ControlFlowGraph struct {
	Entry, Exit *BasicBlock
	Nodes       *adt.HashSet[*BasicBlock]
	Succ        map[string][]*BasicBlock
	Pred        map[string][]*BasicBlock
}

func NewCFG() *ControlFlowGraph {
	return &ControlFlowGraph{
		Nodes: adt.NewHashSet[*BasicBlock](),
		Succ:  map[string][]*BasicBlock{},
		Pred:  map[string][]*BasicBlock{},
	}
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
	return len(cfg.Pred[blk]) > 1
}

func (cfg *ControlFlowGraph) IsBrNode(blk string) bool {
	return len(cfg.Succ[blk]) > 1
}

func (cfg *ControlFlowGraph) String() string {
	var nodes, succ, pred []string
	for _, blk := range cfg.Nodes.Elems() {
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

func (cfg *ControlFlowGraph) PostOrder() []*BasicBlock {
	visited := map[string]bool{}
	order := make([]*BasicBlock, 0)

	cfg.dfs(cfg.Entry, visited, &order)
	return order
}

func (cfg *ControlFlowGraph) dfs(node *BasicBlock, visited map[string]bool, order *[]*BasicBlock) {
	visited[node.name] = true
	for _, succ := range cfg.Succ[node.name] {
		if _, found := visited[succ.name]; !found {
			cfg.dfs(succ, visited, order)
		}
	}

	*order = append(*order, node)
}

func (cfg *ControlFlowGraph) ReversePostOrder() []*BasicBlock {
	pOrder := cfg.PostOrder()
	var revOrder []*BasicBlock
	for i := len(pOrder) - 1; i >= 0; i-- {
		revOrder = append(revOrder, pOrder[i])
	}

	return revOrder
}

func (cfg *ControlFlowGraph) Replace(to Instruction, replace Value) {
	for use := to.OperandList().Front(); use != nil; use = use.Next() {
		useInstr := use.Value.(Instruction)
		for i := 1; i < useInstr.NumOperands()+1; i++ {
			if useInstr.Operand(i).Name() == to.Name() {
				useInstr.SetOperand(i, replace)
			}
		}
	}
}
