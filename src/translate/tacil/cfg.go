package tacil

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
	Nodes       adt.Set[*BasicBlock]
	Succ        map[string]*adt.HashSet[*BasicBlock]
	Pred        map[string]*adt.HashSet[*BasicBlock]
}

func NewCFG() *ControlFlowGraph {
	return &ControlFlowGraph{
		Nodes: adt.NewHashSet[*BasicBlock](),
		Succ:  make(map[string]*adt.HashSet[*BasicBlock]),
		Pred:  make(map[string]*adt.HashSet[*BasicBlock]),
	}
}

func (cfg *ControlFlowGraph) AddSucc(BlkName string, Successors ...*BasicBlock) {
	if cfg.Succ[BlkName] == nil {
		cfg.Succ[BlkName] = adt.NewHashSet[*BasicBlock]()
	}
	cfg.Succ[BlkName].Add(Successors...)
}

func (cfg *ControlFlowGraph) AddPred(BlkName string, Predecessors ...*BasicBlock) {
	if cfg.Pred[BlkName] == nil {
		cfg.Pred[BlkName] = adt.NewHashSet[*BasicBlock]()
	}
	cfg.Pred[BlkName].Add(Predecessors...)
}

func (cfg *ControlFlowGraph) IsJoinNode(blk string) bool {
	if cfg.Pred[blk] != nil {
		return cfg.Pred[blk].Size() > 1
	}

	return false
}

func (cfg *ControlFlowGraph) IsBrNode(blk string) bool {
	return cfg.Succ[blk].Size() > 1
}

func (cfg *ControlFlowGraph) String() string {
	var nodes, succ, pred []string
	for _, blk := range cfg.Nodes.Elems() {
		nodes = append(nodes, blk.name)
	}

	for name, bb := range cfg.Succ {
		var blocks []string
		for _, blk := range bb.Elems() {
			blocks = append(blocks, blk.name)
		}

		succ = append(succ, fmt.Sprintf("\n\t\t\t%s: %s", name, blocks))
	}

	for name, bb := range cfg.Pred {
		var blocks []string
		for _, blk := range bb.Elems() {
			blocks = append(blocks, blk.name)
		}

		pred = append(pred, fmt.Sprintf("\n\t\t\t%s: %s", name, blocks))
	}

	buf := &bytes.Buffer{}
	buf.WriteString("flow-graph = {\n\t")
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
	if cfg.Succ[node.name] != nil {
		for _, succ := range cfg.Succ[node.name].Elems() {
			if _, found := visited[succ.name]; !found {
				cfg.dfs(succ, visited, order)
			}
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

func (cfg *ControlFlowGraph) Reverse() *ControlFlowGraph {
	panic("not implemented")
}

//func (cfg *ControlFlowGraph) Replace(to Instruction, replace Value) {
//	for use := to.OperandList().Front(); use != nil; use = use.Next() {
//		useInstr := use.Value.(Instruction)
//		for i := 1; i < useInstr.NumOperands()+1; i++ {
//			if useInstr.Operand(i).Name() == to.Name() {
//				useInstr.SetOperand(i, replace)
//			}
//		}
//	}
//}
//
//func (cfg *ControlFlowGraph) DeleteBlocks(blocks ...*BasicBlock) {
//	cfg.Nodes.Remove(blocks...)
//	for _, block := range blocks {
//		delete(cfg.Succ, block.Name())
//		delete(cfg.Pred, block.Name())
//
//		for _, succ := range cfg.Succ {
//			for _, bb := range succ.Elems() {
//				if bb == block {
//					succ.Remove(block)
//				}
//			}
//		}
//
//		for _, pred := range cfg.Pred {
//			for _, bb := range pred.Elems() {
//				if bb == block {
//					pred.Remove(block)
//				}
//			}
//		}
//	}
//}
