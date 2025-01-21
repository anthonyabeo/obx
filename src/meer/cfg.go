package meer

import (
	"bytes"
	"container/list"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/adt"
)

// BasicBlock ...
// -------------------------------
type BasicBlock struct {
	id    uint
	name  string
	instr *list.List
	Phi   map[string]AssignInst
}

var nb uint = 1

func NewBasicBlock(lbl *Label) *BasicBlock {
	blk := &BasicBlock{
		name: lbl.Name,
		id:   nb,
	}

	lbl.BlockID = nb

	nb++

	return blk
}

var nextBlock uint = 1

func CreateBasicBlock(lbl *Label) *BasicBlock {
	instr := list.New()
	instr.Init()
	blk := &BasicBlock{
		name:  lbl.Name,
		instr: instr,
		id:    nextBlock,
	}

	lbl.BlockID = nextBlock

	nextBlock++

	return blk
}

func (b *BasicBlock) ID() uint            { return b.id }
func (b *BasicBlock) Name() string        { return b.name }
func (b *BasicBlock) SetName(name string) { b.name = name }
func (b *BasicBlock) HasName() bool       { return b.name != "" }
func (b *BasicBlock) String() string {
	s := fmt.Sprintf("%%%s:\n\t", b.name)

	for inst := b.instr.Front(); inst != nil; inst = inst.Next() {
		i := inst.Value.(Instruction)
		if inst.Next() != nil {
			s += fmt.Sprintf("%s\n\t", i)
		} else {
			s += fmt.Sprintf("%s\n\n", i)
		}
	}

	return s
}
func (b *BasicBlock) Instr() *list.List { return b.instr }
func (b *BasicBlock) InsertInstrBegin(inst Instruction) {
	b.instr.InsertBefore(inst, b.instr.Front())
}
func (b *BasicBlock) RemoveInstr(rem *list.Element) { b.instr.Remove(rem) }
func (b *BasicBlock) Empty() bool {
	if b.instr.Len() != 1 {
		return false
	}

	_, ok := b.instr.Front().Value.(*JumpInst)
	return ok
}
func (b *BasicBlock) LastInst() Instruction  { return b.instr.Back().Value.(Instruction) }
func (b *BasicBlock) AddInstr(i Instruction) { b.instr.PushBack(i) }
func (b *BasicBlock) LastInstIsCondBr() bool {
	_, ok := b.LastInst().(*CondBrInst)
	return ok
}

// ControlFlowGraph
// ----------------------------------------------------------------
type ControlFlowGraph struct {
	Entry, Exit *BasicBlock
	Blocks      map[uint]*BasicBlock
	Nodes       adt.Set[*BasicBlock]
	Suc         map[uint]*adt.HashSet[uint]
	Pred        map[uint]*adt.HashSet[uint]
}

func NewCFG() *ControlFlowGraph {
	return &ControlFlowGraph{
		Nodes:  adt.NewHashSet[*BasicBlock](),
		Blocks: make(map[uint]*BasicBlock),
		Suc:    make(map[uint]*adt.HashSet[uint]),
		Pred:   make(map[uint]*adt.HashSet[uint]),
	}
}

func (cfg *ControlFlowGraph) AddSuc(BlkID uint, Successors ...uint) {
	if cfg.Suc[BlkID] == nil {
		cfg.Suc[BlkID] = adt.NewHashSet[uint]()
	}
	cfg.Suc[BlkID].Add(Successors...)
}

func (cfg *ControlFlowGraph) AddPred(BlkID uint, Predecessors ...uint) {
	if cfg.Pred[BlkID] == nil {
		cfg.Pred[BlkID] = adt.NewHashSet[uint]()
	}
	cfg.Pred[BlkID].Add(Predecessors...)
}

func (cfg *ControlFlowGraph) IsJoinNode(blk uint) bool {
	if cfg.Pred[blk] != nil {
		return cfg.Pred[blk].Size() > 1
	}

	return false
}

func (cfg *ControlFlowGraph) IsBrNode(blk uint) bool {
	return cfg.Suc[blk].Size() > 1
}

func (cfg *ControlFlowGraph) String() string {
	var nodes, suc, pred []string
	for _, blk := range cfg.Nodes.Elems() {
		nodes = append(nodes, blk.name)
	}

	for id, bb := range cfg.Suc {
		var blocks []string
		for _, blk := range bb.Elems() {
			blocks = append(blocks, cfg.Blocks[blk].name)
		}

		suc = append(suc, fmt.Sprintf("\n\t\t\t%s: %s", cfg.Blocks[id], blocks))
	}

	for id, bb := range cfg.Pred {
		var blocks []string
		for _, blk := range bb.Elems() {
			blocks = append(blocks, cfg.Blocks[blk].name)
		}

		pred = append(pred, fmt.Sprintf("\n\t\t\t%s: %s", cfg.Blocks[id], blocks))
	}

	buf := &bytes.Buffer{}
	buf.WriteString("flow-graph = {\n\t")
	buf.WriteString(fmt.Sprintf("\tNodes = {%s},\n\t", strings.Join(nodes, ", ")))
	buf.WriteString(fmt.Sprintf("\tSucc = {%s\n\t\t},\n\t", strings.Join(suc, ", ")))
	buf.WriteString(fmt.Sprintf("\tPred = {%s\n\t\t},\n\t", strings.Join(pred, ", ")))
	buf.WriteString("}")

	return buf.String()
}

func (cfg *ControlFlowGraph) PostOrder() []uint {
	visited := map[uint]bool{}
	order := make([]uint, 0)

	cfg.dfs(cfg.Entry.id, visited, &order)
	return order
}

func (cfg *ControlFlowGraph) dfs(BlkID uint, visited map[uint]bool, order *[]uint) {
	visited[BlkID] = true
	if cfg.Suc[BlkID] != nil {
		for _, suc := range cfg.Suc[BlkID].Elems() {
			if _, found := visited[suc]; !found {
				cfg.dfs(suc, visited, order)
			}
		}
	}

	*order = append(*order, BlkID)
}

func (cfg *ControlFlowGraph) ReversePostOrder() []uint {
	pOrder := cfg.PostOrder()
	var revOrder []uint
	for i := len(pOrder) - 1; i >= 0; i-- {
		revOrder = append(revOrder, pOrder[i])
	}

	return revOrder
}

func (cfg *ControlFlowGraph) Reverse() *ControlFlowGraph {
	panic("not implemented")
}

func (cfg *ControlFlowGraph) DeleteBlocks(blocks ...*BasicBlock) {
	cfg.Nodes.Remove(blocks...)
	for _, block := range blocks {
		delete(cfg.Suc, block.id)
		delete(cfg.Pred, block.id)

		for _, suc := range cfg.Suc {
			for _, bb := range suc.Elems() {
				if bb == block.id {
					suc.Remove(block.id)
				}
			}
		}

		for _, pred := range cfg.Pred {
			for _, bb := range pred.Elems() {
				if bb == block.id {
					pred.Remove(block.id)
				}
			}
		}
	}
}
