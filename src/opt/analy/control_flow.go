package analy

import (
	"bytes"
	"container/list"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/meer"
)

func ExtendedBasicBlocks(cfg *ControlFlowGraph, src uint) map[uint]*adt.HashSet[uint] {
	ebbs := make(map[uint]*adt.HashSet[uint])
	roots := adt.NewQueue[uint]()

	roots.Enqueue(src)
	for !roots.Empty() {
		BB := roots.Dequeue()
		if _, exists := ebbs[BB]; !exists {
			s := adt.NewHashSet[uint]()
			extBasicBlocks(cfg, BB, roots, s)
			ebbs[BB] = s
		}
	}

	return ebbs
}

func extBasicBlocks(cfg *ControlFlowGraph, blkID uint, roots *adt.Queue[uint], s *adt.HashSet[uint]) {
	s.Add(blkID)

	for _, BlockID := range cfg.Suc[blkID].Elems() {
		if cfg.Pred[BlockID].Size() == 1 && !s.Contains(BlockID) {
			extBasicBlocks(cfg, BlockID, roots, s)
		} else {
			roots.Enqueue(BlockID)
		}
	}
}

func ImmDominator(cfg *ControlFlowGraph, Dominance map[uint]adt.Set[*BasicBlock]) map[uint]*BasicBlock {
	IDom := make(map[uint]*BasicBlock)
	Tmp := make(map[uint]adt.Set[*BasicBlock])

	for _, BB := range cfg.Nodes.Elems() {
		Tmp[BB.id] = Dominance[BB.id].Remove(BB)
	}

	for _, BB := range cfg.Nodes.Elems() {
		if BB.id == cfg.Entry.id {
			continue
		}

		for _, s := range Tmp[BB.id].Elems() {
			for _, t := range Tmp[BB.id].Elems() {
				if t == s {
					continue
				}

				if Tmp[s.id].Contains(t) {
					Tmp[BB.id].Remove(t)
				}
			}
		}
	}

	for _, BB := range cfg.Nodes.Elems() {
		if BB.id == cfg.Entry.id {
			continue
		}

		IDom[BB.id] = Tmp[BB.id].Pop()
	}

	return IDom
}

func Dominance(cfg *ControlFlowGraph) map[uint]adt.Set[*BasicBlock] {
	Dom := make(map[uint]adt.Set[*BasicBlock])

	entrySet := adt.NewHashSet[*BasicBlock]()
	entrySet.Add(cfg.Entry)
	Dom[cfg.Entry.id] = entrySet

	for _, BB := range cfg.Nodes.Elems() {
		if BB.id == cfg.Entry.id {
			continue
		}

		Dom[BB.id] = cfg.Nodes.Clone()
	}

	changed := true
	for changed {
		changed = false

		workList := adt.NewQueueFrom[uint](cfg.ReversePostOrder()[1:])
		for !workList.Empty() {
			BB := workList.Dequeue()

			var Temp = cfg.Nodes
			for _, pred := range cfg.Pred[BB].Elems() {
				Temp = Temp.Intersect(Dom[pred])
			}
			Temp.Add(cfg.Blocks[BB])

			if !Temp.Equal(Dom[BB]) {
				Dom[BB] = Temp
				changed = true
			}
		}
	}

	return Dom
}

func DominanceFrontier(cfg *ControlFlowGraph) map[uint]adt.Set[*BasicBlock] {
	DF := make(map[uint]adt.Set[*BasicBlock])
	for _, BB := range cfg.Nodes.Elems() {
		DF[BB.id] = adt.NewHashSet[*BasicBlock]()
	}

	Dom := Dominance(cfg)
	IDom := ImmDominator(cfg, Dom)

	for _, BB := range cfg.Nodes.Elems() {
		if !cfg.IsJoinNode(BB.id) {
			continue
		}

		for _, pred := range cfg.Pred[BB.id].Elems() {
			runner := pred

			for runner != IDom[BB.id].id {
				DF[runner].Add(BB)
				if _, exists := IDom[runner]; exists {
					runner = IDom[runner].id
				}
			}
		}
	}

	return DF
}

func NaturalLoop(cfg *ControlFlowGraph, m, n *BasicBlock) adt.Set[uint] {
	Stack := adt.NewStack[uint]()
	Loop := adt.NewHashSet[uint]()
	Loop.Add(m.id, n.id)

	if m != n {
		Stack.Push(m.id)
	}

	for !Stack.Empty() {
		p := Stack.Pop()
		for _, q := range cfg.Pred[p].Elems() {
			if !Loop.Contains(q) {
				Loop.Add(q)
				Stack.Push(q)
			}
		}
	}

	return Loop
}

func BuildCFG(program *meer.Program) *ControlFlowGraph {
	Main := program.Units["Main"]

	blocks := findLeaders(Main.Inst)
	cfg := NewCFG()

	for id, block := range blocks {
		cfg.Nodes.Add(block)

		lastInstr := block.LastInst()

		switch last := lastInstr.(type) {
		case *meer.JumpInst:
			cfg.AddSuc(block.id, last.Dst.BlockID)
			cfg.AddPred(last.Dst.BlockID, block.id)
		case *meer.CondBrInst:
			cfg.AddSuc(block.id, last.IfTrue.BlockID, last.IfFalse.BlockID)

			cfg.AddPred(last.IfTrue.BlockID, block.id)
			cfg.AddPred(last.IfFalse.BlockID, block.id)
		default:
			fallthroughBlockID := id + 1
			if blocks[fallthroughBlockID] != nil {
				cfg.AddSuc(block.id, fallthroughBlockID)
				cfg.AddPred(fallthroughBlockID, block.id)
			}
		}
	}

	return cfg
}

func isTerm(i meer.Instruction) bool {
	switch i.(type) {
	case *meer.JumpInst, *meer.CondBrInst, *meer.ReturnInst:
		return true
	default:
		return false
	}
}

func findLeaders(instructions []meer.Instruction) map[uint]*BasicBlock {
	Blocks := make(map[uint]*BasicBlock)

	first := instructions[0].(*meer.Label)
	BB := CreateBasicBlock(first)
	first.BlockID = BB.id

	for _, inst := range instructions[1:] {
		i, ok := inst.(*meer.Label)
		if !ok {
			BB.AddInstr(inst)
			continue
		}

		if ok || isTerm(inst) {
			Blocks[BB.id] = BB
			BB = CreateBasicBlock(i)
		}
	}

	Blocks[BB.id] = BB

	return Blocks
}

// BasicBlock ...
// -------------------------------
type BasicBlock struct {
	id    uint
	name  string
	instr *list.List
}

var nb uint = 1

func NewBasicBlock(lbl *meer.Label) *BasicBlock {
	blk := &BasicBlock{
		name: lbl.Name,
		id:   nb,
	}

	lbl.BlockID = nb

	nb++

	return blk
}

var nextBlock uint = 1

func CreateBasicBlock(lbl *meer.Label) *BasicBlock {
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
		i := inst.Value.(meer.Instruction)
		if inst.Next() != nil {
			s += fmt.Sprintf("%s\n\t", i)
		} else {
			s += fmt.Sprintf("%s\n\n", i)
		}
	}

	return s
}
func (b *BasicBlock) Instr() *list.List { return b.instr }
func (b *BasicBlock) InsertInstrBegin(inst meer.Instruction) {
	b.instr.InsertBefore(inst, b.instr.Front())
}
func (b *BasicBlock) RemoveInstr(rem *list.Element) { b.instr.Remove(rem) }
func (b *BasicBlock) Empty() bool {
	if b.instr.Len() != 1 {
		return false
	}

	_, ok := b.instr.Front().Value.(*meer.JumpInst)
	return ok
}
func (b *BasicBlock) LastInst() meer.Instruction  { return b.instr.Back().Value.(meer.Instruction) }
func (b *BasicBlock) AddInstr(i meer.Instruction) { b.instr.PushBack(i) }
func (b *BasicBlock) LastInstIsCondBr() bool {
	_, ok := b.LastInst().(*meer.CondBrInst)
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
