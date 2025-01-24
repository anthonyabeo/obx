package analy

import (
	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/meer"
)

func ExtendedBasicBlocks(cfg *meer.ControlFlowGraph, src meer.BasicBlockID) map[meer.BasicBlockID]*adt.HashSet[meer.BasicBlockID] {
	ebbs := make(map[meer.BasicBlockID]*adt.HashSet[meer.BasicBlockID])
	roots := adt.NewQueue[meer.BasicBlockID]()

	roots.Enqueue(src)
	for !roots.Empty() {
		BB := roots.Dequeue()
		if _, exists := ebbs[BB]; !exists {
			s := adt.NewHashSet[meer.BasicBlockID]()
			extBasicBlocks(cfg, BB, roots, s)
			ebbs[BB] = s
		}
	}

	return ebbs
}

func extBasicBlocks(cfg *meer.ControlFlowGraph, blkID meer.BasicBlockID, roots *adt.Queue[meer.BasicBlockID], s *adt.HashSet[meer.BasicBlockID]) {
	s.Add(blkID)

	for _, BlockID := range cfg.Suc[blkID].Elems() {
		if cfg.Pred[BlockID].Size() == 1 && !s.Contains(BlockID) {
			extBasicBlocks(cfg, BlockID, roots, s)
		} else {
			roots.Enqueue(BlockID)
		}
	}
}

func ImmDominator(cfg *meer.ControlFlowGraph, Dominance map[meer.BasicBlockID]adt.Set[*meer.BasicBlock]) map[meer.BasicBlockID]*meer.BasicBlock {
	IDom := make(map[meer.BasicBlockID]*meer.BasicBlock)
	Tmp := make(map[meer.BasicBlockID]adt.Set[*meer.BasicBlock])

	for _, BB := range cfg.Nodes.Elems() {
		Tmp[BB.ID()] = Dominance[BB.ID()].Remove(BB)
	}

	for _, BB := range cfg.Nodes.Elems() {
		if BB.ID() == cfg.Entry.ID() {
			continue
		}

		for _, s := range Tmp[BB.ID()].Elems() {
			for _, t := range Tmp[BB.ID()].Elems() {
				if t == s {
					continue
				}

				if Tmp[s.ID()].Contains(t) {
					Tmp[BB.ID()].Remove(t)
				}
			}
		}
	}

	for _, BB := range cfg.Nodes.Elems() {
		if BB.ID() == cfg.Entry.ID() {
			continue
		}

		IDom[BB.ID()] = Tmp[BB.ID()].Pop()
	}

	return IDom
}

func Dominance(cfg *meer.ControlFlowGraph) map[meer.BasicBlockID]adt.Set[*meer.BasicBlock] {
	Dom := make(map[meer.BasicBlockID]adt.Set[*meer.BasicBlock])

	entrySet := adt.NewHashSet[*meer.BasicBlock]()
	entrySet.Add(cfg.Entry)
	Dom[cfg.Entry.ID()] = entrySet

	for _, BB := range cfg.Nodes.Elems() {
		if BB.ID() == cfg.Entry.ID() {
			continue
		}

		Dom[BB.ID()] = cfg.Nodes.Clone()
	}

	changed := true
	for changed {
		changed = false

		workList := adt.NewQueueFrom[meer.BasicBlockID](cfg.ReversePostOrder()[1:])
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

func DominanceFrontier(cfg *meer.ControlFlowGraph) map[meer.BasicBlockID]adt.Set[*meer.BasicBlock] {
	DF := make(map[meer.BasicBlockID]adt.Set[*meer.BasicBlock])
	for _, BB := range cfg.Nodes.Elems() {
		DF[BB.ID()] = adt.NewHashSet[*meer.BasicBlock]()
	}

	Dom := Dominance(cfg)
	IDom := ImmDominator(cfg, Dom)

	for _, BB := range cfg.Nodes.Elems() {
		if !cfg.IsJoinNode(BB.ID()) {
			continue
		}

		for _, pred := range cfg.Pred[BB.ID()].Elems() {
			runner := pred

			for runner != IDom[BB.ID()].ID() {
				DF[runner].Add(BB)
				if _, exists := IDom[runner]; exists {
					runner = IDom[runner].ID()
				}
			}
		}
	}

	return DF
}

func NaturalLoop(cfg *meer.ControlFlowGraph, m, n *meer.BasicBlock) adt.Set[meer.BasicBlockID] {
	Stack := adt.NewStack[meer.BasicBlockID]()
	Loop := adt.NewHashSet[meer.BasicBlockID]()
	Loop.Add(m.ID(), n.ID())

	if m != n {
		Stack.Push(m.ID())
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

func BuildCFG(program *meer.Program) *meer.ControlFlowGraph {
	Main := program.Units["Main"]

	blocks := findLeaders(Main.Inst)
	cfg := meer.NewCFG()
	cfg.Entry = blocks[1]

	for id, block := range blocks {
		cfg.Nodes.Add(block)
		cfg.Blocks[id] = block

		lastInstr := block.LastInst()

		switch last := lastInstr.(type) {
		case *meer.JumpInst:
			cfg.AddSuc(block.ID(), last.Dst.BlockID)
			cfg.AddPred(last.Dst.BlockID, block.ID())
		case *meer.CondBrInst:
			cfg.AddSuc(block.ID(), last.IfTrue.BlockID, last.IfFalse.BlockID)

			cfg.AddPred(last.IfTrue.BlockID, block.ID())
			cfg.AddPred(last.IfFalse.BlockID, block.ID())
		default:
			fallthroughBlockID := id + 1
			if blocks[fallthroughBlockID] != nil {
				cfg.AddSuc(block.ID(), fallthroughBlockID)
				cfg.AddPred(fallthroughBlockID, block.ID())
			}
		}
	}

	Main.CFG = cfg

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

func findLeaders(instructions []meer.Instruction) map[meer.BasicBlockID]*meer.BasicBlock {
	Blocks := make(map[meer.BasicBlockID]*meer.BasicBlock)

	first := instructions[0].(*meer.Label)
	BB := meer.CreateBasicBlock(first)

	for _, inst := range instructions[1:] {
		i, ok := inst.(*meer.Label)
		if !ok {
			BB.AddInstr(inst)
			continue
		}

		if ok || isTerm(inst) {
			Blocks[BB.ID()] = BB
			BB = meer.CreateBasicBlock(i)
		}
	}

	Blocks[BB.ID()] = BB

	return Blocks
}
