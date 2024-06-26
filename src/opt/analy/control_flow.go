package analy

import (
	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

func ExtendedBasicBlocks(cfg *ir.ControlFlowGraph, src *ir.BasicBlock) map[string]*adt.HashSet[*ir.BasicBlock] {
	ebbs := make(map[string]*adt.HashSet[*ir.BasicBlock])
	roots := adt.NewQueue[*ir.BasicBlock]()

	roots.Enqueue(src)
	for !roots.Empty() {
		BB := roots.Dequeue()
		if _, exists := ebbs[BB.Name()]; !exists {
			s := adt.NewHashSet[*ir.BasicBlock]()
			extBasicBlocks(cfg, BB, roots, s)
			ebbs[BB.Name()] = s
		}
	}

	return ebbs
}

func extBasicBlocks(cfg *ir.ControlFlowGraph, blk *ir.BasicBlock, roots *adt.Queue[*ir.BasicBlock], s *adt.HashSet[*ir.BasicBlock]) {
	s.Add(blk)

	for _, bb := range cfg.Succ[blk.Name()] {
		if len(cfg.Pred[bb.Name()]) == 1 && !s.Contains(bb) {
			extBasicBlocks(cfg, bb, roots, s)
		} else {
			roots.Enqueue(bb)
		}
	}
}

func ImmDominator(cfg *ir.ControlFlowGraph, Dominance map[string]adt.Set[*ir.BasicBlock]) map[string]*ir.BasicBlock {
	IDom := make(map[string]*ir.BasicBlock)
	Tmp := make(map[string]adt.Set[*ir.BasicBlock])

	for _, BB := range cfg.Nodes.Elems() {
		Tmp[BB.Name()] = Dominance[BB.Name()].Remove(BB)
	}

	for _, BB := range cfg.Nodes.Elems() {
		if BB.Name() == cfg.Entry.Name() {
			continue
		}

		for _, s := range Tmp[BB.Name()].Elems() {
			for _, t := range Tmp[BB.Name()].Elems() {
				if t == s {
					continue
				}

				if Tmp[s.Name()].Contains(t) {
					Tmp[BB.Name()].Remove(t)
				}
			}
		}
	}

	for _, BB := range cfg.Nodes.Elems() {
		if BB.Name() == cfg.Entry.Name() {
			continue
		}

		IDom[BB.Name()] = Tmp[BB.Name()].Pop()
	}

	return IDom
}

func Dominance(cfg *ir.ControlFlowGraph) map[string]adt.Set[*ir.BasicBlock] {
	Dom := make(map[string]adt.Set[*ir.BasicBlock])

	entrySet := adt.NewHashSet[*ir.BasicBlock]()
	entrySet.Add(cfg.Entry)
	Dom[cfg.Entry.Name()] = entrySet

	for _, BB := range cfg.Nodes.Elems() {
		if BB.Name() == cfg.Entry.Name() {
			continue
		}

		Dom[BB.Name()] = cfg.Nodes.Clone()
	}

	changed := true
	for changed {
		changed = false

		workList := adt.NewQueueFrom[*ir.BasicBlock](cfg.ReversePostOrder()[1:])
		for !workList.Empty() {
			BB := workList.Dequeue()

			var Temp adt.Set[*ir.BasicBlock] = cfg.Nodes
			for _, pred := range cfg.Pred[BB.Name()] {
				Temp = Temp.Intersect(Dom[pred.Name()])
			}
			Temp.Add(BB)

			if !Temp.Equal(Dom[BB.Name()]) {
				Dom[BB.Name()] = Temp
				changed = true
			}
		}
	}

	return Dom
}

func DominanceFrontier(cfg *ir.ControlFlowGraph) map[string]adt.Set[*ir.BasicBlock] {
	DF := make(map[string]adt.Set[*ir.BasicBlock])
	for _, BB := range cfg.Nodes.Elems() {
		DF[BB.Name()] = adt.NewHashSet[*ir.BasicBlock]()
	}

	Dom := Dominance(cfg)
	IDom := ImmDominator(cfg, Dom)

	for _, BB := range cfg.Nodes.Elems() {
		if !cfg.IsJoinNode(BB.Name()) {
			continue
		}

		for _, pred := range cfg.Pred[BB.Name()] {
			runner := pred

			for runner.Name() != IDom[BB.Name()].Name() {
				DF[runner.Name()].Add(BB)
				if _, exists := IDom[runner.Name()]; exists {
					runner = IDom[runner.Name()]
				}
			}
		}
	}

	return DF
}

func NaturalLoop(cfg *ir.ControlFlowGraph, m, n *ir.BasicBlock) adt.Set[*ir.BasicBlock] {
	Stack := adt.NewStack[*ir.BasicBlock]()
	Loop := adt.NewHashSet[*ir.BasicBlock]()
	Loop.Add(m, n)

	if m != n {
		Stack.Push(m)
	}

	for !Stack.Empty() {
		p := Stack.Pop()
		for _, q := range cfg.Pred[p.Name()] {
			if !Loop.Contains(q) {
				Loop.Add(q)
				Stack.Push(q)
			}
		}
	}

	return Loop
}
