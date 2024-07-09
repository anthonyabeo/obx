package analy

import (
	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

func ExtendedBasicBlocks(cfg *tacil.ControlFlowGraph, src *tacil.BasicBlock) map[string]*adt.HashSet[*tacil.BasicBlock] {
	ebbs := make(map[string]*adt.HashSet[*tacil.BasicBlock])
	roots := adt.NewQueue[*tacil.BasicBlock]()

	roots.Enqueue(src)
	for !roots.Empty() {
		BB := roots.Dequeue()
		if _, exists := ebbs[BB.Name()]; !exists {
			s := adt.NewHashSet[*tacil.BasicBlock]()
			extBasicBlocks(cfg, BB, roots, s)
			ebbs[BB.Name()] = s
		}
	}

	return ebbs
}

func extBasicBlocks(cfg *tacil.ControlFlowGraph, blk *tacil.BasicBlock, roots *adt.Queue[*tacil.BasicBlock], s *adt.HashSet[*tacil.BasicBlock]) {
	s.Add(blk)

	for _, bb := range cfg.Succ[blk.Name()].Elems() {
		if cfg.Pred[bb.Name()].Size() == 1 && !s.Contains(bb) {
			extBasicBlocks(cfg, bb, roots, s)
		} else {
			roots.Enqueue(bb)
		}
	}
}

func ImmDominator(cfg *tacil.ControlFlowGraph, Dominance map[string]adt.Set[*tacil.BasicBlock]) map[string]*tacil.BasicBlock {
	IDom := make(map[string]*tacil.BasicBlock)
	Tmp := make(map[string]adt.Set[*tacil.BasicBlock])

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

func Dominance(cfg *tacil.ControlFlowGraph) map[string]adt.Set[*tacil.BasicBlock] {
	Dom := make(map[string]adt.Set[*tacil.BasicBlock])

	entrySet := adt.NewHashSet[*tacil.BasicBlock]()
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

		workList := adt.NewQueueFrom[*tacil.BasicBlock](cfg.ReversePostOrder()[1:])
		for !workList.Empty() {
			BB := workList.Dequeue()

			var Temp adt.Set[*tacil.BasicBlock] = cfg.Nodes
			for _, pred := range cfg.Pred[BB.Name()].Elems() {
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

func DominanceFrontier(cfg *tacil.ControlFlowGraph) map[string]adt.Set[*tacil.BasicBlock] {
	DF := make(map[string]adt.Set[*tacil.BasicBlock])
	for _, BB := range cfg.Nodes.Elems() {
		DF[BB.Name()] = adt.NewHashSet[*tacil.BasicBlock]()
	}

	Dom := Dominance(cfg)
	IDom := ImmDominator(cfg, Dom)

	for _, BB := range cfg.Nodes.Elems() {
		if !cfg.IsJoinNode(BB.Name()) {
			continue
		}

		for _, pred := range cfg.Pred[BB.Name()].Elems() {
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

func NaturalLoop(cfg *tacil.ControlFlowGraph, m, n *tacil.BasicBlock) adt.Set[*tacil.BasicBlock] {
	Stack := adt.NewStack[*tacil.BasicBlock]()
	Loop := adt.NewHashSet[*tacil.BasicBlock]()
	Loop.Add(m, n)

	if m != n {
		Stack.Push(m)
	}

	for !Stack.Empty() {
		p := Stack.Pop()
		for _, q := range cfg.Pred[p.Name()].Elems() {
			if !Loop.Contains(q) {
				Loop.Add(q)
				Stack.Push(q)
			}
		}
	}

	return Loop
}
