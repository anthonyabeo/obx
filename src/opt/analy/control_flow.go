package analy

import (
	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

func ExtendedBasicBlocks(cfg *ir.ControlFlowGraph, src *ir.BasicBlock) map[string]ir.SetOfBBs {
	ebbs := map[string]ir.SetOfBBs{}

	roots := adt.NewQueue[string]()
	roots.Enqueue(src.Name())
	for !roots.Empty() {
		x := roots.Dequeue()
		if _, exists := ebbs[x]; !exists {
			s := ir.SetOfBBs{}
			extBasicBlocks(cfg, cfg.Nodes[x], roots, s)
			ebbs[x] = s
		}
	}

	return ebbs
}

func extBasicBlocks(cfg *ir.ControlFlowGraph, blk *ir.BasicBlock, roots *adt.Queue[string], s ir.SetOfBBs) {
	s.Add(blk)

	for _, bb := range cfg.Succ[blk.Name()] {
		if len(cfg.Pred[bb]) == 1 && !s.Contains(bb) {
			extBasicBlocks(cfg, cfg.Nodes[bb], roots, s)
		} else {
			roots.Enqueue(bb)
		}
	}
}

func ImmDominator(cfg *ir.ControlFlowGraph, Dominance map[string]ir.SetOfBBs) ir.SetOfBBs {
	IDom := make(map[string]*ir.BasicBlock)
	Tmp := make(map[string]ir.SetOfBBs)

	for name := range cfg.Nodes {
		Tmp[name] = Dominance[name].Remove(name)
	}

	for name := range cfg.Nodes {
		if name == cfg.Entry.Name() {
			continue
		}

		for s := range Tmp[name] {
			for t := range Tmp[name] {
				if t == s {
					continue
				}

				if Tmp[s].Contains(t) {
					Tmp[name].Remove(t)
				}
			}
		}

	}

	for name := range cfg.Nodes {
		if name == cfg.Entry.Name() {
			continue
		}

		IDom[name] = Tmp[name].Pop()
	}

	return IDom
}

func Dominance(cfg *ir.ControlFlowGraph) map[string]ir.SetOfBBs {
	Dom := map[string]ir.SetOfBBs{}
	Dom[cfg.Entry.Name()] = ir.SetOfBBs{cfg.Entry.Name(): cfg.Entry}

	for name := range cfg.Nodes {
		if name == cfg.Entry.Name() {
			continue
		}

		Dom[name] = cfg.Nodes.Clone()
	}

	changed := true
	for changed {
		changed = false

		for name := range cfg.Nodes {
			if name == cfg.Entry.Name() {
				continue
			}

			Nodes := cfg.Nodes
			for _, pred := range cfg.Pred[name] {
				Nodes = Nodes.Intersection(Dom[pred])
			}
			Nodes.Add(cfg.Nodes[name])

			if !Nodes.Equal(Dom[name]) {
				changed = true
				Dom[name] = Nodes
			}
		}
	}

	return Dom
}

func DominanceFrontier(cfg *ir.ControlFlowGraph) map[string]ir.SetOfBBs {
	DF := map[string]ir.SetOfBBs{}
	for name := range cfg.Nodes {
		DF[name] = ir.SetOfBBs{}
	}

	Dom := Dominance(cfg)
	IDom := ImmDominator(cfg, Dom)

	for name := range cfg.Nodes {
		if !cfg.IsJoinNode(name) {
			continue
		}

		for _, pred := range cfg.Pred[name] {
			runner := pred

			for runner != IDom[name].Name() {
				DF[runner].Add(cfg.Nodes[name])
				if _, exists := IDom[runner]; exists {
					runner = IDom[runner].Name()
				}

			}
		}

	}

	return DF
}

func NaturalLoop(cfg *ir.ControlFlowGraph, m, n string) ir.SetOfBBs {
	Stack := adt.NewStack[string]()
	Loop := ir.SetOfBBs{}
	Loop.Add(cfg.Nodes[m], cfg.Nodes[n])

	if m != n {
		Stack.Push(m)
	}

	for !Stack.Empty() {
		p := Stack.Pop()
		for _, q := range cfg.Pred[p] {
			if !Loop.Contains(q) {
				Loop.Add(cfg.Nodes[q])
				Stack.Push(q)
			}
		}
	}

	return Loop
}
