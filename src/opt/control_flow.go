package opt

import "github.com/anthonyabeo/obx/src/translate/ir"

func BuildExtBB(cfg *ir.ControlFlowGraph, src *ir.BasicBlock, EBBRoots *[]string) ir.SetOfBBs {
	set := ir.SetOfBBs{}
	compExtBBs(cfg, src, set, EBBRoots)

	return set
}

func compExtBBs(cfg *ir.ControlFlowGraph, blk *ir.BasicBlock, ebb ir.SetOfBBs, EBBRoots *[]string) {
	ebb.Add(blk)
	for _, bb := range cfg.Succ[blk.Name()] {
		if len(cfg.Pred[bb]) == 1 && !ebb.Contains(bb) {
			compExtBBs(cfg, cfg.Nodes[bb], ebb, EBBRoots)
		} else {
			*EBBRoots = append(*EBBRoots, bb)
		}
	}
}

func ComputeAllExtBB(cfg *ir.ControlFlowGraph, src *ir.BasicBlock) map[string]ir.SetOfBBs {
	AllEBBs := map[string]ir.SetOfBBs{}
	EBBRoots := []string{src.Name()}

	for len(EBBRoots) != 0 {
		x := EBBRoots[0]
		EBBRoots = EBBRoots[1:]

		if _, exists := AllEBBs[x]; !exists {
			AllEBBs[x] = BuildExtBB(cfg, cfg.Nodes[x], &EBBRoots)
		}
	}

	return AllEBBs
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

		Dom[name] = cfg.Nodes
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
	Stack := make([]string, 0)
	Loop := ir.SetOfBBs{}
	Loop.Add(cfg.Nodes[m], cfg.Nodes[n])

	if m != n {
		Stack = append(Stack, m)
	}

	for len(Stack) != 0 {
		p := Stack[len(Stack)-1]
		Stack = Stack[:len(Stack)-1]

		for _, q := range cfg.Pred[p] {
			if !Loop.Contains(q) {
				Loop.Add(cfg.Nodes[q])
				Stack = append(Stack, q)
			}
		}

	}

	return Loop
}
