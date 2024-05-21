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

func Dominator(cfg *ir.ControlFlowGraph, r *ir.BasicBlock) map[string]ir.SetOfBBs {
	change := true
	Dominance := map[string]ir.SetOfBBs{}
	Dominance[r.Name()] = ir.SetOfBBs{r.Name(): r}

	for name := range cfg.Nodes {
		if name == r.Name() {
			continue
		}

		Dominance[name] = cfg.Nodes
	}

	var nodes []string
	nodes = append(nodes, cfg.Succ[r.Name()]...)
	visited := map[string]bool{r.Name(): true}

	for change {
		change = false
		for _, name := range nodes {
			T := cfg.Nodes

			for _, pred := range cfg.Pred[name] {
				T = T.Intersection(Dominance[pred])
			}

			T.Add(cfg.Nodes[name])
			if !T.Equal(Dominance[name]) {
				change = true
				Dominance[name] = T
			}

			for _, n := range cfg.Succ[name] {
				if _, found := visited[n]; !found {
					nodes = append(nodes, n)
					visited[n] = true
				}
			}

			nodes = nodes[1:]
		}
	}

	return Dominance
}

func ImmDominator() {

}
