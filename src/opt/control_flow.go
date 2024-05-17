package opt

import "github.com/anthonyabeo/obx/src/translate/ir"

func BuildExtBB(cfg *ir.ControlFlowGraph, src string, EBBRoots *[]string) ir.SetOfBBs {
	set := ir.SetOfBBs{}
	compExtBBs(cfg, src, set, EBBRoots)

	return set
}

func compExtBBs(cfg *ir.ControlFlowGraph, blk string, ebb ir.SetOfBBs, EBBRoots *[]string) {
	ebb.Add(blk)
	for _, bb := range cfg.Succ[blk] {
		if len(cfg.Pred[bb.Name()]) == 1 && !ebb.Contains(bb) {
			compExtBBs(cfg, bb.Name(), ebb, EBBRoots)
		} else {
			*EBBRoots = append(*EBBRoots, bb.Name())
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
			AllEBBs[x] = BuildExtBB(cfg, x, &EBBRoots)
		}
	}

	return AllEBBs
}
