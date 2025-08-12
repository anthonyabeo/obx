package opt

import (
	"sort"

	"github.com/anthonyabeo/obx/src/ir/mir"
)

func BuildCFG(fn *mir.Function) {
	labelToBlock := map[string]*mir.Block{}
	for _, blk := range fn.Blocks {
		labelToBlock[blk.Label] = blk
	}

	keys := make([]int, 0, len(fn.Blocks))
	for k := range fn.Blocks {
		keys = append(keys, k)
	}
	sort.Ints(keys)

	// determine succs and preds per block
	for _, i := range keys {
		blk := fn.Blocks[i]
		if blk.Term != nil {
			switch t := blk.Term.(type) {
			case *mir.CondBrInst:
				if b := labelToBlock[t.TrueLabel]; b != nil {
					blk.Succs[b.ID] = b
				}
				if b := labelToBlock[t.FalseLabel]; b != nil {
					blk.Succs[b.ID] = b
				}
			case *mir.JumpInst:
				if b := labelToBlock[t.Target]; b != nil {
					blk.Succs[b.ID] = b
				}
			case *mir.ReturnInst:
				// no succs
			default:
				// unknown terminator: assume fallthrough
				if i+1 < len(fn.Blocks) {
					blk.Instrs = append(blk.Instrs, &mir.JumpInst{Target: fn.Blocks[i+1].Label})
					blk.Succs[i+1] = fn.Blocks[i+1]
				}
			}
		} else {
			// no explicit terminator: fallthrough
			if i+1 < len(fn.Blocks) {
				blk.Instrs = append(blk.Instrs, &mir.JumpInst{Target: fn.Blocks[i+1].Label})
				blk.Succs[i+1] = fn.Blocks[i+1]
			}
		}
	}

	// fill preds
	for _, blk := range fn.Blocks {
		for _, s := range blk.Succs {
			s.Preds[blk.ID] = blk
		}
	}

	CleanCFG(fn)
}

func CleanCFG(f *mir.Function) {
	changed := true
	for changed {
		changed = false

		if EliminateDeadBlocks(f) {
			changed = true
		}
		if RemoveEmptyBlocks(f) {
			changed = true
		}
		if FoldRedundantBranches(f) {
			changed = true
		}
		if HoistBranch(f) {
			changed = true
		}
		if CombineBlocks(f) {
			changed = true
		}
	}
}

func ComputeDominators(fn *mir.Function) map[*mir.Block]map[*mir.Block]struct{} {
	doms := make(map[*mir.Block]map[*mir.Block]struct{})

	blocks := fn.Blocks

	// Step 1: initialize
	for _, b := range blocks {
		doms[b] = make(map[*mir.Block]struct{})
		if b == fn.Entry {
			doms[b][b] = struct{}{}
		} else {
			// Start with all blocks
			for _, bb := range blocks {
				doms[b][bb] = struct{}{}
			}
		}
	}

	changed := true
	for changed {
		changed = false
		for _, b := range blocks {
			if b == fn.Entry {
				continue
			}
			// Intersection of predecessors
			newDom := intersectAllPreds(doms, b.Predecessors())
			// Add self
			newDom[b] = struct{}{}

			if !sameSet(doms[b], newDom) {
				doms[b] = newDom
				changed = true
			}
		}
	}

	return doms
}

func ImmediateDominators(fn *mir.Function, doms map[*mir.Block]map[*mir.Block]struct{}) map[*mir.Block]*mir.Block {
	idom := make(map[*mir.Block]*mir.Block)
	for _, b := range fn.Blocks {
		if b == fn.Entry {
			idom[b] = nil // Entry has no idom
			continue
		}

		// candidates = dominators except b itself
		candidates := make(map[*mir.Block]struct{})
		for d := range doms[b] {
			if d != b {
				candidates[d] = struct{}{}
			}
		}

		// pick the one that is not dominated by any other candidate
		var immediate *mir.Block
		for c := range candidates {
			isImmediate := true
			for other := range candidates {
				if other != c {
					if _, ok := doms[other][c]; ok {
						// c is dominated by other, so it's not immediate
						isImmediate = false
						break
					}
				}
			}
			if isImmediate {
				immediate = c
				break
			}
		}
		idom[b] = immediate
	}

	fn.Dom.IDom = idom
	fn.Dom.DomTree = DominatorTree(idom)

	return idom
}

func DominatorTree(idom map[*mir.Block]*mir.Block) map[*mir.Block][]*mir.Block {
	tree := make(map[*mir.Block][]*mir.Block)
	for b, parent := range idom {
		if parent != nil {
			tree[parent] = append(tree[parent], b)
		}
	}
	return tree
}

func ComputeDF(fn *mir.Function, idom map[*mir.Block]*mir.Block) map[*mir.Block][]*mir.Block {
	df := make(map[*mir.Block][]*mir.Block)

	for _, b := range fn.Blocks {
		if b.IsJoinBlock() {
			for _, p := range b.Preds {
				runner := p
				for runner != idom[b] {
					if !contains(df[runner], b) {
						df[runner] = append(df[runner], b)
					}
					runner = idom[runner]
				}
			}
		}
	}

	fn.Dom.DF = df
	return df
}
