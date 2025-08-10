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

//func ComputeDominators(fn *mir.Function) map[*mir.Block]map[*mir.Block]bool {
//	blocks := fn.Blocks
//	dom := map[*mir.Block]map[*mir.Block]bool{}
//	all := map[*mir.Block]bool{}
//	for _, b := range blocks {
//		all[b] = true
//	}
//
//	// init
//	for _, b := range blocks {
//		dom[b] = map[*mir.Block]bool{}
//		if b == fn.Entry {
//			dom[b][b] = true
//		} else {
//			for bb := range all {
//				dom[b][bb] = true
//			}
//		}
//	}
//
//	changed := true
//	for changed {
//		changed = false
//		for _, b := range blocks {
//			if b == fn.Entry {
//				continue
//			}
//			// intersect doms of preds
//			newdom := copySet(all) // start with all, then intersect
//			first := true
//			for _, p := range b.Preds {
//				if first {
//					newdom = copyMap(dom[p])
//					first = false
//				} else {
//					newdom = intersect(newdom, dom[p])
//				}
//			}
//			newdom[b] = true
//			if !equalSets(newdom, dom[b]) {
//				dom[b] = newdom
//				changed = true
//			}
//		}
//	}
//	return dom
//}

//func ComputeDominanceFrontiers(fn *mir.Function, idom map[*mir.Block]*mir.Block) map[*mir.Block][]*mir.Block {
//	// compute children in dominator tree
//	children := map[*mir.Block][]*mir.Block{}
//	for b, p := range idom {
//		if p != nil {
//			children[p] = append(children[p], b)
//		}
//	}
//	DF := map[*mir.Block]map[*mir.Block]bool{}
//	for _, b := range fn.Blocks {
//		DF[b] = map[*mir.Block]bool{}
//	}
//
//	// for each block b:
//	for _, b := range fn.Blocks {
//		if len(b.Preds) >= 2 {
//			for _, p := range b.Preds {
//				runner := p
//				for runner != nil && runner != idom[b] {
//					DF[runner][b] = true
//					runner = idom[runner]
//				}
//			}
//		}
//	}
//	// convert map->slice
//	result := map[*mir.Block][]*mir.Block{}
//	for b, set := range DF {
//		for x := range set {
//			result[b] = append(result[b], x)
//		}
//	}
//	return result
//}
