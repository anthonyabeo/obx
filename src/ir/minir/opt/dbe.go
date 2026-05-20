package miniropt

import "github.com/anthonyabeo/obx/src/ir/minir"

// CleanCFG runs all five CFG optimisation passes to a fixed point and returns
// the total number of structural changes made across all passes.
//
// Pass order:
//  1. RemoveDeadBlocks  – prune unreachable blocks (BFS from entry).
//  2. FoldRedundantBranches – CondBr{true==false} → JumpInst.
//  3. HoistBranch       – inline single-instruction CondBr successor.
//  4. CombineBlocks     – merge Bi→Bj when Bj has exactly one predecessor.
//  5. RemoveEmptyBlocks – bypass Bi that only contains a jump to Bj.
//
// The loop repeats until none of the passes reports a change.
func CleanCFG(fn *minir.Function) int {
	total := 0
	for {
		n := RemoveDeadBlocks(fn)
		changed := FoldRedundantBranches(fn)
		changed = HoistBranch(fn) || changed
		changed = CombineBlocks(fn) || changed
		changed = RemoveEmptyBlocks(fn) || changed
		total += n
		if n == 0 && !changed {
			break
		}
	}
	return total
}

// RemoveDeadBlocks eliminates blocks that are unreachable from fn.Entry by
// performing a BFS over the Succs edges.  fn.Exit is always preserved (even
// when unreachable) to satisfy the verifier's invariant that fn.Exit ∈
// fn.Blocks.  Returns the number of blocks removed.
func RemoveDeadBlocks(fn *minir.Function) int {
	if fn.Entry == nil {
		return 0
	}

	reachable := make(map[int]bool)
	worklist := []*minir.Block{fn.Entry}
	reachable[fn.Entry.ID] = true

	// Always keep fn.Exit in reachable so it is never deleted.
	if fn.Exit != nil {
		reachable[fn.Exit.ID] = true
	}

	for len(worklist) > 0 {
		b := worklist[0]
		worklist = worklist[1:]
		for id, succ := range b.Succs {
			if !reachable[id] {
				reachable[id] = true
				worklist = append(worklist, succ)
			}
		}
	}

	// Build replacement block map keeping only reachable blocks.
	newBlocks := make(map[int]*minir.Block, len(reachable))
	removed := 0
	for id, b := range fn.Blocks {
		if reachable[id] {
			// Prune Preds/Succs references to dead blocks and keep ordering slices
			// consistent.
			b.Preds = filterBlocks(b.Preds, reachable)
			b.Succs = filterBlocks(b.Succs, reachable)
			b.PredOrder = filterOrder(b.PredOrder, reachable)
			b.SuccOrder = filterOrder(b.SuccOrder, reachable)
			newBlocks[id] = b
		} else {
			removed++
		}
	}

	fn.Blocks = newBlocks
	return removed
}

// CombineBlocks merges Bi → Bj into Bi when Bj has exactly one predecessor
// (Bi).  After the merge, Bj is removed from fn.Blocks.  Returns true when
// at least one merge was performed.
func CombineBlocks(fn *minir.Function) bool {
	changed := false
	for _, bi := range fn.Blocks {
		if len(bi.Instrs) == 0 {
			continue
		}
		jmp, ok := bi.Term.(*minir.JumpInst)
		if !ok {
			continue
		}
		bj := fn.GetBlock(jmp.Target)
		if bj == nil {
			continue
		}
		// Only merge when Bj has exactly one predecessor (which must be Bi).
		if len(bj.Preds) != 1 {
			continue
		}
		if _, hasPred := bj.Preds[bi.ID]; !hasPred {
			continue
		}
		// Don't collapse the exit block into its predecessor.
		if fn.Exit != nil && bj.ID == fn.Exit.ID {
			continue
		}

		// Drop the trailing jump from Bi and append Bj's instructions.
		bi.Instrs = bi.Instrs[:len(bi.Instrs)-1]
		bi.Instrs = append(bi.Instrs, bj.Instrs...)
		bi.Term = bj.Term
		bi.SuccOrder = bj.SuccOrder

		// Reroute Bj's successors to Bi.
		bi.Succs = bj.Succs
		for _, succ := range bj.Succs {
			for _, pred := range succ.Preds {
				if pred == bj {
					fn.ReplaceEdge(bi, bj, succ)
					for _, ins := range succ.Instrs {
						if phi, ok := ins.(*minir.PhiInst); ok {
							phi.ReplaceIncomingBlock(bj.Label, bi.Label)
						}
					}
				}
			}
		}

		fn.RemoveBlock(bj)
		changed = true
	}
	return changed
}

// RemoveEmptyBlocks bypasses blocks that contain only a single unconditional
// jump (i.e., empty dispatch blocks).  All predecessors of Bi are rewired to
// jump directly to Bj.  Bi is then removed from fn.Blocks. fn.Entry and
// fn.Exit are never bypassed.  Returns true when at least one block was
// removed.
func RemoveEmptyBlocks(fn *minir.Function) bool {
	changed := true
	any := false
	for changed {
		changed = false
		for _, bi := range fn.Blocks {
			// Never remove entry or exit.
			if fn.Entry != nil && bi.ID == fn.Entry.ID {
				continue
			}
			if fn.Exit != nil && bi.ID == fn.Exit.ID {
				continue
			}
			if len(bi.Instrs) != 1 {
				continue
			}
			jmp, ok := bi.Instrs[0].(*minir.JumpInst)
			if !ok {
				continue
			}
			bj := fn.GetBlock(jmp.Target)
			if bj == nil || bj == bi {
				continue
			}

			// Redirect all predecessors of Bi to jump to Bj.
			for _, pred := range bi.Preds {
				// Update the terminator instruction in pred to point to Bj.
				switch t := pred.Term.(type) {
				case *minir.JumpInst:
					if t.Target == bi.Label {
						t.Target = bj.Label
					}
				case *minir.CondBrInst:
					if t.TrueLabel == bi.Label {
						t.TrueLabel = bj.Label
					}
					if t.FalseLabel == bi.Label {
						t.FalseLabel = bj.Label
					}
				}

				fn.ReplaceEdge(pred, bi, bj)
				for _, ins := range bj.Instrs {
					if phi, ok := ins.(*minir.PhiInst); ok {
						phi.ReplaceIncomingBlock(bi.Label, pred.Label)
					}
				}
			}

			fn.RemoveBlock(bi)
			changed = true
			any = true
			break // restart: map was mutated
		}
	}
	return any
}

// FoldRedundantBranches replaces a CondBrInst whose true and false targets are
// identical with an unconditional JumpInst.  Returns true when at least one
// branch was folded.
func FoldRedundantBranches(fn *minir.Function) bool {
	changed := false
	for _, b := range fn.Blocks {
		if len(b.Instrs) == 0 {
			continue
		}
		br, ok := b.Term.(*minir.CondBrInst)
		if !ok {
			continue
		}
		if br.TrueLabel != br.FalseLabel {
			continue
		}

		target := br.TrueLabel
		tgt := fn.GetBlock(target)
		if tgt == nil {
			continue
		}

		// Replace the CondBrInst with a JumpInst.
		jmp := &minir.JumpInst{Target: target}
		b.Instrs[len(b.Instrs)-1] = jmp
		b.Term = jmp

		// Simplify Succs to a single entry.
		b.Succs = map[int]*minir.Block{tgt.ID: tgt}
		b.SuccOrder = []int{tgt.ID}

		// Deduplicate predecessor entries in the target block.
		seen := false
		newPreds := make(map[int]*minir.Block)
		for _, pred := range tgt.Preds {
			if pred == b {
				if !seen {
					newPreds[pred.ID] = pred
					seen = true
				}
			} else {
				newPreds[pred.ID] = pred
			}
		}
		tgt.Preds = newPreds
		// Rebuild PredOrder keeping unique IDs.
		tgt.PredOrder = dedupOrder(tgt.PredOrder)

		changed = true
		break // restart since the CFG changed
	}
	return changed
}

// HoistBranch inlines a CondBrInst from Bj into Bi when:
//   - Bi ends with a JumpInst targeting Bj
//   - Bj consists of exactly one instruction (a CondBrInst)
//   - Bj has exactly one predecessor (Bi)
//
// After hoisting, Bi jumps directly to the two targets of Bj's CondBr; Bj
// becomes an empty dispatch block and will be removed by RemoveEmptyBlocks or
// CombineBlocks in a subsequent pass.  Returns true when at least one hoist
// was performed.
func HoistBranch(fn *minir.Function) bool {
	changed := false
	for _, bi := range fn.Blocks {
		if len(bi.Instrs) == 0 {
			continue
		}
		jmp, ok := bi.Term.(*minir.JumpInst)
		if !ok {
			continue
		}
		bj := fn.GetBlock(jmp.Target)
		if bj == nil {
			continue
		}
		// Bj must have exactly one predecessor (Bi).
		if len(bj.Preds) != 1 {
			continue
		}
		if _, hasPred := bj.Preds[bi.ID]; !hasPred {
			continue
		}
		// Bj must be a single CondBrInst.
		if len(bj.Instrs) != 1 {
			continue
		}
		branch, ok := bj.Term.(*minir.CondBrInst)
		if !ok {
			continue
		}

		trueTarget := fn.GetBlock(branch.TrueLabel)
		falseTarget := fn.GetBlock(branch.FalseLabel)
		if trueTarget == nil || falseTarget == nil {
			continue
		}

		// Replace Bi's jump with the inlined branch.
		inlined := &minir.CondBrInst{
			Cond:       branch.Cond,
			TrueLabel:  branch.TrueLabel,
			FalseLabel: branch.FalseLabel,
		}
		bi.Instrs[len(bi.Instrs)-1] = inlined
		bi.Term = inlined

		// Update Bi's successors to the two branch targets.
		//delete(bi.Succs, bj.ID)
		bi.RemoveSucc(bj)
		//bi.SuccOrder = removeFromOrder(bi.SuccOrder, bj.ID)
		bi.AddSucc(trueTarget)
		bi.AddSucc(falseTarget)

		// Update predecessor lists of branch targets.
		trueTarget.AddPred(bi)
		falseTarget.AddPred(bi)

		// Remove Bi from Bj's predecessor list since Bi no longer jumps to Bj.
		//delete(bj.Preds, bi.ID)
		//bj.PredOrder = removeFromOrder(bj.PredOrder, bi.ID)
		bj.RemovePred(bi)

		changed = true
	}
	return changed
}

// ── helpers ───────────────────────────────────────────────────────────────────

// filterBlocks returns a copy of m keeping only blocks whose IDs appear in keep.
func filterBlocks(m map[int]*minir.Block, keep map[int]bool) map[int]*minir.Block {
	out := make(map[int]*minir.Block, len(m))
	for id, b := range m {
		if keep[id] {
			out[id] = b
		}
	}
	return out
}

// filterOrder returns a copy of ids keeping only values present in keep.
func filterOrder(ids []int, keep map[int]bool) []int {
	out := ids[:0:0]
	for _, id := range ids {
		if keep[id] {
			out = append(out, id)
		}
	}
	return out
}

// removeFromOrder returns a copy of ids with all occurrences of target removed.
func removeFromOrder(ids []int, target int) []int {
	out := ids[:0:0]
	for _, id := range ids {
		if id != target {
			out = append(out, id)
		}
	}
	return out
}

// replaceInOrder returns a copy of ids with all occurrences of old replaced by
// new, deduplicating if new already exists.
func replaceInOrder(ids []int, old, new int) []int {
	seen := false
	out := ids[:0:0]
	for _, id := range ids {
		if id == old {
			if !seen {
				out = append(out, new)
				seen = true
			}
		} else if id == new {
			if !seen {
				out = append(out, id)
				seen = true
			}
		} else {
			out = append(out, id)
		}
	}
	return out
}

// dedupOrder returns a copy of ids with duplicate entries removed, preserving
// first-occurrence order.
func dedupOrder(ids []int) []int {
	seen := make(map[int]bool)
	out := ids[:0:0]
	for _, id := range ids {
		if !seen[id] {
			seen[id] = true
			out = append(out, id)
		}
	}
	return out
}
