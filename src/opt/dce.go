package opt

import "github.com/anthonyabeo/obx/src/ir/mir"

func EliminateDeadBlocks(f *mir.Function) bool {
	changed := false
	reachable := make(map[int]bool)
	var worklist []*mir.Block

	// Start from entry block
	entry := f.Entry
	worklist = append(worklist, entry)
	reachable[entry.ID] = true

	// BFS to mark reachable blocks
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

	// Filter only reachable blocks
	newBlocks := make(map[int]*mir.Block)
	for id, b := range f.Blocks {
		if reachable[id] {
			// prune dead preds/succs
			b.Preds = filterBlocks(b.Preds, reachable)
			b.Succs = filterBlocks(b.Succs, reachable)
			newBlocks[id] = b
		}
	}

	if len(newBlocks) != len(f.Blocks) {
		f.Blocks = newBlocks
		changed = true
	}

	return changed
}

func CombineBlocks(f *mir.Function) bool {
	changed := false
	for _, bi := range f.Blocks {
		// Must end with exactly 1 unconditional jump instruction
		if len(bi.Instrs) != 1 {
			continue
		}
		lastInstr := bi.Instrs[len(bi.Instrs)-1]
		jmp, ok := lastInstr.(*mir.JumpInst)
		if !ok {
			continue
		}

		blk := f.GetBlock(jmp.Target)
		if blk == nil {
			continue
		}
		bj := blk

		// Merge only if Bj has one pred (Bi)
		if len(bj.Preds) != 1 || !bj.HasPred(bi) {
			continue
		}

		// Remove jump from Bi
		bi.Instrs = bi.Instrs[:len(bi.Instrs)-1]

		// Append Bj's instructions
		bi.Instrs = append(bi.Instrs, bj.Instrs...)
		bi.Term = bj.Term

		// Update successors
		bi.Succs = bj.Succs
		for _, succ := range bj.Succs {
			// Replace pred reference from Bj to Bi
			for pi, pred := range succ.Preds {
				if pred == bj {
					succ.Preds[pi] = bi
				}
			}
		}

		// Remove Bj from function
		f.RemoveBlock(bj)

		changed = true
	}

	return changed
}

func RemoveEmptyBlocks(f *mir.Function) bool {
	changed := true
	for changed {
		changed = false
		for _, bi := range f.Blocks {
			if bi.Label == "exit" {
				continue
			}

			// Must have exactly one successor
			if len(bi.Succs) != 1 {
				continue
			}

			// Must contain only a single jump instruction
			if len(bi.Instrs) != 1 {
				continue
			}
			jmp, ok := bi.Instrs[0].(*mir.JumpInst)
			if !ok {
				continue
			}

			blk := f.GetBlock(jmp.Target)
			if blk == nil {
				continue
			}
			bj := blk

			// Redirect all preds of Bi to Bj
			for _, pred := range bi.Preds {
				for _, succ := range pred.Succs {
					if succ == bi {
						delete(pred.Succs, bi.ID)
						pred.Succs[bj.ID] = bj
					}
				}
				bj.Preds[pred.ID] = pred

				// update the branch/jump targets
				switch term := pred.Term.(type) {
				case *mir.JumpInst:
					term.Target = bj.Label
				case *mir.CondBrInst:
					if term.TrueLabel == bi.Label {
						term.TrueLabel = bj.Label
					}

					if term.FalseLabel == bi.Label {
						term.FalseLabel = bj.Label
					}
				}
			}

			// Remove Bi from Bj’s predecessor list
			newPreds := make(map[int]*mir.Block)
			for _, pred := range bj.Preds {
				if pred != bi {
					newPreds[pred.ID] = pred
				}
			}
			bj.Preds = newPreds

			// Remove Bi from function
			f.RemoveBlock(bi)

			changed = true
			break // restart because CFG changed
		}
	}

	return changed
}

func FoldRedundantBranches(f *mir.Function) bool {
	changed := false
	for _, b := range f.Blocks {
		if len(b.Instrs) == 0 {
			continue
		}

		// Check if last instruction is a Branch
		br, ok := b.Instrs[len(b.Instrs)-1].(*mir.CondBrInst)
		if !ok {
			continue
		}

		// Both branch targets must be the same
		if br.TrueLabel != br.FalseLabel {
			continue
		}

		// Replace with a jump
		target := br.TrueLabel
		b.Instrs[len(b.Instrs)-1] = &mir.JumpInst{Target: target}

		tgtBlock := f.GetBlock(target)
		if tgtBlock == nil {
			continue
		}

		// Adjust CFG edges
		b.Succs = map[int]*mir.Block{tgtBlock.ID: tgtBlock}

		// Remove duplicate predecessors in target
		seen := make(map[*mir.Block]bool)
		newPreds := make(map[int]*mir.Block)
		for _, pred := range tgtBlock.Preds {
			if pred == b && seen[pred] {
				continue
			}
			newPreds[pred.ID] = pred
			seen[pred] = true
		}
		tgtBlock.Preds = newPreds

		changed = true
		break // restart since CFG changed
	}

	return changed
}

func HoistBranch(f *mir.Function) bool {
	changed := false
	for _, bi := range f.Blocks {
		if len(bi.Instrs) == 0 {
			continue
		}

		// Last instruction must be a Jump
		jmp, ok := bi.Instrs[len(bi.Instrs)-1].(*mir.JumpInst)
		if !ok {
			continue
		}

		blk := f.GetBlock(jmp.Target)
		if blk == nil {
			continue
		}
		bj := blk

		// Bj must have only one predecessor (Bi)
		if bj.HasPred(bi) {
			continue
		}

		// Bj must have exactly one instruction, and it must be a Branch
		if len(bj.Instrs) != 1 {
			continue
		}
		branch, ok := bj.Instrs[0].(*mir.CondBrInst)
		if !ok {
			continue
		}

		// Replace jump in Bi with a copy of the branch
		bi.Instrs[len(bi.Instrs)-1] = &mir.CondBrInst{
			Cond:       branch.Cond,
			TrueLabel:  branch.TrueLabel,
			FalseLabel: branch.FalseLabel,
		}

		// Update Bi's successors
		trueTarget := f.GetBlock(branch.TrueLabel)
		falseTarget := f.GetBlock(branch.FalseLabel)
		if trueTarget == nil || falseTarget == nil {
			continue
		}

		bi.Succs = map[int]*mir.Block{
			trueTarget.ID:  trueTarget,
			falseTarget.ID: falseTarget,
		}

		// Fix predecessor lists of branch targets
		for _, tgt := range bi.Succs {
			tgt.Preds[bi.ID] = bi
		}

		changed = true
	}

	return changed
}
