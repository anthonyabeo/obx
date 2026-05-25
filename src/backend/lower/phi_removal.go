package lower

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// RemovePhisInProgram ...
func RemovePhisInProgram(prog *mir.Program, tgt target.Target) (*mir.Program, error) {
	if prog == nil {
		return mir.NewProgram(), nil
	}
	if tgt == nil {
		return nil, fmt.Errorf("lower switches: nil target")
	}
	for _, mod := range prog.Modules {
		if mod == nil {
			continue
		}
		for _, fn := range mod.Functions {
			if fn == nil {
				continue
			}
			if err := RemovePhisInFunction(fn, tgt); err != nil {
				return nil, fmt.Errorf("remove phis in %s: %w", fn.Name, err)
			}
		}
	}
	return prog, nil
}

func hasPhis(block *mir.Block) bool {
	for _, instr := range block.Instrs {
		if _, ok := instr.(*mir.PhiInstr); ok {
			return true
		}
	}
	return false
}

// RemovePhisInFunction eliminates all phi instructions in fn by inserting
// parallel-copy move sequences on predecessor edges.
//
// Critical-edge splitting: when a predecessor P of a phi-join block J has more
// than one successor, inserting moves into P before its branch terminator would
// execute those moves on every outgoing path—not just the edge P→J.  In that
// case we instead create a dedicated "edge block" that holds the moves and an
// unconditional jump to J, and retarget P's terminator to branch to the edge
// block instead of J.
func RemovePhisInFunction(fn *mir.Function, tgt target.Target) error {
	if fn == nil {
		return fmt.Errorf("remove phis: nil function")
	}
	if tgt == nil {
		return fmt.Errorf("phis: nil target")
	}

	// Snapshot fn.Blocks so that edge blocks added during this loop are not
	// themselves treated as phi-join blocks.
	origBlocks := append([]*mir.Block(nil), fn.Blocks...)
	nextID := maxBlockID(fn) + 1

	for _, block := range origBlocks {
		if block == nil || !hasPhis(block) {
			continue
		}

		phis := []*mir.PhiInstr{}
		newInstrs := []mir.Instr{}
		for _, instr := range block.Instrs {
			if phi, ok := instr.(*mir.PhiInstr); ok {
				phis = append(phis, phi)
			} else {
				newInstrs = append(newInstrs, instr)
			}
		}
		block.Instrs = newInstrs

		plan, err := tgt.LowerPhiBlock(block.Label, phis)
		if err != nil {
			return fmt.Errorf("remove phi block %s: %w", block.Label, err)
		}

		for _, edge := range plan.Edges {
			movs := []mir.Instr{}
			for _, cpy := range target.LinearizeParallelCopy(edge.Copies) {
				movs = append(movs, &mir.MoveInstr{
					Dst: cpy.Dst,
					Src: cpy.Src,
				})
			}

			pred := fn.BlockByLabel(edge.PredLabel)
			if pred == nil {
				return fmt.Errorf("remove phi block %s: missing predecessor %s", block.Label, edge.PredLabel)
			}

			if len(pred.Succs) > 1 {
				// Critical edge: pred has multiple successors.
				// Create a dedicated edge block so the moves only execute on
				// the specific path pred→block, not on all paths from pred.
				edgeLabel := fmt.Sprintf("__phi_edge_%s_%s", edge.PredLabel, block.Label)
				edgeBlock := mir.NewBlock(nextID, edgeLabel)
				nextID++

				// Populate edge block: moves + unconditional jump to join block.
				edgeBlock.Instrs = append(edgeBlock.Instrs, movs...)
				jmp := &mir.JumpInstr{Target: block.Label}
				edgeBlock.Instrs = append(edgeBlock.Instrs, jmp)
				edgeBlock.Term = jmp

				// Wire up CFG edges.
				edgeBlock.Preds[pred.ID] = pred
				edgeBlock.Succs[block.ID] = block
				delete(pred.Succs, block.ID)
				pred.Succs[edgeBlock.ID] = edgeBlock
				delete(block.Preds, pred.ID)
				block.Preds[edgeBlock.ID] = edgeBlock

				// Retarget pred's terminator: replace references to block.Label
				// with the new edge block label.
				retargetTerminator(pred.Term, block.Label, edgeLabel)

				fn.AddBlock(edgeBlock)
			} else {
				// Non-critical edge: pred's only successor is block, so it is
				// safe to insert moves directly before pred's terminator.
				term := pred.Term
				if len(pred.Instrs) > 0 && pred.Instrs[len(pred.Instrs)-1] == term {
					pred.Instrs = pred.Instrs[:len(pred.Instrs)-1]
				}
				pred.Instrs = append(pred.Instrs, movs...)
				pred.Instrs = append(pred.Instrs, term)
			}
		}
	}

	return nil
}

// maxBlockID returns the highest block ID currently present in fn.
func maxBlockID(fn *mir.Function) int {
	max := 0
	for _, b := range fn.Blocks {
		if b != nil && b.ID > max {
			max = b.ID
		}
	}
	return max
}

// retargetTerminator replaces every occurrence of oldLabel with newLabel in
// the branch targets of term.
func retargetTerminator(term mir.Terminator, oldLabel, newLabel string) {
	if term == nil {
		return
	}
	switch t := term.(type) {
	case *mir.CondBrInstr:
		if t.TrueLabel == oldLabel {
			t.TrueLabel = newLabel
		}
		if t.FalseLabel == oldLabel {
			t.FalseLabel = newLabel
		}
	case *mir.JumpInstr:
		if t.Target == oldLabel {
			t.Target = newLabel
		}
	case *mir.MachineTerm:
		for i, tgt := range t.Targets {
			if tgt == oldLabel {
				t.Targets[i] = newLabel
			}
		}
	}
}
