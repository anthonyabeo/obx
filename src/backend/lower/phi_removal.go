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

func RemovePhisInFunction(fn *mir.Function, tgt target.Target) error {
	if fn == nil {
		return fmt.Errorf("remove phis: nil function")
	}
	if tgt == nil {
		return fmt.Errorf("phis: nil target")
	}

	for _, block := range fn.Blocks {
		if block == nil || !hasPhis(block) {
			continue
		}

		phis := []*mir.PhiInstr{}
		newInstrs := []mir.Instr{}
		for _, instr := range block.Instrs {
			if _, ok := instr.(*mir.PhiInstr); !ok {
				newInstrs = append(newInstrs, instr)
			} else if phi, ok := instr.(*mir.PhiInstr); ok {
				phis = append(phis, phi)
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

			// Insert moves BEFORE the terminator (not after)
			// Extract terminator
			term := pred.Term
			// Remove terminator from Instrs if it's the last instruction
			if len(pred.Instrs) > 0 && pred.Instrs[len(pred.Instrs)-1] == term {
				pred.Instrs = pred.Instrs[:len(pred.Instrs)-1]
			}
			// Append moves, then re-attach terminator
			pred.Instrs = append(pred.Instrs, movs...)
			pred.Instrs = append(pred.Instrs, term)
		}
	}

	return nil
}
