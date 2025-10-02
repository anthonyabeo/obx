package ralloc

import (
	"sort"

	"github.com/anthonyabeo/obx/src/ir/asm"
)

func BuildCFG(fn *asm.Function) {
	labelToBlock := map[string]*asm.Block{}
	for _, blk := range fn.Blocks {
		labelToBlock[blk.Label] = blk
	}

	keys := make([]int, 0, len(fn.Blocks))
	for k := range fn.Blocks {
		keys = append(keys, k)
	}
	sort.Ints(keys)

	for _, i := range keys {
		blk := fn.Blocks[i]
		if blk.Term != nil {
			switch blk.Term.Opcode {
			case "j", "jal":
				label := blk.Term.Operands[0].(*asm.Label)
				if b := labelToBlock[label.Name]; b != nil {
					blk.Succ = append(blk.Succ, b)
				}

				var prev *asm.Instr
				if len(blk.Instr) > 2 {
					prev = blk.Instr[len(blk.Instr)-2]
				}

				if prev != nil && (prev.Opcode == "beq" || prev.Opcode == "bne") {
					truePath := prev.Operands[2].(*asm.Label)
					if b := labelToBlock[truePath.Name]; b != nil {
						blk.Succ = append(blk.Succ, b)
					}
				}

			case "ret":
				// no succs
			default:
				// unknown terminator: assume fallthrough
				if i+1 < len(fn.Blocks) {
					blk.Instr = append(blk.Instr, &asm.Instr{Opcode: "j", Operands: []asm.Operand{asm.Label{Name: fn.Blocks[i+1].Label}}})
					blk.Succ[i+1] = fn.Blocks[i+1]
				}
			}
		}
	}

	// fill preds
	for _, blk := range fn.Blocks {
		for _, s := range blk.Succ {
			s.Pred = append(s.Pred, blk)
		}
	}
}

func Liveness(fn *asm.Function) {
	changed := true
	for changed {
		changed = false

		// Iterate blocks in reverse order (not required but often faster)
		for _, block := range fn.Blocks {
			// Work backwards over instructions
			for i := len(block.Instr) - 1; i >= 0; i-- {
				ins := block.Instr[i]

				// Save old sets for comparison
				oldIn := copySet(ins.LiveIn)
				oldOut := copySet(ins.LiveOut)

				if i == len(block.Instr)-1 {
					// Last instruction: liveOut = union of successor block ins
					ins.LiveOut = make(map[string]bool)
					for _, succ := range block.Succ {
						if len(succ.Instr) > 0 {
							succIn := succ.Instr[0].LiveIn
							unionInto(ins.LiveOut, succIn)
						}
					}
				} else {
					// Otherwise, liveOut = liveIn of next instruction
					ins.LiveOut = copySet(block.Instr[i+1].LiveIn)
				}

				// liveIn = use ∪ (liveOut – def)
				ins.LiveIn = make(map[string]bool)
				for _, u := range ins.Uses {
					ins.LiveIn[u.Name] = true
				}
				for r := range ins.LiveOut {
					if !contains(ins.Defs(), r) {
						ins.LiveIn[r] = true
					}
				}

				if !equalSets(oldIn, ins.LiveIn) || !equalSets(oldOut, ins.LiveOut) {
					changed = true
				}
			}
		}
	}
}

func copySet(src map[string]bool) map[string]bool {
	dst := make(map[string]bool)
	for k := range src {
		dst[k] = true
	}
	return dst
}

func unionInto(dst map[string]bool, src map[string]bool) {
	for k := range src {
		dst[k] = true
	}
}

func equalSets(a, b map[string]bool) bool {
	if len(a) != len(b) {
		return false
	}
	for k := range a {
		if !b[k] {
			return false
		}
	}
	return true
}

func contains(list []string, r string) bool {
	for _, x := range list {
		if x == r {
			return true
		}
	}
	return false
}
