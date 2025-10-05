package ralloc

import (
	"sort"

	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/asm"
)

type Allocation struct {
	TempRegMap      map[string]string // vreg -> preg
	TempSpillMap    map[string]int    // vreg -> spill slot index
	CalleeSavedUsed map[string]bool
	NumSpills       int
}

// LinearScan allocates registers using linear scan algorithm.
func LinearScan(intervals []Interval, target target.Machine) Allocation {
	// sort by start
	sort.Slice(intervals, func(i, j int) bool {
		return intervals[i].Start < intervals[j].Start
	})

	alloc := Allocation{
		TempRegMap:      make(map[string]string),
		TempSpillMap:    make(map[string]int),
		CalleeSavedUsed: map[string]bool{"ra": true, "fp": true}, // always save ra
	}

	type activeInterval struct {
		Interval
		Preg string
	}
	active := []activeInterval{}

	registerFile := target.RegisterInfo()
	freeRegs := append([]string(nil), registerFile.Allocatable...) // copy

	for _, iv := range intervals {
		// Expire old intervals
		newActive := []activeInterval{}
		for _, a := range active {
			if a.End >= iv.Start {
				newActive = append(newActive, a)
			} else {
				freeRegs = append(freeRegs, a.Preg) // free the register
			}
		}
		active = newActive

		if len(freeRegs) > 0 {
			// Assign a free register
			preg := freeRegs[len(freeRegs)-1]
			freeRegs = freeRegs[:len(freeRegs)-1]
			alloc.TempRegMap[iv.Reg] = preg
			active = append(active, activeInterval{iv, preg})

			if registerFile.CalleeSaved[preg] {
				alloc.CalleeSavedUsed[preg] = true
			}
		} else {
			// Spill: choose interval with the farthest end
			spillIdx := -1
			maxEnd := iv.End
			for i, a := range active {
				if a.End > maxEnd {
					maxEnd = a.End
					spillIdx = i
				}
			}

			if spillIdx != -1 {
				// Spill the active interval
				spill := active[spillIdx]
				alloc.TempSpillMap[spill.Reg] = alloc.NumSpills
				alloc.NumSpills++

				// Reuse its register for current interval
				alloc.TempRegMap[iv.Reg] = spill.Preg
				active[spillIdx] = activeInterval{iv, spill.Preg}
			} else {
				// Spill current interval
				alloc.TempSpillMap[iv.Reg] = alloc.NumSpills
				alloc.NumSpills++
			}
		}
	}
	return alloc
}

type Spill struct {
	Index  int
	Offset int // byte offset from stack frame base (e.g., fp - offset)
}

func Rewrite(fn *asm.Function, alloc Allocation, target target.Machine, layout target.FrameLayout) /**asm.Function*/ {
	for _, block := range fn.Blocks {
		newInstrs := []*asm.Instr{}
		for _, instr := range block.Instr {
			rewritten, extra := rewriteInstr(instr, alloc, fn.Spills, target)
			// extra holds loads before + stores after
			newInstrs = append(newInstrs, extra.Before...)
			newInstrs = append(newInstrs, rewritten)
			newInstrs = append(newInstrs, extra.After...)
		}
		// newBlocks = append(newBlocks, &asm.Block{Instr: newInstrs})
		block.Instr = newInstrs
	}

	target.EmitPrologueEpilogue(fn, layout)
}

type Extra struct {
	Before []*asm.Instr // loads
	After  []*asm.Instr // stores
}

var tempIdx int

func freshTemp(temps []string) *asm.Register {
	t := temps[tempIdx%len(temps)]
	tempIdx++
	return &asm.Register{
		Name: t,
		Mode: asm.Phys,
		Kind: asm.GPR,
	}
}

func rewriteInstr(instr *asm.Instr, alloc Allocation, spills map[string]asm.SpillInfo, target target.Machine) (*asm.Instr, Extra) {
	before := []*asm.Instr{}
	after := []*asm.Instr{}

	// Rewrite uses
	for _, idx := range instr.UseIdx() {
		op := instr.Operands[idx].(*asm.Register)
		if preg, ok := alloc.TempRegMap[op.Name]; ok {
			reg := &asm.Register{
				Name: preg,
				Mode: asm.Phys,
				Kind: op.Kind,
			}
			instr.Operands[idx] = reg
			instr.Uses = append(instr.Uses, reg)
		} else if slot, ok := spills[op.Name]; ok {
			temp := freshTemp(target.RegisterInfo().Temporaries) // pick a preg (like x10) reserved for spill reload
			fp := &asm.Register{Name: "fp", Mode: asm.Phys, Kind: asm.GPR}
			mem := &asm.MemAddr{Base: fp, Offset: asm.Imm{Value: -slot.Offset}}
			load := &asm.Instr{Opcode: "ld", Operands: []asm.Operand{temp, mem}, Def: temp, Uses: []*asm.Register{fp}}

			before = append(before, load)
			instr.Uses = append(instr.Uses, temp)
			instr.Operands[idx] = temp
		}
	}

	// Rewrite defs
	if instr.Def != nil {
		if preg, ok := alloc.TempRegMap[instr.Def.Name]; ok {
			instr.Def = &asm.Register{
				Name: preg,
				Mode: asm.Phys,
				Kind: asm.GPR,
			}
			instr.Operands[0] = instr.Def // assuming def is always the first operand
		} else if slot, ok := spills[instr.Def.Name]; ok {
			temp := freshTemp(target.RegisterInfo().Temporaries)
			fp := &asm.Register{Name: "fp", Mode: asm.Phys, Kind: asm.GPR}
			mem := &asm.MemAddr{Base: fp, Offset: asm.Imm{Value: -slot.Offset}}

			instr.Def = temp
			instr.Operands[0] = temp // assuming def is always the first operand
			store := &asm.Instr{Opcode: "sd", Operands: []asm.Operand{temp, mem}, Uses: []*asm.Register{temp, fp}}
			after = append(after, store)
		}
	}

	return instr, Extra{Before: before, After: after}
}
