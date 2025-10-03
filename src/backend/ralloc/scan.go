package ralloc

import (
	"sort"

	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/asm"
)

type Allocation struct {
	RegMap    map[string]string // vreg -> preg
	SpillMap  map[string]int    // vreg -> spill slot index
	NumSpills int
}

// LinearScan allocates registers using linear scan algorithm.
func LinearScan(intervals []Interval /*physicalRegs []string,*/, target target.Machine) Allocation {
	// sort by start
	sort.Slice(intervals, func(i, j int) bool {
		return intervals[i].Start < intervals[j].Start
	})

	alloc := Allocation{
		RegMap:   make(map[string]string),
		SpillMap: make(map[string]int),
	}

	type activeInterval struct {
		Interval
		Preg string
	}
	active := []activeInterval{}

	freeRegs := append([]string(nil), target.RegisterInfo().Allocatable...) // copy

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
			alloc.RegMap[iv.Reg] = preg
			active = append(active, activeInterval{iv, preg})
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
				alloc.SpillMap[spill.Reg] = alloc.NumSpills
				alloc.NumSpills++

				// Reuse its register for current interval
				alloc.RegMap[iv.Reg] = spill.Preg
				active[spillIdx] = activeInterval{iv, spill.Preg}
			} else {
				// Spill current interval
				alloc.SpillMap[iv.Reg] = alloc.NumSpills
				alloc.NumSpills++
			}
		}
	}
	return alloc
}

type SpillSlot struct {
	Index  int
	Offset int // byte offset from stack frame base (e.g., fp - offset)
}

func Rewrite(fn *asm.Function, alloc Allocation, target target.Machine) *asm.Function {
	newBlocks := []*asm.Block{}
	spillSlots := map[string]SpillSlot{}

	// assign offsets for each spilled register
	slotSize := 8 // assuming 64-bit word
	for vreg, slot := range alloc.SpillMap {
		spillSlots[vreg] = SpillSlot{
			Index:  slot,
			Offset: slot * slotSize,
		}
	}

	for _, block := range fn.Blocks {
		newInstrs := []*asm.Instr{}
		for _, instr := range block.Instr {
			rewritten, extra := rewriteInstr(instr, alloc, spillSlots, target)
			// extra holds loads before + stores after
			newInstrs = append(newInstrs, extra.Before...)
			newInstrs = append(newInstrs, rewritten)
			newInstrs = append(newInstrs, extra.After...)
		}
		newBlocks = append(newBlocks, &asm.Block{Instr: newInstrs})
	}

	return &asm.Function{Blocks: newBlocks}
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

func rewriteInstr(instr *asm.Instr, alloc Allocation, spills map[string]SpillSlot, target target.Machine) (*asm.Instr, Extra) {
	before := []*asm.Instr{}
	after := []*asm.Instr{}

	// Rewrite uses
	for _, idx := range instr.UseIdx() {
		op := instr.Operands[idx].(*asm.Register)
		if preg, ok := alloc.RegMap[op.Name]; ok {
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
		if preg, ok := alloc.RegMap[instr.Def.Name]; ok {
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
