package ralloc

import (
	"fmt"
	"sort"

	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/asm"
)

type Allocation struct {
	TempRegMap      map[string]string // vreg -> preg
	TempSpillMap    map[string]int    // vreg -> spill slot index
	CalleeSavedUsed []string
	NumSpills       int
}

// LinearScan allocates registers using linear scan algorithm.
func LinearScan(fn *asm.Function, intervals []Interval, target target.Machine) Allocation {
	// sort by start
	sort.Slice(intervals, func(i, j int) bool {
		return intervals[i].Start < intervals[j].Start
	})

	alloc := Allocation{
		TempRegMap:      make(map[string]string),
		TempSpillMap:    make(map[string]int),
		CalleeSavedUsed: []string{"fp"}, // always save ra
	}

	if !fn.IsLeaf {
		alloc.CalleeSavedUsed = append(alloc.CalleeSavedUsed, "ra")
	}

	CalleeSavedUsed := make(map[string]bool)

	type activeInterval struct {
		Interval
		Preg string
	}
	active := make([]activeInterval, 0)

	registerFile := target.RegisterInfo()
	freeRegs := append([]string(nil), registerFile.Allocatable...) // copy

	for _, iv := range intervals {
		// Expire old intervals
		newActive := make([]activeInterval, 0)
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
			if _, exists := alloc.TempRegMap[iv.Reg]; !exists {
				freeRegs = freeRegs[:len(freeRegs)-1]

				alloc.TempRegMap[iv.Reg] = preg
				active = append(active, activeInterval{iv, preg})

				if registerFile.CalleeSaved[preg] && !CalleeSavedUsed[preg] {
					CalleeSavedUsed[preg] = true
					alloc.CalleeSavedUsed = append(alloc.CalleeSavedUsed, preg)
				}
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

func Rewrite(fn *asm.Function, alloc Allocation, target target.Machine, layout target.FrameLayout) {
	for _, block := range fn.Blocks {
		newInstrs := make([]*asm.Instr, 0)
		for _, instr := range block.Instr {
			rewritten, extra := rewriteInstr(instr, alloc, layout, target)
			// extra holds loads before + stores after
			newInstrs = append(newInstrs, extra.Before...)
			newInstrs = append(newInstrs, rewritten)
			newInstrs = append(newInstrs, extra.After...)
		}
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

func rewriteInstr(instr *asm.Instr, alloc Allocation, layout target.FrameLayout, target target.Machine) (instrOut *asm.Instr, extra *Extra) {
	before := make([]*asm.Instr, 0)
	after := make([]*asm.Instr, 0)

	var (
		dst asm.Operand
		src []asm.Operand
	)

	if len(instr.Operands) > 0 {
		dst = instr.Operands[0]
		src = instr.Operands[1:]
	}

	for i, operand := range src {
		switch op := operand.(type) {
		case *asm.Register:
			if op.Mode == asm.Phys {
				continue
			}

			if preg, ok := alloc.TempRegMap[op.Name]; ok {
				reg := &asm.Register{
					Name: preg,
					Mode: asm.Phys,
					Kind: op.Kind,
				}
				instr.Operands[i+1] = reg

				continue
			}

			obj := layout.GetSpillObjectByName(op.Name)
			if obj == nil {
				panic("spill slot not found")
			}
			// Write to spill slot
			temp := freshTemp(target.RegisterInfo().Temporaries) // pick a preg (like x10) reserved for spill reload
			instr.Def = temp
			instr.Operands[0] = temp
			instr.Uses = append(instr.Uses, temp)
			fp := &asm.Register{Name: "fp", Mode: asm.Phys, Kind: asm.GPR}
			mem := &asm.MemAddr{Base: fp, Offset: asm.Imm{Value: obj.Offset}}
			store := &asm.Instr{Opcode: "sd", Operands: []asm.Operand{temp, mem}, Uses: []*asm.Register{temp, fp}}
			after = append(after, store)
		case *asm.Symbol:
			switch op.Kind {
			case asm.ParamSK:
				obj := layout.GetParamObjectByName(op.Name)
				if obj == nil {
					panic("param variable not found")
				}

				if obj.InRegister {
					instr.Operands[i+1] = &asm.Register{Name: obj.Reg, Mode: asm.Phys, Kind: asm.GPR}
				} else {
					fp := &asm.Register{Name: "fp", Mode: asm.Phys, Kind: asm.GPR}
					mem := &asm.MemAddr{Base: fp, Offset: &asm.Imm{Value: obj.Offset}}
					temp := freshTemp(target.RegisterInfo().Temporaries)

					load := &asm.Instr{Opcode: "ld", Operands: []asm.Operand{temp, mem}, Def: temp, Uses: []*asm.Register{fp}}

					before = append(before, load)
					instr.Uses = append(instr.Uses, temp)
					instr.Operands[i+1] = temp
				}
			case asm.LocalSK:
				obj := layout.GetLocalObjectByName(op.Name)
				if obj == nil {
					panic("local variable not found")
				}

				fp := &asm.Register{Name: target.RegisterInfo().FramePointer, Mode: asm.Phys, Kind: asm.GPR}
				mem := &asm.MemAddr{Base: fp, Offset: &asm.Imm{Value: obj.Offset}}
				temp := freshTemp(target.RegisterInfo().Temporaries)

				load := &asm.Instr{Opcode: "ld", Operands: []asm.Operand{temp, mem}, Def: temp, Uses: []*asm.Register{fp}}
				before = append(before, load)
				instr.Uses = append(instr.Uses, temp)
				instr.Operands[i+1] = temp
			case asm.GlobalSK:
				temp := freshTemp(target.RegisterInfo().Temporaries)
				sym := &asm.Symbol{Name: op.Name, Kind: asm.GlobalSK}

				mem := &asm.MemAddr{Base: temp, Offset: &asm.Imm{Value: 0}}

				before = append(before, &asm.Instr{
					Opcode:   "la",
					Operands: []asm.Operand{temp, sym},
					Def:      temp,
				})
				instr.Operands[i+1] = mem
			case asm.ConstSK:
				temp := freshTemp(target.RegisterInfo().Temporaries)
				sym := &asm.Symbol{Name: op.Name, Kind: asm.ConstSK}

				before = append(before, &asm.Instr{
					Opcode:   "la",
					Operands: []asm.Operand{temp, sym},
					Def:      temp,
				})
				instr.Operands[i+1] = temp
			}
		}
	}

	// Rewrite dst (def)
	if dst != nil {
		switch op := dst.(type) {
		case *asm.Register:
			if op.Mode == asm.Phys {
				break
			}

			if preg, ok := alloc.TempRegMap[op.Name]; ok {
				reg := &asm.Register{
					Name: preg,
					Mode: asm.Phys,
					Kind: asm.GPR,
				}
				instr.Def = reg
				instr.Operands[0] = reg
				break
			}

			obj := layout.GetSpillObjectByName(op.Name)
			if obj == nil {
				panic("spill slot not found")
			}

			// Write to spill slot
			temp := freshTemp(target.RegisterInfo().Temporaries) // pick a preg (like x10) reserved for spill reload
			instr.Def = temp
			instr.Operands[0] = temp
			instr.Uses = append(instr.Uses, temp)
			fp := &asm.Register{Name: "fp", Mode: asm.Phys, Kind: asm.GPR}
			mem := &asm.MemAddr{Base: fp, Offset: asm.Imm{Value: obj.Offset}}

			store := &asm.Instr{Opcode: "sd", Operands: []asm.Operand{temp, mem}, Uses: []*asm.Register{temp, fp}}
			after = append(after, store)
		case *asm.Symbol:
			switch op.Kind {
			case asm.ParamSK:
				obj := layout.GetParamObjectByName(op.Name)
				if obj == nil {
					panic("param variable not found")
				}

				if obj.InRegister {
					instr.Operands[0] = &asm.Register{Name: obj.Reg, Mode: asm.Phys, Kind: asm.GPR}
				} else {
					fp := &asm.Register{Name: "fp", Mode: asm.Phys, Kind: asm.GPR}
					mem := &asm.MemAddr{Base: fp, Offset: &asm.Imm{Value: obj.Offset}}
					temp := freshTemp(target.RegisterInfo().Temporaries) // pick a preg (like x10) reserved for spill reload

					store := &asm.Instr{Opcode: "sd", Operands: []asm.Operand{temp, mem}, Uses: []*asm.Register{temp, fp}}

					instr.Def = temp
					instr.Operands[0] = temp
					after = append(after, store)
				}
			case asm.LocalSK:
				obj := layout.GetLocalObjectByName(op.Name)
				if obj == nil {
					panic("local variable not found")
				}

				fp := &asm.Register{Name: target.RegisterInfo().FramePointer, Mode: asm.Phys, Kind: asm.GPR}
				mem := &asm.MemAddr{Base: fp, Offset: &asm.Imm{Value: obj.Offset}}
				temp := freshTemp(target.RegisterInfo().Temporaries) // pick a preg (like x10) reserved for spill reload

				store := &asm.Instr{Opcode: "sd", Operands: []asm.Operand{temp, mem}, Uses: []*asm.Register{temp, fp}}

				instr.Def = temp
				instr.Operands[0] = temp
				after = append(after, store)
			case asm.GlobalSK:
				temp := freshTemp(target.RegisterInfo().Temporaries)
				sym := &asm.Symbol{Name: op.Name, Kind: asm.GlobalSK}

				mem := &asm.MemAddr{Base: temp, Offset: &asm.Imm{Value: 0}}

				before = append(before, &asm.Instr{
					Opcode:   "la",
					Operands: []asm.Operand{temp, sym},
					Def:      temp,
				})
				instr.Operands[0] = mem
			case asm.ConstSK:
			}
		case *asm.Argument:
			if 0 <= op.Index && op.Index <= 7 {
				instr.Operands[0] = &asm.Register{
					Name: fmt.Sprintf("a%d", op.Index),
					Mode: asm.Phys,
					Kind: asm.GPR,
				}
			} else {
				offset := (op.Index - target.RegisterInfo().MaxArgRegs) * target.FrameInfo().WordSize

				sp := &asm.Register{Name: "sp", Mode: asm.Phys, Kind: asm.GPR}
				mem := &asm.MemAddr{Base: sp, Offset: &asm.Imm{Value: offset}}
				temp := src[0]

				instr = &asm.Instr{Opcode: "sd", Operands: []asm.Operand{temp, mem}}
			}
		}
	}

	return instr, &Extra{Before: before, After: after}
}
