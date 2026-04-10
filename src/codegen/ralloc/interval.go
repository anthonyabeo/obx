package ralloc

import "github.com/anthonyabeo/obx/src/codegen/asm"

type Interval struct {
	Reg   string
	Start int
	End   int
}

// extendInterval expands iv to cover position idx.
func extendInterval(iv Interval, idx int) Interval {
	if idx < iv.Start {
		iv.Start = idx
	}
	if idx > iv.End {
		iv.End = idx
	}
	return iv
}

// BuildIntervals computes live intervals for each vreg in a function.
// Assumes each instruction has its own LiveIn / LiveOut sets already computed.
func BuildIntervals(fn *asm.Function) []Interval {
	intervals := make(map[string]Interval)

	getOrInit := func(m map[string]Interval, reg string, idx int) Interval {
		iv, ok := m[reg]
		if !ok {
			iv = Interval{Start: idx, End: idx, Reg: reg}
		}
		return iv
	}

	idx := 0
	for _, block := range fn.Blocks {
		for _, instr := range block.Instr {

			for _, r := range instr.Defs() {
				intervals[r] = extendInterval(getOrInit(intervals, r, idx), idx)
			}

			for _, r := range instr.Uses {
				intervals[r.Name] = extendInterval(getOrInit(intervals, r.Name, idx), idx)
			}

			for r := range instr.LiveIn {
				intervals[r] = extendInterval(getOrInit(intervals, r, idx), idx)
			}

			for r := range instr.LiveOut {
				intervals[r] = extendInterval(getOrInit(intervals, r, idx), idx)
			}
			idx++
		}
	}
	intervalList := make([]Interval, 0, len(intervals))
	for _, iv := range intervals {
		intervalList = append(intervalList, iv)
	}
	return intervalList
}
