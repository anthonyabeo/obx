package ralloc

import "github.com/anthonyabeo/obx/src/ir/asm"

type Interval struct {
	Reg   string
	Start int
	End   int
}

// BuildIntervals computes live intervals for each vreg in a function.
// Assumes each instruction has its own LiveIn / LiveOut sets already computed.
func BuildIntervals(fn *asm.Function) []Interval {
	intervals := make(map[string]Interval)

	getOrInit := func(m map[string]Interval, reg string, idx int) Interval {
		iv, ok := m[reg]
		if !ok && (iv.Start == 0 && iv.End == 0) {
			iv = Interval{Start: idx, End: idx, Reg: reg}
		}
		return iv
	}

	idx := 0
	for _, block := range fn.Blocks {
		for _, instr := range block.Instr {

			for _, r := range instr.Defs() {
				iv := getOrInit(intervals, r, idx)
				if idx < iv.Start {
					iv.Start = idx
				}
				if idx > iv.End {
					iv.End = idx
				}
				intervals[r] = iv
			}

			for _, r := range instr.Uses {
				iv := getOrInit(intervals, r.Name, idx)
				if idx < iv.Start {
					iv.Start = idx
				}
				if idx > iv.End {
					iv.End = idx
				}
				intervals[r.Name] = iv
			}

			for r := range instr.LiveIn {
				iv := getOrInit(intervals, r, idx)
				if idx < iv.Start {
					iv.Start = idx
				}
				if idx > iv.End {
					iv.End = idx
				}
				intervals[r] = iv
			}

			for r := range instr.LiveOut {
				iv := getOrInit(intervals, r, idx)
				if idx < iv.Start {
					iv.Start = idx
				}
				if idx > iv.End {
					iv.End = idx
				}
				intervals[r] = iv
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
