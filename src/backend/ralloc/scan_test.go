package ralloc

import (
	"fmt"
	"testing"
)

func TestLinearScan(t *testing.T) {
	// Example intervals
	intervals := []Interval{
		{Reg: "v1", Start: 0, End: 6},
		{Reg: "v2", Start: 1, End: 2},
		{Reg: "v3", Start: 3, End: 5},
		{Reg: "v4", Start: 4, End: 7},
	}

	physicalRegs := []string{"x10", "x11"} // pretend we have 2 free regs

	alloc := LinearScan(intervals, physicalRegs)

	fmt.Println("Register Map:", alloc.RegMap)
	fmt.Println("Spill Map:", alloc.SpillMap)
}
