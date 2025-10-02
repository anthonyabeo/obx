package ralloc

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/asm"
)

func TestBuildIntervals(t *testing.T) {
	r1 := reg("t1")
	r2 := reg("t2")

	block := &asm.Block{
		Label: "bb0",
		Instr: []*asm.Instr{
			{Opcode: "add", Def: r1, Uses: []*asm.Register{r2}},
			{Opcode: "sub", Def: r2, Uses: []*asm.Register{r1}},
		},
	}

	fn := &asm.Function{
		Entry:  block,
		Blocks: []*asm.Block{block},
	}

	Liveness(fn)
	intervals := BuildIntervals(fn)

	if len(intervals) != 2 {
		t.Fatalf("expected 2 intervals, got %d", len(intervals))
	}

	expected := map[string][2]int{
		"t1": {0, 1},
		"t2": {0, 1},
	}

	for _, iv := range intervals {
		exp, ok := expected[iv.Reg]
		if !ok {
			t.Errorf("interval for %s not found", iv.Reg)
			continue
		}

		if iv.Start != exp[0] || iv.End != exp[1] {
			t.Errorf("unexpected interval for %s: got [%d,%d], want [%d,%d]", iv.Reg, iv.Start, iv.End, exp[0], exp[1])
		}
	}
}

func TestBuildIntervals_BasicBlock(t *testing.T) {
	v1 := reg("v1")
	v2 := reg("v2")
	v3 := reg("v3")
	v4 := reg("v4")

	block := &asm.Block{
		Label: "bb0",
		Instr: []*asm.Instr{
			{Opcode: "li", Def: v1},
			{Opcode: "li", Def: v2},
			{Opcode: "add", Def: v3, Uses: []*asm.Register{v1, v2}},
			{Opcode: "mul", Def: v4, Uses: []*asm.Register{v3, v2}},
			{Opcode: "store", Uses: []*asm.Register{v4}},
			{Opcode: "ret"},
		},
	}

	fn := &asm.Function{
		Entry:  block,
		Blocks: []*asm.Block{block},
	}

	Liveness(fn)
	intervals := BuildIntervals(fn)

	expected := map[string][2]int{
		"v1": {0, 2},
		"v2": {1, 3},
		"v3": {2, 3},
		"v4": {3, 4},
	}

	for _, iv := range intervals {
		exp, ok := expected[iv.Reg]
		if !ok {
			t.Errorf("interval for %s not found", iv.Reg)
			continue
		}

		if iv.Start != exp[0] || iv.End != exp[1] {
			t.Errorf("unexpected interval for %s: got [%d,%d], want [%d,%d]", iv.Reg, iv.Start, iv.End, exp[0], exp[1])
		}
	}
}

func TestBuildIntervals_IfElseJoin(t *testing.T) {
	v1 := reg("v1")
	v2 := reg("v2")
	tt := reg("t")
	v3 := reg("v3")
	v4 := reg("v4")

	entry := &asm.Block{
		Label: "B0",
		Instr: []*asm.Instr{
			{Opcode: "li", Def: v1},
			{Opcode: "li", Def: v2},
			{Opcode: "lt", Def: tt, Uses: []*asm.Register{v1, v2}},
			{Opcode: "if", Uses: []*asm.Register{tt}},
		},
	}
	then := &asm.Block{
		Label: "B1",
		Instr: []*asm.Instr{
			{Opcode: "add", Def: v3, Uses: []*asm.Register{v1, v2}},
			{Opcode: "goto"},
		},
	}
	els := &asm.Block{
		Label: "B2",
		Instr: []*asm.Instr{
			{Opcode: "sub", Def: v3, Uses: []*asm.Register{v2, v1}},
			{Opcode: "goto"},
		},
	}
	join := &asm.Block{
		Label: "B3",
		Instr: []*asm.Instr{
			{Opcode: "mul", Def: v4, Uses: []*asm.Register{v3, v2}},
			{Opcode: "store", Uses: []*asm.Register{v4}},
			{Opcode: "ret"},
		},
	}

	// Set up CFG relationships
	entry.Succ = []*asm.Block{then, els}
	then.Pred = []*asm.Block{entry}
	then.Succ = []*asm.Block{join}
	els.Pred = []*asm.Block{entry}
	els.Succ = []*asm.Block{join}
	join.Pred = []*asm.Block{then, els}

	fn := &asm.Function{
		Entry:  entry,
		Blocks: []*asm.Block{entry, then, els, join},
	}

	Liveness(fn)
	intervals := BuildIntervals(fn)

	expected := map[string][2]int{
		"v1": {0, 6}, // live from li to add/sub
		"v2": {1, 8}, // live from li to mul
		"t":  {2, 3}, // live for comparison and branch
		"v3": {4, 8}, // defined in then/else, used in join
		"v4": {8, 9}, // defined in join, used in store
	}

	for _, iv := range intervals {
		exp, ok := expected[iv.Reg]
		if !ok {
			t.Errorf("interval for %s not found", iv.Reg)
			continue
		}

		if iv.Start != exp[0] || iv.End != exp[1] {
			t.Errorf("unexpected interval for %s: got [%d,%d], want [%d,%d]", iv.Reg, iv.Start, iv.End, exp[0], exp[1])
		}
	}
}

func TestBuildIntervals_LoopVarUpdate(t *testing.T) {
	v1 := reg("v1")
	v2 := reg("v2")
	v3 := reg("v3")
	v4 := reg("v4")

	block0 := &asm.Block{
		Label: "block0",
		Instr: []*asm.Instr{
			{Opcode: "li", Def: v1},
			{Opcode: "jmp"},
		},
	}
	block1 := &asm.Block{
		Label: "block1",
		Instr: []*asm.Instr{
			{Opcode: "mov", Def: v2, Uses: []*asm.Register{v1}},
			{Opcode: "add", Def: v3, Uses: []*asm.Register{v2}},
			{Opcode: "mul", Def: v4, Uses: []*asm.Register{v3}},
			{Opcode: "mov", Def: v1, Uses: []*asm.Register{v4}},
			{Opcode: "br"}, // branch: cond, block1, block2
		},
	}
	block2 := &asm.Block{
		Label: "block2",
		Instr: []*asm.Instr{
			{Opcode: "ret", Uses: []*asm.Register{v1}},
		},
	}

	// Set up CFG relationships
	block0.Succ = []*asm.Block{block1}
	block1.Pred = []*asm.Block{block0, block1}
	block1.Succ = []*asm.Block{block1, block2}
	block2.Pred = []*asm.Block{block1}

	fn := &asm.Function{
		Entry:  block0,
		Blocks: []*asm.Block{block0, block1, block2},
	}

	Liveness(fn)
	intervals := BuildIntervals(fn)

	expected := map[string][2]int{
		"v1": {0, 7}, // defined in block0, used/updated in block1, used in ret
		"v2": {2, 3}, // defined in mov, used in add
		"v3": {3, 4}, // defined in add, used in mul
		"v4": {4, 5}, // defined in mul, used in mov
	}

	for _, iv := range intervals {
		exp, ok := expected[iv.Reg]
		if !ok {
			t.Errorf("interval for %s not found", iv.Reg)
			continue
		}

		if iv.Start != exp[0] || iv.End != exp[1] {
			t.Errorf("unexpected interval for %s: got [%d,%d], want [%d,%d]", iv.Reg, iv.Start, iv.End, exp[0], exp[1])
		}
	}
}
