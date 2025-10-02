package ralloc

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/ir/asm"
)

func reg(name string) *asm.Register {
	return &asm.Register{Name: name}
}

func regs(names ...string) []*asm.Register {
	var out []*asm.Register
	for _, n := range names {
		out = append(out, reg(n))
	}
	return out
}

func keys(rs map[string]bool) []string {
	var out []string
	for k := range rs {
		out = append(out, k)
	}
	return out
}

func cfg1() *asm.Function {
	bb0 := &asm.Block{ID: 1, Label: "bb0"}
	bb0.Instr = []*asm.Instr{
		{Opcode: "add", Def: reg("t1"), Uses: regs("t2", "t3")},
		{Opcode: "bne", Uses: regs("t1")},
		{Opcode: "j"},
	}

	bb1 := &asm.Block{ID: 2, Label: "bb1"}
	bb1.Instr = []*asm.Instr{
		{Opcode: "mul", Def: reg("t4"), Uses: regs("t1", "t5")},
		{Opcode: "j"},
	}

	bb2 := &asm.Block{ID: 3, Label: "bb2"}
	bb2.Instr = []*asm.Instr{
		{Opcode: "sub", Def: reg("t6"), Uses: regs("t1", "t7")},
		{Opcode: "j"},
	}

	bb3 := &asm.Block{ID: 4, Label: "bb3"}
	bb3.Instr = []*asm.Instr{
		{Opcode: "add", Def: reg("t8"), Uses: regs("t4", "t6")},
		{Opcode: "ret", Uses: regs("t8")},
	}

	// Connect CFG
	bb0.Succ = []*asm.Block{bb1, bb2}
	bb1.Succ = []*asm.Block{bb3}
	bb2.Succ = []*asm.Block{bb3}

	bb1.Pred = []*asm.Block{bb0}
	bb2.Pred = []*asm.Block{bb0}
	bb3.Pred = []*asm.Block{bb1, bb2}

	return &asm.Function{
		Entry:  bb0,
		Blocks: []*asm.Block{bb0, bb1, bb2, bb3},
	}
}

func cfg2() *asm.Function {
	bb0 := &asm.Block{ID: 1, Label: "bb0"}
	bb0.Instr = []*asm.Instr{
		{Opcode: "add", Def: reg("t1"), Uses: regs("t2", "t3")},
		{Opcode: "mul", Def: reg("t4"), Uses: regs("t1", "t5")},
		{Opcode: "sub", Def: reg("t6"), Uses: regs("t4", "t2")},
		{Opcode: "mov", Def: reg("t7"), Uses: regs("t6")},
	}

	return &asm.Function{
		Entry:  bb0,
		Blocks: []*asm.Block{bb0},
	}
}

func TestLiveness1(t *testing.T) {
	bb0 := &asm.Block{ID: 1, Label: "bb0"}
	bb0.Instr = []*asm.Instr{
		{Opcode: "add", Def: reg("t1"), Uses: regs("t2", "t3")},
		{Opcode: "sub", Def: reg("t2"), Uses: regs("t1", "t4")},
	}

	bb1 := &asm.Block{ID: 2, Label: "bb1"}
	bb1.Instr = []*asm.Instr{
		{Opcode: "mul", Def: reg("t5"), Uses: regs("t2")},
	}

	bb0.Succ = []*asm.Block{bb1}
	bb1.Pred = []*asm.Block{bb0}

	fn := &asm.Function{
		Entry:  bb0,
		Blocks: []*asm.Block{bb0, bb1},
	}

	Liveness(fn)

	for _, bb := range fn.Blocks {
		fmt.Printf("%s:\n", bb.Label)
		for i, instr := range bb.Instr {
			fmt.Printf("  %d %s\n", i, instr.Opcode)
			fmt.Printf("    LiveIn:  %v\n", keys(instr.LiveIn))
			fmt.Printf("    LiveOut: %v\n", keys(instr.LiveOut))
		}
	}
}

func TestLiveness2(t *testing.T) {
	fn := cfg1()

	Liveness(fn)

	for _, bb := range fn.Blocks {
		fmt.Printf("%s:\n", bb.Label)
		for i, instr := range bb.Instr {
			fmt.Printf("  %d %s\n", i, instr.Opcode)
			fmt.Printf("    LiveIn:  %v\n", keys(instr.LiveIn))
			fmt.Printf("    LiveOut: %v\n", keys(instr.LiveOut))
		}
	}
}

func TestLiveness3(t *testing.T) {
	fn := cfg2()

	Liveness(fn)

	for _, bb := range fn.Blocks {
		fmt.Printf("%s:\n", bb.Label)
		for i, instr := range bb.Instr {
			fmt.Printf("  %d %s\n", i, instr.Opcode)
			fmt.Printf("    LiveIn:  %v\n", keys(instr.LiveIn))
			fmt.Printf("    LiveOut: %v\n", keys(instr.LiveOut))
		}
	}
}
