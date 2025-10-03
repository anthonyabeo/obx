package ralloc

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/asm"
	"github.com/anthonyabeo/obx/src/ir/mir"
)

type DemoTarget struct{}

func (d DemoTarget) Name() string {
	//TODO implement me
	panic("implement me")
}

func (d DemoTarget) Triple() string {
	//TODO implement me
	panic("implement me")
}

func (d DemoTarget) InstrInfo() {
	//TODO implement me
	panic("implement me")
}

func (d DemoTarget) RegisterInfo() *target.RegisterFile {
	return &target.RegisterFile{
		Allocatable: []string{"x10", "x11"},
		Temporaries: []string{"t0", "t1", "t2", "t3", "t4", "t5", "t6"},
	}
}

func (d DemoTarget) FrameInfo() {
	//TODO implement me
	panic("implement me")
}

func (d DemoTarget) SelectInstr(function *mir.Function) *asm.Function {
	//TODO implement me
	panic("implement me")
}

func (d DemoTarget) Legalize(function *asm.Function) {
	//TODO implement me
	panic("implement me")
}

func (d DemoTarget) AllocRegs(function *asm.Function) {
	//TODO implement me
	panic("implement me")
}

func (d DemoTarget) Emit(module *asm.Module) string {
	//TODO implement me
	panic("implement me")
}

func TestLinearScan(t *testing.T) {
	// Example intervals
	intervals := []Interval{
		{Reg: "v1", Start: 0, End: 6},
		{Reg: "v2", Start: 1, End: 2},
		{Reg: "v3", Start: 3, End: 5},
		{Reg: "v4", Start: 4, End: 7},
	}

	alloc := LinearScan(intervals, DemoTarget{})

	fmt.Println("Register Map:", alloc.RegMap)
	fmt.Println("Spill Map:", alloc.SpillMap)
}

func createFunction() *asm.Function {
	// Create registers
	v1 := &asm.Register{Name: "v1", Mode: asm.Virt}
	v2 := &asm.Register{Name: "v2", Mode: asm.Virt}
	v3 := &asm.Register{Name: "v3", Mode: asm.Virt}
	v4 := &asm.Register{Name: "v4", Mode: asm.Virt}

	// Instructions
	// 0: v1 = li 5
	li1 := &asm.Instr{
		Opcode:   "li",
		Operands: []asm.Operand{v1, &asm.Imm{Value: 5}},
		Def:      v1,
	}
	// 1: v2 = li 7
	li2 := &asm.Instr{
		Opcode:   "li",
		Operands: []asm.Operand{v2, &asm.Imm{Value: 7}},
		Def:      v2,
	}
	// 2: v3 = add v1, v2
	add := &asm.Instr{
		Opcode:   "add",
		Operands: []asm.Operand{v3, v1, v2},
		Def:      v3,
		Uses:     []*asm.Register{v1, v2},
	}
	// 3: v4 = mul v3, v2
	mul := &asm.Instr{
		Opcode:   "mul",
		Operands: []asm.Operand{v4, v3, v2},
		Def:      v4,
		Uses:     []*asm.Register{v3, v2},
	}
	// 4: store v4, @x
	store := &asm.Instr{
		Opcode:   "store",
		Operands: []asm.Operand{v4, &asm.Global{Name: "x"}},
		Uses:     []*asm.Register{v4},
	}
	// 5: ret
	ret := &asm.Instr{
		Opcode: "ret",
	}

	// Create block and function
	block := asm.NewBlock(0, "entry")
	block.Instr = []*asm.Instr{li1, li2, add, mul, store}
	block.Term = ret

	fn := &asm.Function{
		Name:   "example",
		Blocks: []*asm.Block{block},
		Entry:  block,
	}

	return fn
}

func TestRewrite(t *testing.T) {
	intervals := []Interval{
		{Reg: "v1", Start: 0, End: 2},
		{Reg: "v2", Start: 1, End: 3},
		{Reg: "v3", Start: 2, End: 3},
		{Reg: "v4", Start: 3, End: 4},
	}

	mach := DemoTarget{}
	alloc := LinearScan(intervals, mach)
	fn := Rewrite(createFunction(), alloc, mach)
	PrettyPrint(fn)
}

func PrettyPrint(fn *asm.Function) {
	fmt.Printf("func %s:\n", fn.Name)
	for _, block := range fn.Blocks {
		fmt.Printf("  block %s:\n", block.Label)
		for _, instr := range block.Instr {
			fmt.Printf("    %s", instr.Opcode)
			for _, op := range instr.Operands {
				switch v := op.(type) {
				case *asm.Register:
					fmt.Printf(" %s", v.Name)
				case *asm.Imm:
					fmt.Printf(" %d", v.Value)
				case *asm.Global:
					fmt.Printf(" @%s", v.Name)
				case *asm.MemAddr:
					fmt.Printf(" %s", v.String())
				default:
					fmt.Printf(" ?")
				}
			}
			fmt.Println()
		}
		if block.Term != nil {
			fmt.Printf("    %s\n", block.Term.Opcode)
		}
	}
}
