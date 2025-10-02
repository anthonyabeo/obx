package riscv

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/backend/target"
	"math"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/asm"
	"github.com/anthonyabeo/obx/src/ir/mir"
)

type RV64IMAFD struct {
}

func NewRV64IMAFDTarget() *RV64IMAFD {
	return &RV64IMAFD{}
}

func (r RV64IMAFD) Name() string { return "rv64imafd" }

func (r RV64IMAFD) Triple() string {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) InstrInfo() {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) RegisterInfo() *target.RegisterFile {
	return &target.RegisterFile{
		AllRegs: []string{
			"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
			"x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
			"x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
			"x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31",
			"f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",
			"f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",
			"f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",
			"f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",
		},
		GeneralRegs: []string{
			"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
			"x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
			"x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
			"x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31",
		},
		FloatRegs: []string{
			"f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",
			"f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",
			"f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",
			"f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",
		},
		RegToIdx: map[string]int{
			"x0":  0,
			"x1":  1,
			"x2":  2,
			"x3":  3,
			"x4":  4,
			"x5":  5,
			"x6":  6,
			"x7":  7,
			"x8":  8,
			"x9":  9,
			"x10": 10,
			"x11": 11,
			"x12": 12,
			"x13": 13,
			"x14": 14,
			"x15": 15,
			"x16": 16,
			"x17": 17,
			"x18": 18,
			"x19": 19,
			"x20": 20,
			"x21": 21,
			"x22": 22,
			"x23": 23,
			"x24": 24,
			"x25": 25,
			"x26": 26,
			"x27": 27,
			"x28": 28,
			"x29": 29,
			"x30": 30,
			"x31": 31,
		},
		CallerSaved: []string{
			"x1",  // ra
			"x5",  // t0
			"x6",  // t1
			"x7",  // t2
			"x10", // a0
			"x11", // a1
			"x12", // a2
			"x13", // a3
			"x14", // a4
			"x15", // a5
			"x16", // a6
			"x17", // a7
			"x28", // t3
			"x29", // t4
			"x30", // t5
			"x31", // t6
		},
		CalleeSaved: []string{
			"x2",  // sp
			"x8",  // s0/fp
			"x9",  // s1
			"x18", // s2
			"x19", // s3
			"x20", // s4
			"x21", // s5
			"x22", // s6
			"x23", // s7
			"x24", // s8
			"x25", // s9
			"x26", // s10
			"x27", // s11
		},
		ArgRegs: []string{
			"x10", // a0
			"x11", // a1
			"x12", // a2
			"x13", // a3
			"x14", // a4
			"x15", // a5
			"x16", // a6
			"x17", // a7
		},
		ReturnRegs: []string{
			"x10", // a0
			"x11", // a1
		},
		StackPointer: "x2", // sp
		FramePointer: "x8", // s0/fp
		Reserved: []string{
			"x0", // zero
			"x1", // ra
			"x2", // sp
			"x3", // gp
			"x4", // tp
			"x8", // s0/fp
		},
		Temporaries: []string{
			"x5",  // t0
			"x6",  // t1
			"x7",  // t2
			"x28", // t3
			"x29", // t4
			"x30", // t5
			"x31", // t6
		},
	}
}

func (r RV64IMAFD) FrameInfo() {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) SelectInstr(function *mir.Function) *asm.Function {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) Legalize(fn *asm.Function) {

}

func (r RV64IMAFD) AllocRegs(function *asm.Function) {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) Emit(module *asm.Module) string {
	var buf strings.Builder

	buf.WriteString(".section .bss\n")
	for _, global := range module.Globals {
		buf.WriteString(r.formatGlobal(global))
	}
	buf.WriteString("\n")

	var funcNames []string
	funcMap := make(map[string]*asm.Function)
	for _, function := range module.Funcs {
		funcNames = append(funcNames, function.Name)
		funcMap[function.Name] = function
	}
	// sort function names to have deterministic output
	// (map iteration order is random)
	for _, name := range funcNames {
		function := funcMap[name]
		buf.WriteString(r.formatFunc(function))
	}

	return buf.String()
}

func (r RV64IMAFD) formatFunc(fn *asm.Function) string {
	var buf strings.Builder

	buf.WriteString(".section .text\n")
	buf.WriteString(".align 2\n\n")

	fnName := fn.Name
	if strings.HasPrefix(fnName, "__init_") {
		fnName = "start"
		fn.Exported = true
	}

	if fn.Exported {
		buf.WriteString(".globl " + fnName + "\n")
	}

	buf.WriteString(fnName + ":\n")
	for _, block := range fn.Blocks {
		if block.Label != "entry" {
			buf.WriteString(block.Label + ":\n")
		}

		for _, inst := range block.Instr {
			buf.WriteString("  " + inst.String() + "\n")
		}
		buf.WriteString("\n")
	}

	return buf.String()
}

func (r RV64IMAFD) formatGlobal(g *asm.Global) string {
	var buf strings.Builder

	align := int(math.Log2(float64(g.Size)))
	buf.WriteString(fmt.Sprintf(".align %d\n", align))
	buf.WriteString(fmt.Sprintf("%s: .skip %d\n\n", g.Name, g.Size))

	return buf.String()
}
