package riscv

import (
	"fmt"
	"math"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/target"
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
			"ra":  1,
			"sp":  2,
			"gp":  3,
			"tp":  4,
			"t0":  5,
			"t1":  6,
			"t2":  7,
			"s0":  8,
			"fp":  8,
			"s1":  9,
			"a0":  10,
			"a1":  11,
			"a2":  12,
			"a3":  13,
			"a4":  14,
			"a5":  15,
			"a6":  16,
			"a7":  17,
			"s2":  18,
			"s3":  19,
			"s4":  20,
			"s5":  21,
			"s6":  22,
			"s7":  23,
			"s8":  24,
			"s9":  25,
			"s10": 26,
			"s11": 27,
			"t3":  28,
			"t4":  29,
			"t5":  30,
			"t6":  31,
			"f0":  32,
			"f1":  33,
			"f2":  34,
			"f3":  35,
			"f4":  36,
			"f5":  37,
			"f6":  38,
			"f7":  39,
			"f8":  40,
			"f9":  41,
			"f10": 42,
			"f11": 43,
			"f12": 44,
			"f13": 45,
			"f14": 46,
			"f15": 47,
			"f16": 48,
			"f17": 49,
			"f18": 50,
			"f19": 51,
			"f20": 52,
			"f21": 53,
			"f22": 54,
			"f23": 55,
			"f24": 56,
			"f25": 57,
			"f26": 58,
			"f27": 59,
			"f28": 60,
			"f29": 61,
			"f30": 62,
			"f31": 63,
		},
		Allocatable: []string{
			"t0",  // x5
			"t1",  // x6
			"t2",  // x7
			"a0",  // x10
			"a1",  // x11
			"a2",  // x12
			"a3",  // x13
			"a4",  // x14
			"a5",  // x15
			"a6",  // x16
			"a7",  // x17
			"t3",  // x28
			"t4",  // x29
			"t5",  // x30
			"t6",  // x31
			"s0",  // x8/fp
			"s1",  // x9
			"s2",  // x18
			"s3",  // x19
			"s4",  // x20
			"s5",  // x21
			"s6",  // x22
			"s7",  // x23
			"s8",  // x24
			"s9",  // x25
			"s10", // x26
			"s11", // x27
		},
		CallerSaved: []string{
			"ra", // x1
			"t0", // x5
			"t1", // x6
			"t2", // x7
			"a0", // x10
			"a1", // x11
			"a2", // x12
			"a3", // x13
			"a4", // x14
			"a5", // x15
			"a6", // x16
			"a7", // x17
			"t3", // x28
			"t4", // x29
			"t5", // x30
			"t6", // x31
		},
		CalleeSaved: []string{
			"sp",  //  x2
			"fp",  // s0/x8
			"s1",  //  x9
			"s2",  //  x18
			"s3",  //  x19
			"s4",  //  x20
			"s5",  //  x21
			"s6",  //  x22
			"s7",  //  x23
			"s8",  //  x24
			"s9",  //  x25
			"s10", //  x26
			"s11", //  x27
		},
		ArgRegs: []string{
			"a0", // x10
			"a1", // x11
			"a2", // x12
			"a3", // x13
			"a4", // x14
			"a5", // x15
			"a6", // x16
			"a7", // x17
		},
		ReturnRegs: []string{
			"a0", // x10
			"a1", // x11
		},
		StackPointer: "sp", // x2
		FramePointer: "fp", // s0/x8
		Reserved: []string{
			"x0", // zero
			"ra", // x1
			"sp", // x2
			"gp", // x3
			"tp", // x4
			"fp", // s0/x8
		},
		Temporaries: []string{
			"t0", // x5
			"t1", //  x6
			"t2", //  x7
			"t3", //  x28
			"t4", //  x29
			"t5", //  x30
			"t6", //  x31
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
