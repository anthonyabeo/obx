package riscv

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/asm"
)

type RV64IMAFD struct {
	Register *target.RegisterFile
	Frame    *target.FrameInfo
}

func NewRV64IMAFDTarget() *RV64IMAFD {
	return &RV64IMAFD{
		Register: &target.RegisterFile{
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
			CallerSaved: map[string]bool{
				"ra": true, // x1
				"t0": true, // x5
				"t1": true, // x6
				"t2": true, // x7
				"a0": true, // x10
				"a1": true, // x11
				"a2": true, // x12
				"a3": true, // x13
				"a4": true, // x14
				"a5": true, // x15
				"a6": true, // x16
				"a7": true, // x17
				"t3": true, // x28
				"t4": true, // x29
				"t5": true, // x30
				"t6": true, // x31
			},
			CalleeSaved: map[string]bool{
				"sp":  true, //  x2
				"fp":  true, // s0/x8
				"s1":  true, //  x9
				"s2":  true, //  x18
				"s3":  true, //  x19
				"s4":  true, //  x20
				"s5":  true, //  x21
				"s6":  true, //  x22
				"s7":  true, //  x23
				"s8":  true, //  x24
				"s9":  true, //  x25
				"s10": true, //  x26
				"s11": true, //  x27
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
			MaxArgRegs: 8,
		},
		Frame: &target.FrameInfo{
			WordSize:    8,
			PointerSize: 8,
			FrameAlign:  16,
		},
	}
}

func (r RV64IMAFD) Name() string { return "rv64imafd" }

func (r RV64IMAFD) InstrInfo() {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) RegisterInfo() *target.RegisterFile { return r.Register }

func (r RV64IMAFD) FrameInfo() *target.FrameInfo { return r.Frame }

func (r RV64IMAFD) Legalize(fn *asm.Function) {
	for _, block := range fn.Blocks {
		for _, instr := range block.Instr {
			switch instr.Opcode {
			case "store":
				if len(instr.Operands) != 2 {
					panic("store instruction must have exactly 2 operands")
				}

				dst := instr.Operands[1]
				switch addr := dst.(type) {
				case *asm.MemAddr:
				case *asm.Symbol:
					switch r.Alignment(addr.Ty) {
					case 1:
						instr.Opcode = "sb"
					case 2:
						instr.Opcode = "sh"
					case 4:
						instr.Opcode = "sw"
					case 8:
						instr.Opcode = "sd"
					default:
						panic("unsupported alignment for store symbol")
					}
				}
			case "load":
				if len(instr.Operands) != 2 {
					panic("load instruction must have exactly 2 operands")
				}

				src := instr.Operands[1]
				switch addr := src.(type) {
				case *asm.MemAddr:
				case *asm.Symbol:
					switch r.Alignment(addr.Ty) {
					case 1:
						instr.Opcode = "lb"
					case 2:
						instr.Opcode = "lh"
					case 4:
						instr.Opcode = "lw"
					case 8:
						instr.Opcode = "ld"
					default:
						panic("unsupported alignment for load symbol")
					}
				}
			default:
				continue
			}
		}
	}
}

func (r RV64IMAFD) Alignment(ty asm.Type) int {
	switch t := ty.(type) {
	case *asm.BasicType:
		switch t.Kind {
		case asm.I8, asm.U8:
			return 1
		case asm.I16, asm.U16:
			return 2
		case asm.I32, asm.U32, asm.F32:
			return 4
		case asm.I64, asm.U64, asm.F64:
			return 8
		case asm.Ptr:
			return r.FrameInfo().PointerSize
		default:
			panic("unknown basic type for alignment")
		}
	case *asm.ArrayType:
		return r.Alignment(t.Element)
	default:
		panic("unknown type for alignment")
	}
}

func (r RV64IMAFD) EmitPrologueEpilogue(fn *asm.Function, layout target.FrameLayout) {
	fs := layout.FrameSize

	sp := &asm.Register{Name: "sp", Mode: asm.Phys, Kind: asm.GPR}
	fp := &asm.Register{Name: "fp", Mode: asm.Phys, Kind: asm.GPR}

	var (
		prologue []*asm.Instr
		epilogue []*asm.Instr
	)

	// ---------- PROLOGUE ----------
	prologue = append(prologue, &asm.Instr{
		Opcode:   "addi",
		Operands: []asm.Operand{sp, sp, asm.Imm{Value: -fs}}},
	)

	for _, obj := range layout.Saves {
		spOffset := fs + obj.Offset

		reg := &asm.Register{Name: obj.Name, Mode: asm.Phys, Kind: asm.GPR}
		mem := &asm.MemAddr{Base: sp, Offset: asm.Imm{Value: spOffset}}
		prologue = append(prologue, &asm.Instr{
			Opcode:   "sd",
			Operands: []asm.Operand{reg, mem},
		})
	}

	// Set up frame pointer
	prologue = append(prologue, &asm.Instr{
		Opcode:   "addi",
		Operands: []asm.Operand{fp, sp, asm.Imm{Value: fs}},
	})
	fn.Entry.Instr = append(prologue, fn.Entry.Instr...)

	// ---------- EPILOGUE ----------
	for i := len(layout.Saves) - 1; i >= 0; i-- {
		obj := layout.Saves[i]
		spOffset := fs + obj.Offset
		reg := &asm.Register{Name: obj.Name, Mode: asm.Phys, Kind: asm.GPR}
		mem := &asm.MemAddr{Base: sp, Offset: asm.Imm{Value: spOffset}}

		epilogue = append(epilogue, &asm.Instr{
			Opcode:   "ld",
			Operands: []asm.Operand{reg, mem},
		})
	}

	epilogue = append(epilogue,
		&asm.Instr{
			Opcode:   "addi",
			Operands: []asm.Operand{sp, sp, asm.Imm{Value: fs}},
		},
		&asm.Instr{Opcode: "ret"},
	)

	fn.Exit.Instr = append(fn.Exit.Instr, epilogue...)
}

func (r RV64IMAFD) Emit(module *asm.Module) string {
	var buf strings.Builder

	buf.WriteString("\t.section .bss\n")
	for _, global := range module.Globals {
		buf.WriteString(r.formatGlobal(global))
	}

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

func resolve(c asm.Constant) string {
	switch c.Type.(type) {
	case *asm.StringType:
		return c.Value.(string)
	default:
		panic("unknown type for constant")
	}
}

func (r RV64IMAFD) formatFunc(fn *asm.Function) string {
	var buf strings.Builder

	buf.WriteString("\t.section .rodata\n")
	for _, c := range fn.Constant {
		value := resolve(c)
		buf.WriteString(fmt.Sprintf("%s: %s \"%s\"\n", c.Name, r.toType(c.Type), strings.Trim(strconv.Quote(value), `"`)))
	}
	buf.WriteString("\n")

	buf.WriteString("\t.section .text\n")
	buf.WriteString("\t.align 2\n")

	fnName := fn.Name
	if strings.HasPrefix(fnName, "__init_") {
		fnName = "main"
		fn.Exported = true
	}

	if fn.Exported {
		buf.WriteString("\t.globl " + fnName + "\n")
	}
	buf.WriteString("\t.type " + fnName + ", @function\n")

	buf.WriteString(fnName + ":\n")
	for _, block := range fn.Blocks {
		if block.Label != "entry" {
			buf.WriteString(block.Label + ":\n")
		}

		for _, inst := range block.Instr {
			buf.WriteString("\t" + inst.String() + "\n")
		}

		if strings.HasSuffix(block.Label, "exit") {
			buf.WriteString("\t.size " + fnName + ", .-" + fnName + "\n")
		}

		buf.WriteString("\n")
	}

	return buf.String()
}

func (r RV64IMAFD) formatGlobal(g *asm.Symbol) string {
	var buf strings.Builder

	alignment := math.Min(float64(r.Alignment(g.Ty)), float64(r.FrameInfo().WordSize))

	align := int(math.Log2(alignment))
	buf.WriteString(fmt.Sprintf("\t.align %d\n", align))
	buf.WriteString(fmt.Sprintf("%s: .skip %d\n\n", g.Name, g.Size))

	return buf.String()
}

func (r RV64IMAFD) AssignParams(paramCount int) []target.Location {
	intRegIdx, stackOff := 0, 0

	locs := make([]target.Location, paramCount)
	wordSize := r.FrameInfo().WordSize

	for i := 0; i < paramCount; i++ {
		if intRegIdx < r.RegisterInfo().MaxArgRegs {
			locs[i] = target.Location{
				Kind:     target.InRegister,
				Register: fmt.Sprintf("a%d", intRegIdx),
				Size:     wordSize,
				Align:    wordSize,
			}
			intRegIdx++
		} else {
			// Align stack offset to 8 bytes
			stackOff = alignTo(stackOff, wordSize)
			locs[i] = target.Location{
				Kind:   target.OnStack,
				Offset: stackOff,
				Size:   wordSize,
				Align:  wordSize,
			}
			stackOff += wordSize
		}
	}
	return locs
}

func alignTo(x, align int) int {
	if x%align == 0 {
		return x
	}
	return ((x / align) + 1) * align
}

func (r RV64IMAFD) toType(ty asm.Type) string {
	switch ty.(type) {
	case *asm.StringType:
		return ".string"
	default:
		panic("unsupported type conversion")
	}
}
