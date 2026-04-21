// Package arm64 implements the AArch64 (Apple Silicon) macOS code-generation
// target.  It registers under two names so both the "modern" triple and the
// LLVM-style triple are accepted:
//
//	arm64-apple-macos     (preferred)
//	aarch64-apple-darwin  (alias)
//
// The target follows the Apple AAPCS64 ABI:
//   - x0–x7   integer argument / result registers
//   - x8       indirect-result-location / scratch (caller-saved)
//   - x9–x17   caller-saved temporaries  (x16/x17 = IP0/IP1)
//   - x18      platform register – RESERVED on Apple, never allocated
//   - x19–x28  callee-saved general registers
//   - x29 (fp) frame pointer – callee-saved, always preserved in prologue
//   - x30 (lr) link register  – callee-saved, always preserved in prologue
//   - xzr / sp special-purpose, never allocated
//   - v0–v7    FP/SIMD argument / result (caller-saved)
//   - v8–v15   FP/SIMD callee-saved (only lower 64 bits need preservation)
//   - v16–v31  FP/SIMD caller-saved
package arm64

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/codegen/asm"
	"github.com/anthonyabeo/obx/src/codegen/target"
)

// ARM64AppleMacos is the concrete Machine implementation for arm64-apple-macos.
type ARM64AppleMacos struct {
	Register *target.RegisterFile
	Frame    *target.FrameInfo
}

// NewARM64AppleMacosTarget constructs a fully initialised ARM64AppleMacos.
func NewARM64AppleMacosTarget() *ARM64AppleMacos {
	return &ARM64AppleMacos{
		Register: &target.RegisterFile{
			AllRegs: []string{
				"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
				"x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
				"x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
				"x24", "x25", "x26", "x27", "x28", "x29", "x30", "xzr", "sp",
				"v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7",
				"v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15",
				"v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",
				"v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31",
			},
			GeneralRegs: []string{
				"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
				"x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
				"x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
				"x24", "x25", "x26", "x27", "x28", "x29", "x30", "xzr", "sp",
			},
			FloatRegs: []string{
				"v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7",
				"v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15",
				"v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",
				"v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31",
			},
			RegToIdx: map[string]int{
				"x0": 0, "x1": 1, "x2": 2, "x3": 3, "x4": 4,
				"x5": 5, "x6": 6, "x7": 7, "x8": 8, "x9": 9,
				"x10": 10, "x11": 11, "x12": 12, "x13": 13, "x14": 14,
				"x15": 15, "x16": 16, "x17": 17, "x18": 18, "x19": 19,
				"x20": 20, "x21": 21, "x22": 22, "x23": 23, "x24": 24,
				"x25": 25, "x26": 26, "x27": 27, "x28": 28,
				"x29": 29, // frame pointer
				"x30": 30, // link register
				"xzr": 31, "sp": 32,
				"v0": 33, "v1": 34, "v2": 35, "v3": 36, "v4": 37,
				"v5": 38, "v6": 39, "v7": 40, "v8": 41, "v9": 42,
				"v10": 43, "v11": 44, "v12": 45, "v13": 46, "v14": 47,
				"v15": 48, "v16": 49, "v17": 50, "v18": 51, "v19": 52,
				"v20": 53, "v21": 54, "v22": 55, "v23": 56, "v24": 57,
				"v25": 58, "v26": 59, "v27": 60, "v28": 61, "v29": 62,
				"v30": 63, "v31": 64,
			},
			// Allocatable: caller-saved scratch first (no save/restore overhead),
			// then argument registers, then callee-saved.
			// x18 (platform), x29 (fp), x30 (lr), xzr, sp are excluded.
			Allocatable: []string{
				// Caller-saved scratch
				"x9", "x10", "x11", "x12", "x13", "x14", "x15",
				"x16", "x17",
				// Argument / result registers (caller-saved)
				"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8",
				// Callee-saved
				"x19", "x20", "x21", "x22", "x23",
				"x24", "x25", "x26", "x27", "x28",
			},
			CallerSaved: map[string]bool{
				// Integer caller-saved (x18 is platform-reserved, omitted)
				"x0": true, "x1": true, "x2": true, "x3": true,
				"x4": true, "x5": true, "x6": true, "x7": true,
				"x8": true, "x9": true, "x10": true, "x11": true,
				"x12": true, "x13": true, "x14": true, "x15": true,
				"x16": true, "x17": true,
				// FP/SIMD caller-saved
				"v0": true, "v1": true, "v2": true, "v3": true,
				"v4": true, "v5": true, "v6": true, "v7": true,
				"v16": true, "v17": true, "v18": true, "v19": true,
				"v20": true, "v21": true, "v22": true, "v23": true,
				"v24": true, "v25": true, "v26": true, "v27": true,
				"v28": true, "v29": true, "v30": true, "v31": true,
			},
			CalleeSaved: map[string]bool{
				// Integer callee-saved
				"x19": true, "x20": true, "x21": true, "x22": true,
				"x23": true, "x24": true, "x25": true, "x26": true,
				"x27": true, "x28": true,
				"x29": true, // frame pointer
				"x30": true, // link register
				// FP/SIMD callee-saved (lower 64 bits must be preserved)
				"v8": true, "v9": true, "v10": true, "v11": true,
				"v12": true, "v13": true, "v14": true, "v15": true,
			},
			ArgRegs:      []string{"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"},
			ReturnRegs:   []string{"x0", "x1"},
			StackPointer: "sp",
			FramePointer: "x29",
			Reserved: []string{
				"xzr", // zero register
				"sp",  // stack pointer
				"x18", // platform register (reserved on Apple)
				"x29", // frame pointer
				"x30", // link register
			},
			Temporaries: []string{
				"x9", "x10", "x11", "x12", "x13",
				"x14", "x15", "x16", "x17",
			},
			MaxArgRegs: 8,
		},
		Frame: &target.FrameInfo{
			WordSize:       8,
			PointerSize:    8,
			FrameAlign:     16,
			StackGrowsDown: true,
		},
	}
}

// ─── Machine interface ────────────────────────────────────────────────────────

func (a ARM64AppleMacos) Name() string { return "arm64-apple-macos" }

func (a ARM64AppleMacos) InstrInfo() {
	// TODO: implement me
	panic("implement me")
}

func (a ARM64AppleMacos) RegisterInfo() *target.RegisterFile { return a.Register }

func (a ARM64AppleMacos) FrameInfo() *target.FrameInfo { return a.Frame }

// Alignment returns the natural alignment (in bytes) of a given asm type.
// For FP/SIMD types, F16→2, F32→4, F64→8 match AAPCS64 requirements.
func (a ARM64AppleMacos) Alignment(ty asm.Type) int {
	switch t := ty.(type) {
	case *asm.BasicType:
		switch t.Kind {
		case asm.I8, asm.U8:
			return 1
		case asm.I16, asm.U16, asm.F16:
			return 2
		case asm.I32, asm.U32, asm.F32:
			return 4
		case asm.I64, asm.U64, asm.F64:
			return 8
		case asm.Ptr:
			return a.FrameInfo().PointerSize
		default:
			panic(fmt.Sprintf("arm64: unknown basic type kind for alignment: %v", t.Kind))
		}
	case *asm.ArrayType:
		return a.Alignment(t.Element)
	case *asm.PointerType:
		return a.FrameInfo().PointerSize
	case *asm.RecordType:
		maxAlign := 1
		for _, field := range t.Fields {
			if fa := a.Alignment(field.Type); fa > maxAlign {
				maxAlign = fa
			}
		}
		return maxAlign
	default:
		panic(fmt.Sprintf("arm64: unknown type for alignment: %T", ty))
	}
}

// Legalize rewrites abstract load/store opcodes to concrete AArch64 variants
// based on the operand type's natural alignment.
//
//	load  → ldrb / ldrh / ldr  (1 / 2 / 4-or-8 bytes)
//	store → strb / strh / str
//
// Both MemAddr and Symbol operands are handled.
func (a ARM64AppleMacos) Legalize(fn *asm.Function) {
	for _, block := range fn.Blocks {
		for _, instr := range block.Instr {
			switch instr.Opcode {

			case "store":
				if len(instr.SrcOperands)+1 != 2 {
					panic("arm64: store must have exactly 2 operands (dst + 1 src)")
				}
				switch addr := instr.DstOperand.(type) {
				case *asm.MemAddr:
					// MemAddr carries no type info; fall back to word-sized store.
					instr.Opcode = "str"
				case *asm.Symbol:
					switch a.Alignment(addr.Ty) {
					case 1:
						instr.Opcode = "strb"
					case 2:
						instr.Opcode = "strh"
					case 4, 8:
						instr.Opcode = "str"
					default:
						panic(fmt.Sprintf("arm64: unsupported alignment for store symbol %q", addr.Name))
					}
				}

			case "load":
				if 1+len(instr.SrcOperands) != 2 {
					panic("arm64: load must have exactly 2 operands (dst + 1 src)")
				}
				switch addr := instr.SrcOperands[0].(type) {
				case *asm.MemAddr:
					// MemAddr carries no type info; default to word-sized load.
					instr.Opcode = "ldr"
				case *asm.Symbol:
					switch a.Alignment(addr.Ty) {
					case 1:
						instr.Opcode = "ldrb"
					case 2:
						instr.Opcode = "ldrh"
					case 4, 8:
						instr.Opcode = "ldr"
					default:
						panic(fmt.Sprintf("arm64: unsupported alignment for load symbol %q", addr.Name))
					}
				}

			default:
				continue
			}
		}
	}
}

// EmitPrologueEpilogue inserts AArch64 AAPCS64-compliant prologue and epilogue
// instructions into the function's entry and exit blocks.
//
// Frame layout (addresses descending from old SP):
//
//	[sp + (N-16)] : saved x29 (fp)
//	[sp + (N-8)]  : saved x30 (lr)
//	[sp + ...]    : callee-saved registers (from layout.Saves)
//	[sp + 0 ..]   : locals / spills / outgoing
//
// x29 (fp) is set to point at the saved-fp slot, satisfying the ABI
// requirement that fp always points to the previous frame record.
func (a ARM64AppleMacos) EmitPrologueEpilogue(fn *asm.Function, layout target.FrameLayout) {
	fs := layout.FrameSize

	sp := &asm.Register{Name: "sp", Mode: asm.Phys, Kind: asm.GPR}
	fp := &asm.Register{Name: "x29", Mode: asm.Phys, Kind: asm.GPR}
	lr := &asm.Register{Name: "x30", Mode: asm.Phys, Kind: asm.GPR}

	fpSaveOff := fs - 16 // [sp + (fs-16)] holds saved fp
	lrSaveOff := fs - 8  // [sp + (fs-8)]  holds saved lr

	var prologue, epilogue []*asm.Instr

	// ── PROLOGUE ────────────────────────────────────────────────────────────

	// 1. Allocate frame
	prologue = append(prologue, &asm.Instr{
		Opcode:      "sub",
		DstOperand:  sp,
		SrcOperands: []asm.Operand{sp, asm.Imm{Value: fs}},
	})

	// 2. Save x29 (fp) and x30 (lr) – always required by AAPCS64
	prologue = append(prologue,
		&asm.Instr{
			Opcode:      "str",
			DstOperand:  &asm.MemAddr{Base: sp, Offset: asm.Imm{Value: fpSaveOff}},
			SrcOperands: []asm.Operand{fp},
		},
		&asm.Instr{
			Opcode:      "str",
			DstOperand:  &asm.MemAddr{Base: sp, Offset: asm.Imm{Value: lrSaveOff}},
			SrcOperands: []asm.Operand{lr},
		},
	)

	// 3. Point frame pointer at saved-fp slot
	prologue = append(prologue, &asm.Instr{
		Opcode:      "add",
		DstOperand:  fp,
		SrcOperands: []asm.Operand{sp, asm.Imm{Value: fpSaveOff}},
	})

	// 4. Save other callee-saved registers (x29/x30 handled above)
	for _, obj := range layout.Saves {
		if obj.Name == "x29" || obj.Name == "x30" {
			continue
		}
		kind := regKindFor(obj.Name)
		reg := &asm.Register{Name: obj.Name, Mode: asm.Phys, Kind: kind}
		mem := &asm.MemAddr{Base: sp, Offset: asm.Imm{Value: fs + obj.Offset}}
		prologue = append(prologue, &asm.Instr{
			Opcode:      "str",
			DstOperand:  mem,
			SrcOperands: []asm.Operand{reg},
		})
	}

	fn.Entry.Instr = append(prologue, fn.Entry.Instr...)

	// ── EPILOGUE ────────────────────────────────────────────────────────────

	// 1. Restore other callee-saved regs (reverse order, skip x29/x30)
	for i := len(layout.Saves) - 1; i >= 0; i-- {
		obj := layout.Saves[i]
		if obj.Name == "x29" || obj.Name == "x30" {
			continue
		}
		kind := regKindFor(obj.Name)
		reg := &asm.Register{Name: obj.Name, Mode: asm.Phys, Kind: kind}
		mem := &asm.MemAddr{Base: sp, Offset: asm.Imm{Value: fs + obj.Offset}}
		epilogue = append(epilogue, &asm.Instr{
			Opcode:      "ldr",
			DstOperand:  reg,
			SrcOperands: []asm.Operand{mem},
		})
	}

	// 2. Restore x29 (fp) and x30 (lr)
	epilogue = append(epilogue,
		&asm.Instr{
			Opcode:      "ldr",
			DstOperand:  fp,
			SrcOperands: []asm.Operand{&asm.MemAddr{Base: sp, Offset: asm.Imm{Value: fpSaveOff}}},
		},
		&asm.Instr{
			Opcode:      "ldr",
			DstOperand:  lr,
			SrcOperands: []asm.Operand{&asm.MemAddr{Base: sp, Offset: asm.Imm{Value: lrSaveOff}}},
		},
	)

	// 3. Deallocate frame
	epilogue = append(epilogue, &asm.Instr{
		Opcode:      "add",
		DstOperand:  sp,
		SrcOperands: []asm.Operand{sp, asm.Imm{Value: fs}},
	})

	// 4. Return
	epilogue = append(epilogue, &asm.Instr{Opcode: "ret"})

	fn.Exit.Instr = append(fn.Exit.Instr, epilogue...)
}

// AssignParams maps the first 8 parameters to x0–x7; the rest spill to stack.
func (a ARM64AppleMacos) AssignParams(paramCount int) []target.Location {
	intRegIdx, stackOff := 0, 0
	locs := make([]target.Location, paramCount)
	wordSize := a.FrameInfo().WordSize

	for i := 0; i < paramCount; i++ {
		if intRegIdx < a.RegisterInfo().MaxArgRegs {
			locs[i] = target.Location{
				Kind:     target.InRegister,
				Register: fmt.Sprintf("x%d", intRegIdx),
				Size:     wordSize,
				Align:    wordSize,
			}
			intRegIdx++
		} else {
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

// Emit renders the module as Mach-O AArch64 assembly text.
func (a ARM64AppleMacos) Emit(module *asm.Module) string {
	var buf strings.Builder

	// Emit .extern declarations for all foreign symbols (§12.4).
	for _, ext := range module.Externals {
		buf.WriteString(fmt.Sprintf("\t.extern _%s\n", ext.CName))
	}

	buf.WriteString("\t.section __DATA,__bss\n")
	for _, global := range module.Globals {
		buf.WriteString(a.formatGlobal(global))
	}

	// Sort function names for deterministic output (map iteration is random).
	var funcNames []string
	funcMap := make(map[string]*asm.Function)
	for _, fn := range module.Funcs {
		funcNames = append(funcNames, fn.Name)
		funcMap[fn.Name] = fn
	}
	for _, name := range funcNames {
		buf.WriteString(formatFunc(funcMap[name]))
	}

	return buf.String()
}

// ─── init ─────────────────────────────────────────────────────────────────────

func init() {
	factory := func() target.Machine { return NewARM64AppleMacosTarget() }
	target.Register("arm64-apple-macos", factory)
	target.Register("aarch64-apple-darwin", factory)
}

// ─── helpers ──────────────────────────────────────────────────────────────────

// regKindFor returns GPR for integer registers and FPR for v-registers.
func regKindFor(name string) asm.RegKind {
	if strings.HasPrefix(name, "v") {
		return asm.FPR
	}
	return asm.GPR
}
