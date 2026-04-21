package arm64

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/codegen/asm"
)

// ─── Alignment helper ─────────────────────────────────────────────────────────

func alignTo(x, align int) int {
	if align == 0 || x%align == 0 {
		return x
	}
	return ((x / align) + 1) * align
}

// ─── Function formatter ───────────────────────────────────────────────────────

// formatFunc emits a single function in Mach-O AArch64 assembly.
//
// Differences from RISC-V / ELF:
//   - Section names use __TEXT,__text  and __TEXT,__cstring
//   - Global symbols are prefixed with '_'  (Mach-O ABI requirement)
//   - .p2align 2  (= 4-byte code alignment) instead of .align 2
//   - No .type … @function  directive (ELF-only)
//   - No .size  directive (ELF-only)
//   - Block labels inside the function body are emitted as  Lname  (local)
func formatFunc(fn *asm.Function) string {
	var buf strings.Builder

	// ── String / rodata constants ─────────────────────────────────────────
	if len(fn.Constant) > 0 {
		buf.WriteString("\t.section __TEXT,__cstring,cstring_literals\n")
		for _, c := range fn.Constant {
			value := resolveConst(c)
			quoted := strings.Trim(strconv.Quote(value), `"`)
			buf.WriteString(fmt.Sprintf("_%s:\n\t.asciz \"%s\"\n", c.Name, quoted))
		}
		buf.WriteString("\n")
	}

	// ── Code section ──────────────────────────────────────────────────────
	buf.WriteString("\t.section __TEXT,__text\n")
	buf.WriteString("\t.p2align 2\n")

	fnName := fn.Name
	if strings.HasPrefix(fnName, "__init_") {
		fnName = "main"
		fn.Exported = true
	}

	mangled := "_" + fnName // Mach-O symbols require underscore prefix
	if fn.Exported {
		buf.WriteString("\t.globl " + mangled + "\n")
	}
	buf.WriteString(mangled + ":\n")

	for _, block := range fn.Blocks {
		// entry block has no label of its own
		if block.Label != "entry" {
			buf.WriteString("L" + block.Label + ":\n")
		}

		for _, inst := range block.Instr {
			buf.WriteString("\t" + formatInstr(inst) + "\n")
		}
		buf.WriteString("\n")
	}

	return buf.String()
}

// ─── Instruction formatter ────────────────────────────────────────────────────

// formatInstr formats a single AArch64 instruction as an assembly string.
// Operand immediates are prefixed with '#' and memory addresses use the
// [base, #offset] bracket syntax required by Apple's ARM64 assembler.
func formatInstr(ins *asm.Instr) string {
	switch ins.Opcode {

	// ── Stores: opcode src, [mem] ────────────────────────────────────────
	case "str", "strb", "strh":
		return fmt.Sprintf("%s %s, %s",
			ins.Opcode,
			formatOperand(ins.SrcOperands[0]),
			formatMemArg(ins.DstOperand))

	// ── Loads: opcode dst, [mem] ─────────────────────────────────────────
	case "ldr", "ldrb", "ldrh", "ldrsb", "ldrsh", "ldrsw":
		src := ins.SrcOperands[0]
		// Fallback for un-expanded global symbol loads
		if sym, ok := src.(*asm.Symbol); ok {
			return fmt.Sprintf("ldr %s, =_%s", ins.DstOperand, sym.Name)
		}
		return fmt.Sprintf("%s %s, %s",
			ins.Opcode,
			ins.DstOperand,
			formatMemArg(src))

	// ── Store pair: stp r1, r2, [mem] ───────────────────────────────────
	// Encoding: DstOperand = mem, SrcOperands = [r1, r2]
	case "stp":
		return fmt.Sprintf("stp %s, %s, %s",
			formatOperand(ins.SrcOperands[0]),
			formatOperand(ins.SrcOperands[1]),
			formatMemArg(ins.DstOperand))

	// ── Load pair: ldp r1, r2, [mem] ────────────────────────────────────
	// Encoding: DstOperand = r1, SrcOperands = [r2, mem]
	case "ldp":
		return fmt.Sprintf("ldp %s, %s, %s",
			ins.DstOperand,
			formatOperand(ins.SrcOperands[0]),
			formatMemArg(ins.SrcOperands[1]))

	// ── Unconditional branch / call ──────────────────────────────────────
	case "b":
		return fmt.Sprintf("b L%s", ins.SrcOperands[0])

	case "bl":
		// Function calls use '_'-prefixed Mach-O symbol names.
		if l, ok := ins.SrcOperands[0].(*asm.Label); ok {
			return fmt.Sprintf("bl _%s", l.Name)
		}
		return fmt.Sprintf("bl %s", formatOperand(ins.SrcOperands[0]))

	// ── Conditional branches ─────────────────────────────────────────────
	case "b.eq", "b.ne", "b.lt", "b.le", "b.gt", "b.ge":
		return fmt.Sprintf("%s L%s", ins.Opcode, ins.SrcOperands[0])

	// ── Compare and branch if (non-)zero ────────────────────────────────
	case "cbz", "cbnz":
		return fmt.Sprintf("%s %s, L%s",
			ins.Opcode,
			formatOperand(ins.SrcOperands[0]),
			ins.SrcOperands[1])

	// ── Compare (sets flags, no destination register) ────────────────────
	case "cmp":
		parts := make([]string, len(ins.SrcOperands))
		for i, op := range ins.SrcOperands {
			parts[i] = formatOperand(op)
		}
		return fmt.Sprintf("cmp %s", strings.Join(parts, ", "))

	// ── Conditional set ──────────────────────────────────────────────────
	case "cset":
		// SrcOperands[0] is a Label holding the condition code (e.g. "eq").
		return fmt.Sprintf("cset %s, %s", ins.DstOperand, ins.SrcOperands[0])

	// ── Return ───────────────────────────────────────────────────────────
	case "ret":
		return "ret"

	// ── Large-immediate materialisation (movz / movk) ───────────────────
	//
	// These are generated by MOVri_LargeImm.  The reloc operand carries the
	// integer value as a decimal string in rf.Symbol; we split it into the
	// upper-16 and lower-16 halves.
	case "movz":
		if rf, ok := ins.SrcOperands[0].(*asm.RelocFunc); ok && rf.Kind == asm.Hi {
			if v, err := strconv.ParseInt(rf.Symbol, 10, 64); err == nil {
				hi16 := (v >> 16) & 0xFFFF
				return fmt.Sprintf("movz %s, #0x%x, lsl #16", ins.DstOperand, hi16)
			}
			// Symbol-form reloc (address upper half)
			return fmt.Sprintf("movz %s, #:abs_g1:_%s", ins.DstOperand, rf.Symbol)
		}
		return formatGeneric(ins)

	case "movk":
		// SrcOperands = [rd, reloc:lo(val)]
		if len(ins.SrcOperands) >= 2 {
			if rf, ok := ins.SrcOperands[1].(*asm.RelocFunc); ok && rf.Kind == asm.Lo {
				if v, err := strconv.ParseInt(rf.Symbol, 10, 64); err == nil {
					lo16 := v & 0xFFFF
					return fmt.Sprintf("movk %s, #0x%x", ins.DstOperand, lo16)
				}
				return fmt.Sprintf("movk %s, #:abs_g0:_%s", ins.DstOperand, rf.Symbol)
			}
		}
		return formatGeneric(ins)

	// ── ADRP: emit  adrp rd, _sym@PAGE ──────────────────────────────────
	case "adrp":
		if len(ins.SrcOperands) > 0 {
			return fmt.Sprintf("adrp %s, %s",
				ins.DstOperand,
				formatRelocArm64(ins.SrcOperands[0]))
		}
		return formatGeneric(ins)

	// ── la_str: two-instruction string-constant address load ─────────────
	// Generated by Mov_r_str_const; formatted as adrp+add inline.
	case "la_str":
		if str, ok := ins.SrcOperands[0].(*asm.String); ok {
			reg := ins.DstOperand.String()
			return fmt.Sprintf("adrp %s, _%s@PAGE\n\tadd %s, %s, _%s@PAGEOFF",
				reg, str.Name, reg, reg, str.Name)
		}
		return formatGeneric(ins)

	// ── Default: dst, src, src, … ────────────────────────────────────────
	default:
		return formatGeneric(ins)
	}
}

// formatGeneric formats an instruction in the common  opcode [dst,] src...  form,
// with ARM64 '#' prefixes on immediates and @PAGE/@PAGEOFF on relocations.
func formatGeneric(ins *asm.Instr) string {
	parts := make([]string, len(ins.SrcOperands))
	for i, op := range ins.SrcOperands {
		parts[i] = formatOperand(op)
	}
	if ins.DstOperand != nil {
		return fmt.Sprintf("%s %s, %s",
			ins.Opcode, ins.DstOperand, strings.Join(parts, ", "))
	}
	return fmt.Sprintf("%s %s", ins.Opcode, strings.Join(parts, ", "))
}

// ─── Operand formatters ───────────────────────────────────────────────────────

// formatOperand converts a single operand to its ARM64 assembly string:
//   - Imm (value type)  → "#N"
//   - *asm.Imm          → "#N"
//   - *asm.RelocFunc     → _sym@PAGE or _sym@PAGEOFF
//   - anything else      → op.String()
func formatOperand(op asm.Operand) string {
	switch v := op.(type) {
	case asm.Imm:
		return fmt.Sprintf("#%v", v.Value)
	case *asm.Imm:
		return fmt.Sprintf("#%v", v.Value)
	case *asm.RelocFunc:
		return formatRelocArm64(op)
	case *asm.Symbol:
		return "_" + v.Name
	default:
		return op.String()
	}
}

// formatMemArg formats a memory operand as [base, #offset] or [base].
// For relocations in the offset position, @PAGE / @PAGEOFF notation is used.
func formatMemArg(op asm.Operand) string {
	m, ok := op.(*asm.MemAddr)
	if !ok {
		return op.String()
	}
	if m.Offset == nil {
		return fmt.Sprintf("[%s]", m.Base)
	}
	return fmt.Sprintf("[%s, %s]", m.Base, formatOperand(m.Offset))
}

// formatRelocArm64 converts an asm.RelocFunc to Apple Mach-O relocation syntax:
//
//	%hi(sym) → _sym@PAGE
//	%lo(sym) → _sym@PAGEOFF
func formatRelocArm64(op asm.Operand) string {
	rf, ok := op.(*asm.RelocFunc)
	if !ok {
		return op.String()
	}
	sym := "_" + rf.Symbol
	switch rf.Kind {
	case asm.Hi, asm.Pcrel_hi:
		return sym + "@PAGE"
	case asm.Lo, asm.Pcrel_lo:
		return sym + "@PAGEOFF"
	default:
		return rf.String()
	}
}

// ─── Global formatter ─────────────────────────────────────────────────────────

// formatGlobal emits a BSS global in Mach-O format.
//
//	.section __DATA,__bss
//	.p2align <log2(align)>
//	_name:
//	  .zero <size>
func (a ARM64AppleMacos) formatGlobal(g *asm.Symbol) string {
	var buf strings.Builder

	rawAlign := float64(a.Alignment(g.Ty))
	capped := math.Min(rawAlign, float64(a.FrameInfo().WordSize))
	p2align := int(math.Log2(capped))

	buf.WriteString(fmt.Sprintf("\t.p2align %d\n", p2align))
	buf.WriteString(fmt.Sprintf("_%s:\n\t.zero %d\n\n", g.Name, g.Size))
	return buf.String()
}

// ─── Constant resolver ────────────────────────────────────────────────────────

func resolveConst(c asm.Constant) string {
	switch c.Type.(type) {
	case *asm.StringType:
		return c.Value.(string)
	default:
		panic(fmt.Sprintf("arm64: unknown constant type %T", c.Type))
	}
}
