package riscv

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/codegen/asm"
)

func alignTo(x, align int) int {
	if x%align == 0 {
		return x
	}
	return ((x / align) + 1) * align
}

func toType(ty asm.Type) string {
	switch ty.(type) {
	case *asm.StringType:
		return ".string"
	default:
		panic("unsupported type conversion")
	}
}

func resolve(c asm.Constant) string {
	switch c.Type.(type) {
	case *asm.StringType:
		return c.Value.(string)
	default:
		panic("unknown type for constant")
	}
}

func formatFunc(fn *asm.Function) string {
	var buf strings.Builder

	buf.WriteString("\t.section .rodata\n")
	for _, c := range fn.Constant {
		value := resolve(c)
		buf.WriteString(fmt.Sprintf("%s: %s \"%s\"\n", c.Name, toType(c.Type), strings.Trim(strconv.Quote(value), `"`)))
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
			buf.WriteString("\t" + formatInstr(inst) + "\n")
		}

		if strings.HasSuffix(block.Label, "exit") {
			buf.WriteString("\t.size " + fnName + ", .-" + fnName + "\n")
		}

		buf.WriteString("\n")
	}

	return buf.String()
}

// formatModuleConstants emits module-level constants (strings, arrays) in
// .rodata so they are available as global symbols.
func formatModuleConstants(constants []asm.Constant) string {
	var buf strings.Builder
	if len(constants) == 0 {
		return ""
	}

	buf.WriteString("\t.section .rodata\n")
	for _, c := range constants {
		// Only string constants are currently supported here.
		switch t := c.Type.(type) {
		case *asm.StringType:
			value := c.Value.(string)
			buf.WriteString(fmt.Sprintf("%s: %s \"%s\"\n", c.Name, toType(c.Type), strings.Trim(strconv.Quote(value), `"`)))
		default:
			// Unknown constant types could be extended (arrays, pointers).
			// For now skip them to avoid emitting invalid data.
			_ = t
		}
	}
	buf.WriteString("\n")
	return buf.String()
}

func formatInstr(ins *asm.Instr) string {
	switch ins.Opcode {
	case "sw", "sd", "sb", "sh":
		return fmt.Sprintf("%s %s, %s", ins.Opcode, ins.SrcOperands[0], ins.DstOperand)
	case "lw", "ld", "lb", "lh":
		return fmt.Sprintf("%s %s, %s", ins.Opcode, ins.DstOperand, ins.SrcOperands[0])
	case "j":
		return fmt.Sprintf("%s %s", ins.Opcode, ins.SrcOperands[0])
	case "ret":
		return "ret"
	default:
		var operands []string
		for _, op := range ins.SrcOperands {
			operands = append(operands, op.String())
		}

		if ins.DstOperand != nil {
			return fmt.Sprintf("%s %s, %s", ins.Opcode, ins.DstOperand, strings.Join(operands, ", "))
		}

		return fmt.Sprintf("%s %s", ins.Opcode, strings.Join(operands, ", "))
	}
}

func (r RV64IMAFD) formatGlobal(g *asm.Symbol) string {
	var buf strings.Builder

	alignment := math.Min(float64(r.Alignment(g.Ty)), float64(r.FrameInfo().WordSize))

	align := int(math.Log2(alignment))
	buf.WriteString(fmt.Sprintf("\t.align %d\n", align))
	buf.WriteString(fmt.Sprintf("%s: .skip %d\n\n", g.Name, g.Size))

	return buf.String()
}
