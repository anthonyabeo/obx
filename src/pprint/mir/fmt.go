package mir

import (
	"fmt"
	"io"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/mir"
)

func formatBlock(b *mir.Block) string {
	var sb strings.Builder
	sb.WriteString(b.Label + ":\n")
	for _, instr := range b.Instrs {
		sb.WriteString("  " + instr.String() + "\n")
	}
	if b.Term != nil {
		sb.WriteString("  " + b.Term.String() + "\n")
	}
	return sb.String()
}

func FormatFunction(fn *mir.Function) string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("func %s @%s(", fn.Result, fn.Name))
	for i, p := range fn.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.Name())
	}
	sb.WriteString("):\n")

	for _, block := range fn.Blocks {
		sb.WriteString(formatBlock(block))
	}

	sb.WriteString("end\n\n")
	return sb.String()
}

func FormatProgram(p *mir.Program) string {
	var sb strings.Builder
	for _, m := range p.Modules {
		sb.WriteString(fmt.Sprintf("module %s:\n", m.Name))
		for _, g := range m.Globals {
			sb.WriteString(fmt.Sprintf("@%s = global %s", g.Name(), g.Kind))
			if g.Value != nil {
				sb.WriteString(fmt.Sprintf(" := %v", g.Value))
			}
			sb.WriteString("\n\n")
		}
		for _, f := range m.Funcs {
			sb.WriteString(FormatFunction(f))
		}
		sb.WriteString("end\n")
	}
	return sb.String()
}

func EmitMIR(w io.Writer, program *mir.Program) error {
	output := FormatProgram(program)
	_, err := w.Write([]byte(output))
	return err
}
