package mir

import (
	"fmt"
	"io"
	"sort"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/mir"
)

func formatBlock(b *mir.Block) string {
	var sb strings.Builder
	sb.WriteString(b.Label + ":\n")
	for _, instr := range b.Instrs {
		sb.WriteString("  " + instr.String() + "\n")
	}

	sb.WriteString("\n")

	return sb.String()
}

func FormatFunction(fn *mir.Function) string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("func %s @%s(", fn.Result, fn.Name))
	var i int
	for _, p := range fn.Params {
		sb.WriteString(fmt.Sprintf("%s %s", p.Type(), p.Name()))
		if i < len(fn.Params)-1 {
			sb.WriteString(", ")
		}
	}
	sb.WriteString("):\n")

	// Print constants
	for _, c := range fn.Constants {
		sb.WriteString(fmt.Sprintf("  const %s: %s = %v\n", c.ID, c.Typ, c.Value))
	}

	// Print local variables
	for _, l := range fn.Locals {
		sb.WriteString(fmt.Sprintf("  var %s: %s\n", l.Name(), l.Type()))
	}

	// Collect and sort block IDs
	blocks := make([]int, 0, len(fn.Blocks))
	for id := range fn.Blocks {
		blocks = append(blocks, id)
	}
	sort.Ints(blocks)

	// Iterate in sorted order
	for _, id := range blocks {
		sb.WriteString(formatBlock(fn.Blocks[id]))
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
			sb.WriteString("\n")
		}
		sb.WriteString("\n")

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
