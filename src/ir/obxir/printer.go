package obxir

import (
	"fmt"
	"io"
	"sort"
	"strings"
)

func formatBlock(b *Block) string {
	var sb strings.Builder
	sb.WriteString(b.Label + ":\n")
	for _, instr := range b.Instrs {
		sb.WriteString("  " + instr.String() + "\n")
	}
	sb.WriteString("\n")
	return sb.String()
}

// FormatFunction renders fn as a human-readable text IR string.
func FormatFunction(fn *Function) string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("func %s @%s(", fn.Result, fn.Name()))
	for i, p := range fn.Params {
		sb.WriteString(fmt.Sprintf("%s %s", p.Type(), p.Name()))
		if i < len(fn.Params)-1 {
			sb.WriteString(", ")
		}
	}
	sb.WriteString("):\n")

	for _, c := range fn.Constants {
		sb.WriteString(fmt.Sprintf("  const %s: %s = %v\n", c.Name(), c.Type(), c.Value()))
	}
	for _, l := range fn.Locals {
		sb.WriteString(fmt.Sprintf("  var %s: %s\n", l.Name(), l.Type()))
	}

	ids := make([]int, 0, len(fn.Blocks))
	for id := range fn.Blocks {
		ids = append(ids, id)
	}
	sort.Ints(ids)
	for _, id := range ids {
		sb.WriteString(formatBlock(fn.Blocks[id]))
	}

	sb.WriteString("end\n\n")
	return sb.String()
}

// FormatProgram renders every module and function in p as text IR.
func FormatProgram(p *Program) string {
	var sb strings.Builder
	for _, m := range p.Modules {
		sb.WriteString(fmt.Sprintf("module %s:\n", m.Name))
		for _, g := range m.Globals {
			sb.WriteString(fmt.Sprintf("%s = global %s\n", g.Name(), g.Typ))
		}
		sb.WriteString("\n")
		for _, f := range m.Funcs {
			sb.WriteString(FormatFunction(f))
		}
		sb.WriteString("end\n")
	}
	return sb.String()
}

// EmitIR writes the text IR of p to w.
func EmitIR(w io.Writer, p *Program) error {
	_, err := w.Write([]byte(FormatProgram(p)))
	return err
}
