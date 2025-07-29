package mir

import (
	"fmt"
	"io"
	"strings"
)

// FormatProgram formats an entire MIR program with all its modules.
func FormatProgram(prog *Program) string {
	var b strings.Builder
	for _, mod := range prog.Modules {
		b.WriteString(FormatModule(mod, "  "))
		b.WriteString("\n") // Optional separation between modules
	}
	return b.String()
}

// WriteProgram writes the formatted MIR program to an io.Writer (e.g. file or buffer).
func WriteProgram(w io.Writer, prog *Program) error {
	_, err := fmt.Fprint(w, FormatProgram(prog))
	return err
}

func FormatModule(mod *Module, indent string) string {
	var b strings.Builder

	// Header
	b.WriteString(fmt.Sprintf("module %s", mod.Name))
	if mod.IsEntry {
		b.WriteString(" [entry]")
	}
	b.WriteString(" {\n")

	// Imports
	if len(mod.Imports) > 0 {
		b.WriteString("  imports:\n")
		for _, imp := range mod.Imports {
			if imp.Alias != "" && imp.Alias != imp.Path {
				b.WriteString(fmt.Sprintf("    %s => %s\n", imp.Alias, imp.Path))
			} else {
				b.WriteString(fmt.Sprintf("    %s\n", imp.Path))
			}
		}
		b.WriteString("\n")
	}

	// Globals
	if len(mod.Decl) > 0 {
		b.WriteString(indent)
		b.WriteString("globals:\n")
		for _, g := range mod.Decl {
			switch g := g.(type) {
			case *Function:
				b.WriteString(indent + "  ")
				b.WriteString(FormatFunction(g, indent+"  "))
				b.WriteString("\n")
			}

		}
		b.WriteString("\n")
	}

	// Init procedure
	if mod.Init != nil {
		b.WriteString("  init:\n")
		b.WriteString(FormatFunction(mod.Init, indent))
	}

	b.WriteString("}\n")
	return b.String()
}

func FormatFunction(fn *Function, indent string) string {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("fn @%s(", fn.Name))
	for i, p := range fn.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.Name + ": " + p.Type.String())
	}
	sb.WriteString(")")
	if fn.Result != nil {
		sb.WriteString(fmt.Sprintf(" -> %s", fn.Result))
	}
	sb.WriteString("\n")

	for _, blk := range fn.Blocks {
		sb.WriteString(indent)
		sb.WriteString(FormatBlock(blk, indent))
	}

	return sb.String()
}

func FormatBlock(b *Block, indent string) string {
	indent = indent + "  "
	var sb strings.Builder
	sb.WriteString(b.Label + ":\n")
	for _, instr := range b.Inst {
		sb.WriteString(indent)
		sb.WriteString(instr.String())
		sb.WriteString("\n")
	}
	return sb.String()
}
