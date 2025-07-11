package lir

import (
	"fmt"
	"io"
	"strings"
)

func FormatProgram(w io.Writer, prog *Program) error {
	for _, mod := range prog.Modules {
		if err := FormatModule(w, mod); err != nil {
			return err
		}
		fmt.Fprintln(w)
	}
	return nil
}

// FormatModule pretty-prints a LIR Module to the writer.
func FormatModule(w io.Writer, m *Module) error {
	fmt.Fprintf(w, "module %s {\n", m.Name)

	// Print global variables (if any)
	if len(m.Globals) > 0 {
		fmt.Fprintln(w, "  globals:")
		for _, g := range m.Globals {
			if g.Init != nil {
				fmt.Fprintf(w, "    VAR %s: %s = %s\n", g.Name, g.Type.String(), g.Init.String())
			} else {
				fmt.Fprintf(w, "    VAR %s: %s\n", g.Name, g.Type.String())
			}
		}
		fmt.Fprintln(w)
	}

	// Print all procedures
	for _, p := range m.Procs {
		fmt.Fprintln(w)
		if err := FormatProc(w, p); err != nil {
			return err
		}
	}

	fmt.Fprintln(w, "}")

	return nil
}

func FormatProc(w io.Writer, p *Procedure) error {
	exportPrefix := ""
	if p.IsExported {
		exportPrefix = "export "
	}

	// Format parameter list
	paramList := make([]string, len(p.Params))
	for i, param := range p.Params {
		paramList[i] = fmt.Sprintf("%s: %s", param.Name, param.Type)
	}

	ret := ""
	if p.Ret != nil {
		ret = fmt.Sprintf(" -> %s", p.Ret.String())
	}

	fmt.Fprintf(w, "%sproc %s(%s)%s\n", exportPrefix, p.Name, strings.Join(paramList, ", "), ret)

	// Format locals
	if len(p.Locals) > 0 {
		fmt.Fprintf(w, "  locals:")
		for _, local := range p.Locals {
			fmt.Fprintf(w, " %s: %s", local.Name, local.Type)
		}
		fmt.Fprintln(w)
	}

	// Format blocks
	for _, block := range p.Blocks {
		if err := FormatBlock(w, block); err != nil {
			return err
		}
	}

	return nil
}

func FormatBlock(w io.Writer, b *Block) error {
	fmt.Fprintf(w, "%s:\n", b.Name)
	for _, instr := range b.Instrs {
		fmt.Fprintf(w, "  %s\n", instr.String())
	}
	return nil
}
