package lir

import (
	"fmt"
	"io"
	"strings"
)

func FormatProgram(w io.Writer, prog *Program) error {
	for _, proc := range prog.Procs {
		if err := FormatProc(w, proc); err != nil {
			return err
		}
		fmt.Fprintln(w)
	}
	return nil
}

func FormatProc(w io.Writer, p *Proc) error {
	exportPrefix := ""
	if p.IsExported {
		exportPrefix = "export "
	}

	// Format parameter list
	paramList := make([]string, len(p.Params))
	for i, param := range p.Params {
		paramList[i] = fmt.Sprintf("%s: %s", param.Name, param.Type)
	}

	fmt.Fprintf(w, "%sproc %s(%s)\n", exportPrefix, p.Name, strings.Join(paramList, ", "))

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
	fmt.Fprintf(w, "%s:\n", b.Label)
	for _, instr := range b.Instrs {
		fmt.Fprintf(w, "  %s\n", instr.String())
	}
	return nil
}
