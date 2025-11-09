package asm

import (
	"fmt"
	"strings"
)

type Instr struct {
	Opcode      string
	DstOperand  Operand
	SrcOperands []Operand

	Def  *Register   // register defined by this instruction
	Uses []*Register // registers used by this instruction

	LiveIn  map[string]bool
	LiveOut map[string]bool
}

func (i Instr) Defs() []string {
	if i.Def == nil {
		return nil
	}

	return []string{i.Def.Name}
}

func (i Instr) String() string {
	var operands []string
	for _, op := range i.SrcOperands {
		operands = append(operands, op.String())
	}

	if i.DstOperand != nil {
		return fmt.Sprintf("%s %s, %s", i.Opcode, i.DstOperand, strings.Join(operands, ", "))
	}
	return fmt.Sprintf("%s %s", i.Opcode, strings.Join(operands, ", "))
}
