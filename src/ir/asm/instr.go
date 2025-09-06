package asm

import (
	"fmt"
	"strings"
)

type Instr struct {
	Opcode   string
	Operands []Operand
}

func (i Instr) String() string {
	var operands []string
	for _, op := range i.Operands {
		operands = append(operands, op.String())
	}

	return fmt.Sprintf("%s %s", i.Opcode, strings.Join(operands, ", "))
}
