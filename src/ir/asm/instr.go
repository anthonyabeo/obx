package asm

import (
	"fmt"
	"strings"
)

type Instr struct {
	Opcode   string
	Operands []Operand

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
	for _, op := range i.Operands {
		operands = append(operands, op.String())
	}

	return fmt.Sprintf("%s %s", i.Opcode, strings.Join(operands, ", "))
}

func (i Instr) Copy() *Instr {
	newInstr := &Instr{
		Opcode:   i.Opcode,
		Operands: make([]Operand, len(i.Operands)),
		Def:      i.Def,
		Uses:     make([]*Register, len(i.Uses)),
		LiveIn:   make(map[string]bool),
		LiveOut:  make(map[string]bool),
	}

	copy(newInstr.Operands, i.Operands)
	copy(newInstr.Uses, i.Uses)

	for k, v := range i.LiveIn {
		newInstr.LiveIn[k] = v
	}
	for k, v := range i.LiveOut {
		newInstr.LiveOut[k] = v
	}

	return newInstr
}
