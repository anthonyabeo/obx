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

func (i Instr) UseIdx() []int {
	var uses []int
	for idx, op := range i.Operands {
		if reg, ok := op.(*Register); ok && reg.Mode == Virt {
			if i.Def != nil && reg.Name == i.Def.Name {
				continue
			}
			uses = append(uses, idx)
		}
	}
	return uses
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
	}

	return newInstr
}
