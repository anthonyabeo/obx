package meer

import (
	"bytes"
	"fmt"
)

type Opcode int

func (op Opcode) String() string {
	return opcodes[op]
}

var opcodes = [...]string{
	Lbl:    "lbl",
	Assign: "=",

	Add: "+",
	Sub: "-",
	Mul: "*",
	Div: "div",

	Call: "call",

	Eq:  "==",
	Ne:  "!=",
	Gt:  ">",
	Ge:  ">=",
	Lt:  "<",
	Le:  "<=",
	Not: "~",

	Br:  "br",
	Ret: "ret",
	Jmp: "jmp",
}

const (
	Invalid Opcode = iota

	Lbl
	Assign

	Add
	Sub
	Mul
	Div

	Call

	Eq
	Ne
	Gt
	Ge
	Lt
	Le
	Not

	Br
	Ret
	Jmp
)

// Program ...
// ------------------
type Program struct {
	Units map[string]*ProgramUnit
}

func NewProgram() *Program {
	return &Program{Units: map[string]*ProgramUnit{}}
}

func (p *Program) String() string {
	buf := new(bytes.Buffer)

	for _, pu := range p.Units {
		buf.WriteString(pu.String())
	}

	return buf.String()
}

// ProgramUnit ...
// ------------------
type ProgramUnit struct {
	Inst []Instruction
}

func NewProgramUnit(name string) *ProgramUnit {
	pu := new(ProgramUnit)
	pu.Inst = append(pu.Inst, NewLabel(name))

	return pu
}

func (p *ProgramUnit) String() string {
	buf := new(bytes.Buffer)

	buf.WriteString(fmt.Sprintf("module %s\n\t", p.Inst[0].String()))
	buf.WriteString("begin\n\t\t")
	for _, inst := range p.Inst[1:] {
		buf.WriteString(inst.String())
		buf.WriteString("\n\t\t")
	}
	buf.WriteString("\n\tend")

	return buf.String()
}
