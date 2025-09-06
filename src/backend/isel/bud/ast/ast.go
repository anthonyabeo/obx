package ast

import "bytes"

type MachineDesc struct {
	Header *Header
	Rules  []*Rule
}

type Header struct {
	Fields map[string][]string
}

type Instr struct {
	Opcode   string
	Operands []Operand
}

// Pattern denotes Pattern tree (variables are leaves with Var != "")
type Pattern struct {
	Op   string
	Args []*Pattern
	Var  string // e.g. "$rs1", "$ofs" (only for variable leaves)
}

func (e Pattern) String() string {
	if len(e.Args) == 0 {
		return e.Op
	}
	var buf bytes.Buffer
	buf.WriteString(e.Op)
	buf.WriteString("(")
	for i, arg := range e.Args {
		if i > 0 {
			buf.WriteString(", ")
		}
		buf.WriteString(arg.String())
	}
	buf.WriteString(")")
	return buf.String()
}
