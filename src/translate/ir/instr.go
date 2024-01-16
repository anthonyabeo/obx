package ir

type LabeledInstr struct {
	label string
	instr []Instruction
}

func CreateLabeledInstr(label string, kind InstrKind) *LabeledInstr {
	return &LabeledInstr{label: label}
}

func (l LabeledInstr) Opcode() Opcode {
	if len(l.instr) > 0 {
		return l.instr[0].Opcode()
	}

	return Invalid
}

func (l LabeledInstr) Kind() InstrKind { return Labeled }

func (l LabeledInstr) String() string { panic("not implemented") }

// NormalInstr performs computation or data movement.
// They consist of an opcode, a list of comma-separated
// source operands, and a list of comma-separated result operands.
// ---------------------------------------------------------------
type NormalInstr struct {
	op     Opcode
	srcOps []Operand
	resOps []Operand
}

func CreateNormalInstr(op Opcode) *NormalInstr {
	return &NormalInstr{op: op}
}
func (n NormalInstr) Opcode() Opcode { return n.op }

func (n NormalInstr) Kind() InstrKind { return Normal }

func (n NormalInstr) String() string { panic("not implement") }

// CtlFlowInstr used to change the flow of control in the program.
// ----------------------------------------------------------------
type CtlFlowInstr struct {
	op Opcode
}

func CreateCtlFlowInstr(op Opcode) *CtlFlowInstr { return &CtlFlowInstr{op: op} }

func (c CtlFlowInstr) Opcode() Opcode { return c.op }

func (c CtlFlowInstr) Kind() InstrKind { return CtrlFlow }

func (c CtlFlowInstr) String() string { panic("not implemented") }
