package ir

import (
	"fmt"
	"strings"
)

type InstrKind int

const (
	Error InstrKind = iota

	Labeled
	CtrlFlow
	Normal
)

type Instruction interface {
	Opcode() Opcode
	Kind() InstrKind
	String() string
}
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
	dstOps []Operand
}

func CreateNormalInstr(op Opcode, src, dst []Operand) *NormalInstr {
	return &NormalInstr{op: op, srcOps: src, dstOps: dst}
}
func (n NormalInstr) Opcode() Opcode { return n.op }

func (n NormalInstr) Kind() InstrKind { return Normal }

func (n NormalInstr) String() string {
	var src, dst []string
	for _, op := range n.srcOps {
		src = append(src, op.String())
	}

	for _, op := range n.dstOps {
		dst = append(dst, op.String())
	}

	return fmt.Sprintf("%s %s => %s", n.op, strings.Join(src, ", "), strings.Join(dst, ", "))
}

// CtlFlowInstr used to change the flow of control in the program.
// ----------------------------------------------------------------
type CtlFlowInstr struct {
	op Opcode
}

func CreateCtlFlowInstr(op Opcode) *CtlFlowInstr { return &CtlFlowInstr{op: op} }

func (c CtlFlowInstr) Opcode() Opcode { return c.op }

func (c CtlFlowInstr) Kind() InstrKind { return CtrlFlow }

func (c CtlFlowInstr) String() string { panic("not implemented") }

// CallInstr ...
// --------------------
type CallInstr struct {
	Proc string
	Args []Operand
}

func (c CallInstr) Opcode() Opcode {
	return call
}

func (c CallInstr) Kind() InstrKind {
	return Normal
}

func (c CallInstr) String() string {
	var args []string
	for _, op := range c.Args {
		args = append(args, op.String())
	}

	return fmt.Sprintf("call %s(%s)", c.Proc, strings.Join(args, ", "))
}

// CmpInstr ...
// --------------------------
type CmpInstr struct {
	Cond   Opcode
	X, Y   Operand
	Result Operand
}

func (c CmpInstr) Opcode() Opcode { return c.Cond }

func (c CmpInstr) Kind() InstrKind { return Normal }

func (c CmpInstr) String() string {
	return fmt.Sprintf("cmp %s %s %s => %s", c.Cond, c.X, c.Y, c.Result)
}
