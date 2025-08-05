package lir

import (
	"fmt"
	"strings"
)

type (
	AllocInst struct {
		StartAddr Operand      // The address where the memory is allocated
		Size      IntegerConst // The size of the memory to allocate in bytes
		OpCode    InstrOpCode
	}

	// JmpInst denoted an unconditional jump to a label.
	JmpInst struct {
		OpCode InstrOpCode
		Dst    *Label
	}

	// CondBrInst represents a conditional branch instruction.
	// It branches to one of two labels based on the evaluation of a condition.
	// If the condition is true, it jumps to IfTrue; otherwise, it jumps to IfFalse.
	CondBrInst struct {
		OpCode  InstrOpCode
		Cond    Operand
		IfTrue  *Label
		IfFalse *Label
	}

	// RetInst represents a return instruction. It can optionally return a value.
	RetInst struct {
		OpCode InstrOpCode
		Value  Operand
	}

	// MoveInst represents a move instruction that copies a value from one operand to another.
	MoveInst struct {
		OpCode InstrOpCode
		Src    Operand
		Dst    Operand
	}

	// LoadInst represents a load instruction that reads a value from memory into a register or temporary.
	LoadInst struct {
		OpCode InstrOpCode
		Dst    Operand
		Src    *Mem
	}

	// StoreInst represents a store instruction that writes a value from a register or temporary to memory.
	StoreInst struct {
		OpCode InstrOpCode
		Src    Operand
		Dst    Operand
	}

	// HaltInst represents a halt instruction, which stops the execution of the program.
	HaltInst struct {
		OpCode InstrOpCode
		Code   IntegerConst // An optional code to return on halt
	}

	// BinaryInst represents a binary operation (e.g., addition, subtraction). This operation
	// is for arithmetic or logical operations between two operands.
	BinaryInst struct {
		Op   InstrOpCode // e.g., "add", "sub", etc.
		Dst  Operand     // result of the operation
		Lhs  Operand     // Left-hand side operand
		Rhs  Operand     // Right-hand side operand
		Type Type        // Type of the result
	}

	CallInst struct {
		Op   InstrOpCode // e.g., "call"
		Func string      // Function to call
		Args []Operand   // Arguments to the function
		Type Type        // Return type of the function
	}
)

func (*CondBrInst) isInstr() {}
func (*JmpInst) isInstr()    {}
func (*RetInst) isInstr()    {}
func (*MoveInst) isInstr()   {}
func (*LoadInst) isInstr()   {}
func (*StoreInst) isInstr()  {}
func (*Label) isInstr()      {}
func (*AllocInst) isInstr()  {}
func (*HaltInst) isInstr()   {}
func (*BinaryInst) isInstr() {}

func (i *JmpInst) String() string { return fmt.Sprintf("jmp %s", i.Dst) }
func (i *CondBrInst) String() string {
	return fmt.Sprintf("br %s, label %s, label %s", i.Cond, i.IfTrue, i.IfFalse)
}
func (i *RetInst) String() string {
	out := "ret"
	if i.Value != nil {
		out += " " + i.Value.String()
	}

	return out
}
func (i *MoveInst) String() string  { return fmt.Sprintf("mov %s %s", i.Dst, i.Src) }
func (i *LoadInst) String() string  { return fmt.Sprintf("load %s %s", i.Dst, i.Src) }
func (i *StoreInst) String() string { return fmt.Sprintf("store %s %s", i.Dst, i.Src) }
func (i *Label) String() string {
	name := i.Name
	if i.Kind == "INST" {
		name += ":"
	}
	return name
}
func (i *AllocInst) String() string { return fmt.Sprintf("alloc %s, %s", i.StartAddr, i.Size) }
func (i *HaltInst) String() string {
	if i.Code.Value != 0 {
		return fmt.Sprintf("halt %s", i.Code)
	}
	return "halt"
}
func (i *BinaryInst) String() string {
	return fmt.Sprintf("%s %s %s", i.Lhs.String(), i.Op, i.Rhs)
}
func (i *CallInst) String() string {
	var args []string
	for _, arg := range i.Args {
		args = append(args, arg.String())
	}
	return fmt.Sprintf("%s @%s(%s)", i.Op, i.Func, strings.Join(args, ", "))
}

func (i *CondBrInst) Children() []Node { return []Node{i.Cond, i.IfTrue, i.IfFalse} }
func (i *JmpInst) Children() []Node    { return []Node{i.Dst} }
func (i *RetInst) Children() []Node {
	if i.Value != nil {
		return []Node{i.Value}
	}
	return []Node{}
}
func (i *MoveInst) Children() []Node  { return []Node{i.Src, i.Dst} }
func (i *LoadInst) Children() []Node  { return []Node{i.Src, i.Dst} }
func (i *StoreInst) Children() []Node { return []Node{i.Src, i.Dst} }

// func (i *LabelInst) Children() []Node { return []Node{i.Label} }
func (i *AllocInst) Children() []Node { return []Node{i.StartAddr, i.Size} }
func (i *HaltInst) Children() []Node {
	if i.Code.Value != 0 {
		return []Node{i.Code}
	}
	return []Node{}
}
func (i *BinaryInst) Children() []Node { return []Node{i.Lhs, i.Rhs} }
func (i *CallInst) Children() []Node {
	children := make([]Node, len(i.Args))
	for i, arg := range i.Args {
		children[i+1] = arg
	}
	return children
}
