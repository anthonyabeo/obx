package mir

import "fmt"

// Instr is a backend MIR instruction with explicit defs/uses.
type Instr interface {
	Defs() []*Register
	Uses() []Operand
	String() string
}

// Terminator marks a control-flow-ending instruction.
type Terminator interface {
	Instr
	isTerminator()
}

func defs1(dst *Register) []*Register {
	if dst == nil {
		return nil
	}
	return []*Register{dst}
}

func useList(ops ...Operand) []Operand {
	out := make([]Operand, 0, len(ops))
	for _, op := range ops {
		if op != nil {
			out = append(out, op)
		}
	}
	return out
}

// MoveInstr copies Src into Dst.
type MoveInstr struct {
	Dst *Register
	Src Operand
}

func (i *MoveInstr) Defs() []*Register { return defs1(i.Dst) }
func (i *MoveInstr) Uses() []Operand   { return useList(i.Src) }
func (i *MoveInstr) String() string    { return fmt.Sprintf("mov %s, %s", i.Dst, i.Src) }

// LoadInstr loads from Addr into Dst.
type LoadInstr struct {
	Dst  *Register
	Addr Operand
}

func (i *LoadInstr) Defs() []*Register { return defs1(i.Dst) }
func (i *LoadInstr) Uses() []Operand   { return useList(i.Addr) }
func (i *LoadInstr) String() string    { return fmt.Sprintf("load %s <- %s", i.Dst, i.Addr) }

// StoreInstr stores Value into Addr.
type StoreInstr struct {
	Addr  Operand
	Value Operand
}

func (i *StoreInstr) Defs() []*Register { return nil }
func (i *StoreInstr) Uses() []Operand   { return useList(i.Addr, i.Value) }
func (i *StoreInstr) String() string    { return fmt.Sprintf("store %s -> %s", i.Value, i.Addr) }

// UnaryInstr applies Op to X and writes the result to Dst.
type UnaryInstr struct {
	Dst *Register
	Op  string
	X   Operand
}

func (i *UnaryInstr) Defs() []*Register { return defs1(i.Dst) }
func (i *UnaryInstr) Uses() []Operand   { return useList(i.X) }
func (i *UnaryInstr) String() string    { return fmt.Sprintf("%s %s, %s", i.Op, i.Dst, i.X) }

// BinaryInstr applies Op to Left and Right and writes the result to Dst.
type BinaryInstr struct {
	Dst         *Register
	Op          string
	Left, Right Operand
}

func (i *BinaryInstr) Defs() []*Register { return defs1(i.Dst) }
func (i *BinaryInstr) Uses() []Operand   { return useList(i.Left, i.Right) }
func (i *BinaryInstr) String() string {
	return fmt.Sprintf("%s %s, %s, %s", i.Op, i.Dst, i.Left, i.Right)
}

// CompareInstr compares Left and Right using Pred and writes the boolean result
// to Dst.
type CompareInstr struct {
	Dst         *Register
	Pred        string
	Left, Right Operand
}

func (i *CompareInstr) Defs() []*Register { return defs1(i.Dst) }
func (i *CompareInstr) Uses() []Operand   { return useList(i.Left, i.Right) }
func (i *CompareInstr) String() string {
	return fmt.Sprintf("cmp.%s %s, %s, %s", i.Pred, i.Dst, i.Left, i.Right)
}

// PhiArm selects a value from a predecessor block.
type PhiArm struct {
	BlockLabel string
	Value      Operand
}

// PhiInstr merges values from predecessor blocks.
type PhiInstr struct {
	Dst  *Register
	Arms []PhiArm
}

func (i *PhiInstr) Defs() []*Register { return defs1(i.Dst) }
func (i *PhiInstr) Uses() []Operand {
	out := make([]Operand, 0, len(i.Arms))
	for _, arm := range i.Arms {
		if arm.Value != nil {
			out = append(out, arm.Value)
		}
	}
	return out
}
func (i *PhiInstr) String() string { return fmt.Sprintf("phi %s", i.Dst) }

// CallInstr invokes Callee with Args. Dst is nil for procedures.
type CallInstr struct {
	Dst    *Register
	Callee Operand
	Args   []Operand
}

func (i *CallInstr) Defs() []*Register { return defs1(i.Dst) }
func (i *CallInstr) Uses() []Operand {
	out := useList(i.Callee)
	out = append(out, i.Args...)
	return out
}
func (i *CallInstr) String() string { return fmt.Sprintf("call %s", i.Callee) }

// JumpInstr transfers control to Target.
type JumpInstr struct {
	Target string
}

func (*JumpInstr) isTerminator()     {}
func (*JumpInstr) Defs() []*Register { return nil }
func (*JumpInstr) Uses() []Operand   { return nil }
func (i *JumpInstr) String() string  { return fmt.Sprintf("jmp %s", i.Target) }

// CondBrInstr branches on Cond.
type CondBrInstr struct {
	Cond       Operand
	TrueLabel  string
	FalseLabel string
}

func (*CondBrInstr) isTerminator()     {}
func (*CondBrInstr) Defs() []*Register { return nil }
func (i *CondBrInstr) Uses() []Operand { return useList(i.Cond) }
func (i *CondBrInstr) String() string {
	return fmt.Sprintf("br %s ? %s : %s", i.Cond, i.TrueLabel, i.FalseLabel)
}

// ReturnInstr returns an optional Value.
type ReturnInstr struct {
	Value Operand
}

func (*ReturnInstr) isTerminator()     {}
func (*ReturnInstr) Defs() []*Register { return nil }
func (i *ReturnInstr) Uses() []Operand { return useList(i.Value) }
func (i *ReturnInstr) String() string  { return fmt.Sprintf("ret %s", i.Value) }

// HaltInstr terminates execution with a status code.
type HaltInstr struct {
	Code Operand
}

func (*HaltInstr) isTerminator()     {}
func (*HaltInstr) Defs() []*Register { return nil }
func (i *HaltInstr) Uses() []Operand { return useList(i.Code) }
func (i *HaltInstr) String() string  { return fmt.Sprintf("halt %s", i.Code) }

// SwitchArm associates a comparison value with a target label.
type SwitchArm struct {
	Value Operand
	Label string
}

// SwitchInstr branches to one of several labels or Default.
type SwitchInstr struct {
	Value   Operand
	Arms    []SwitchArm
	Default string
}

func (*SwitchInstr) isTerminator()     {}
func (*SwitchInstr) Defs() []*Register { return nil }
func (i *SwitchInstr) Uses() []Operand {
	out := useList(i.Value)
	for _, arm := range i.Arms {
		if arm.Value != nil {
			out = append(out, arm.Value)
		}
	}
	return out
}
func (i *SwitchInstr) String() string { return fmt.Sprintf("switch %s", i.Value) }
