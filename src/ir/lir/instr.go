package lir

import (
	"fmt"
	"strings"
)

type (
	JmpInstr struct {
		Dst *Label
	}

	CondBrInstr struct {
		Cond    Operand
		IfTrue  *Label
		IfFalse *Label
	}

	RetInstr struct {
		Value Operand
	}

	MovInstr struct {
		Src Operand
		Dst Operand
	}

	LoadInstr struct {
		Dst Operand
		Src *Addr
	}

	StoreInstr struct {
		Src Operand
		Dst *Addr
	}

	AllocaInstr struct {
		Size int
		Elem Type
	}

	BinaryInstr struct {
		Op  string  // "add", "sub", "mul", "and", etc.
		Dst Operand // result register or variable
		Lhs Operand
		Rhs Operand
	}

	UnaryOpInstr struct {
		Op  string // "neg", "not"
		Dst Operand
		Val Operand
	}

	CallInstr struct {
		Func Operand // *Func or *Label
		Args []Operand
		Dst  Operand // result variable (can be nil)
	}

	PhiInstr struct {
		Dst  Operand  // destination variable
		Arms []PhiArm // each arm has a label and value
	}

	CmpInstr struct {
		Dst  Operand
		Op   Op
		X, Y Operand
	}
)

func (*CondBrInstr) isInstr()  {}
func (*JmpInstr) isInstr()     {}
func (*RetInstr) isInstr()     {}
func (*MovInstr) isInstr()     {}
func (*LoadInstr) isInstr()    {}
func (*StoreInstr) isInstr()   {}
func (*AllocaInstr) isInstr()  {}
func (*BinaryInstr) isInstr()  {}
func (*UnaryOpInstr) isInstr() {}
func (*CallInstr) isInstr()    {}
func (*PhiInstr) isInstr()     {}
func (*CmpInstr) isInstr()     {}

func (j *JmpInstr) String() string { return fmt.Sprintf("jmp %s", j.Dst) }
func (c *CondBrInstr) String() string {
	return fmt.Sprintf("br %s, label %s, label %s", c.Cond, c.IfTrue, c.IfFalse)
}
func (r *RetInstr) String() string {
	out := "ret"
	if r.Value != nil {
		out += " " + r.Value.String()
	}

	return out
}
func (m *MovInstr) String() string     { return fmt.Sprintf("mov %s %s", m.Dst, m.Src) }
func (l *LoadInstr) String() string    { return fmt.Sprintf("load %s %s", l.Dst, l.Src) }
func (l *StoreInstr) String() string   { return fmt.Sprintf("store %s %s", l.Dst, l.Src) }
func (a *AllocaInstr) String() string  { return fmt.Sprintf("alloca %s %d", a.Elem, a.Size) }
func (b *BinaryInstr) String() string  { return fmt.Sprintf("%s %s, %s, %s", b.Op, b.Dst, b.Lhs, b.Rhs) }
func (u *UnaryOpInstr) String() string { return fmt.Sprintf("%s %s, %s", u.Op, u.Dst, u.Val) }
func (c *CallInstr) String() string {
	args := make([]string, len(c.Args))
	for i, a := range c.Args {
		args[i] = a.String()
	}
	if c.Dst != nil {
		return fmt.Sprintf("%s = call %s(%s)", c.Dst, c.Func, strings.Join(args, ", "))
	}
	return fmt.Sprintf("call %s(%s)", c.Func, strings.Join(args, ", "))
}
func (p *PhiInstr) String() string {
	parts := make([]string, len(p.Arms))
	for i, a := range p.Arms {
		parts[i] = a.String()
	}
	return fmt.Sprintf("%s = phi %s", p.Dst, strings.Join(parts, ", "))
}
func (c *CmpInstr) String() string {
	return fmt.Sprintf("%s = cmp %s, %s %s", c.Dst, c.Op, c.X, c.Y)
}
