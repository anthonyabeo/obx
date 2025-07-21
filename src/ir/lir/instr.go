package lir

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/ir"
	"strings"
)

type (
	JmpInst struct {
		Dst *Label
	}

	CondBrInst struct {
		Cond    Operand
		IfTrue  *Label
		IfFalse *Label
	}

	RetInst struct {
		Value Operand
	}

	MovInst struct {
		Src Operand
		Dst Operand
	}

	LoadInst struct {
		Dst Operand
		Src *Addr
	}

	StoreInst struct {
		Src Operand
		Dst *Addr
	}

	AllocaInst struct {
		Size int
		Elem Type
	}

	BinaryInst struct {
		Op  ir.Operator // "add", "sub", "mul", "and", etc.
		Dst Operand     // result register or variable
		Lhs Operand
		Rhs Operand
	}

	UnaryInst struct {
		Op  ir.Operator // "neg", "not"
		Dst Operand
		Val Operand
	}

	CallInst struct {
		Func Operand // *Func or *Label
		Args []Operand
		Dst  Operand // result variable (can be nil)
	}

	PhiInst struct {
		Dst  Operand  // destination variable
		Arms []PhiArm // each arm has a label and value
	}

	CmpInst struct {
		Dst  Operand
		Op   ir.Operator
		X, Y Operand
	}
)

func (*CondBrInst) isInstr() {}
func (*JmpInst) isInstr()    {}
func (*RetInst) isInstr()    {}
func (*MovInst) isInstr()    {}
func (*LoadInst) isInstr()   {}
func (*StoreInst) isInstr()  {}
func (*AllocaInst) isInstr() {}
func (*BinaryInst) isInstr() {}
func (*UnaryInst) isInstr()  {}
func (*CallInst) isInstr()   {}
func (*PhiInst) isInstr()    {}
func (*CmpInst) isInstr()    {}

func (j *JmpInst) String() string { return fmt.Sprintf("jmp %s", j.Dst) }
func (c *CondBrInst) String() string {
	return fmt.Sprintf("br %s, label %s, label %s", c.Cond, c.IfTrue, c.IfFalse)
}
func (r *RetInst) String() string {
	out := "ret"
	if r.Value != nil {
		out += " " + r.Value.String()
	}

	return out
}
func (m *MovInst) String() string    { return fmt.Sprintf("mov %s %s", m.Dst, m.Src) }
func (l *LoadInst) String() string   { return fmt.Sprintf("load %s %s", l.Dst, l.Src) }
func (l *StoreInst) String() string  { return fmt.Sprintf("store %s %s", l.Dst, l.Src) }
func (a *AllocaInst) String() string { return fmt.Sprintf("alloca %s %d", a.Elem, a.Size) }
func (b *BinaryInst) String() string { return fmt.Sprintf("%s %s, %s, %s", b.Op, b.Dst, b.Lhs, b.Rhs) }
func (u *UnaryInst) String() string  { return fmt.Sprintf("%s %s, %s", u.Op, u.Dst, u.Val) }
func (c *CallInst) String() string {
	args := make([]string, len(c.Args))
	for i, a := range c.Args {
		args[i] = a.String()
	}
	if c.Dst != nil {
		return fmt.Sprintf("%s = call %s(%s)", c.Dst, c.Func, strings.Join(args, ", "))
	}
	return fmt.Sprintf("call %s(%s)", c.Func, strings.Join(args, ", "))
}
func (p *PhiInst) String() string {
	parts := make([]string, len(p.Arms))
	for i, a := range p.Arms {
		parts[i] = a.String()
	}
	return fmt.Sprintf("%s = phi %s", p.Dst, strings.Join(parts, ", "))
}
func (c *CmpInst) String() string {
	return fmt.Sprintf("%s = cmp.%s %s, %s", c.Dst, c.Op, c.X, c.Y)
}
