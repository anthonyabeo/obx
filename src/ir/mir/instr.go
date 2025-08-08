package mir

import (
	"fmt"
	"strings"
)

type Instr interface {
	inst()
	String() string
	Def() Value    // result variable (nil if none)
	Uses() []Value // operands this instruction uses
}

type (
	CmpInst struct {
		Target *Temp
		Op     InstrOp
		Left   Value
		Right  Value
	}

	AssignInst struct {
		Target Value
		Value  Value
	}

	JumpInst struct {
		Target string
	}

	CondBrInst struct {
		Cond       Value
		TrueLabel  string
		FalseLabel string
	}

	ReturnInst struct {
		Result Value // nil if procedure returns nothing
	}

	BinaryInst struct {
		Target Value
		Op     InstrOp
		Left   Value
		Right  Value
	}

	UnaryInst struct {
		Target  Value
		Op      InstrOp
		Operand Value
	}

	CallInst struct {
		Target Value  // optional: nil for void calls
		Callee string // function name
		Args   []Value
	}

	LoadInst struct {
		Target Temp
		Addr   Value
	}

	StoreInst struct {
		Addr Value
		Val  Value
	}

	//AddrOf struct {
	//	Target Temp
	//	Source string
	//}
)

func (*AssignInst) inst()            {}
func (a *AssignInst) Def() Value     { return a.Target }
func (a *AssignInst) Uses() []Value  { return []Value{a.Value} }
func (a *AssignInst) String() string { return fmt.Sprintf("%v = %v", a.Target, a.Value) }

func (*JumpInst) inst()            {}
func (*JumpInst) Def() Value       { return nil }
func (*JumpInst) Uses() []Value    { return nil }
func (j *JumpInst) String() string { return fmt.Sprintf("jmp %s", j.Target) }

func (*CondBrInst) inst()           {}
func (b *CondBrInst) Def() Value    { return nil }
func (b *CondBrInst) Uses() []Value { return []Value{b.Cond} }
func (b *CondBrInst) String() string {
	return fmt.Sprintf("br %s, %s, %s", b.Cond.Name(), b.TrueLabel, b.FalseLabel)
}

func (*ReturnInst) inst()      {}
func (*ReturnInst) Def() Value { return nil }
func (r *ReturnInst) Uses() []Value {
	if r.Result == nil {
		return nil
	}
	return []Value{r.Result}
}
func (r *ReturnInst) String() string {
	if r.Result != nil {
		return fmt.Sprintf("ret %v", r.Result.Name())
	}

	return "ret"
}

func (*CmpInst) inst()           {}
func (c *CmpInst) Def() Value    { return c.Target }
func (c *CmpInst) Uses() []Value { return []Value{c.Left, c.Right} }
func (c *CmpInst) String() string {
	return fmt.Sprintf("%s := %s %s, %s",
		c.Target.Name(), c.Op, c.Left.Name(), c.Right.Name())
}

func (b *BinaryInst) inst()         {}
func (b *BinaryInst) Def() Value    { return b.Target }
func (b *BinaryInst) Uses() []Value { return []Value{b.Left, b.Right} }
func (b *BinaryInst) String() string {
	return fmt.Sprintf("%s := %s %s, %s", b.Target.Name(), b.Op, b.Left.Name(), b.Right.Name())
}

func (u *UnaryInst) inst()         {}
func (u *UnaryInst) Def() Value    { return u.Target }
func (u *UnaryInst) Uses() []Value { return []Value{u.Operand} }
func (u *UnaryInst) String() string {
	return fmt.Sprintf("%s := %s %s", u.Target.Name(), u.Op, u.Operand.Name())
}

func (c *CallInst) inst()         {}
func (c *CallInst) Def() Value    { return c.Target }
func (c *CallInst) Uses() []Value { return c.Args }
func (c *CallInst) String() string {
	var argNames []string
	for _, arg := range c.Args {
		argNames = append(argNames, arg.Name())
	}
	callStr := fmt.Sprintf("call %s(%s)", c.Callee, strings.Join(argNames, ", "))
	if c.Target == nil {
		return callStr
	}
	return fmt.Sprintf("%s := %s", c.Target.Name(), callStr)
}

func (l *LoadInst) inst()         {}
func (l *LoadInst) Def() Value    { return l.Target }
func (l *LoadInst) Uses() []Value { return []Value{l.Addr} }
func (l *LoadInst) String() string {
	return fmt.Sprintf("%s := load %s", l.Target.Name(), l.Addr.Name())
}

func (s *StoreInst) inst()         {}
func (s *StoreInst) Def() Value    { return nil }
func (s *StoreInst) Uses() []Value { return []Value{s.Addr, s.Val} }
func (s *StoreInst) String() string {
	return fmt.Sprintf("store %s, %s", s.Addr.Name(), s.Val.Name())
}

//func (a *AddrOf) inst()         {}
//func (a *AddrOf) Def() Value    { return a.Target }
//func (a *AddrOf) Uses() []Value { return []Value{a.Source} }
//func (a *AddrOf) String() string {
//	return fmt.Sprintf("%s := addr %s", a.Target.Name(), a.Source.Name())
//}
