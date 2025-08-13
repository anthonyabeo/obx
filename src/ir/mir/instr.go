package mir

import (
	"fmt"
	"strings"
)

type Instr interface {
	String() string
	Def() Value    // result variable (nil if none)
	Uses() []Value // operands this instruction uses
	ReplaceUses(map[string]Value)
	ReplaceDef(Value)
}

type (
	CmpInst struct {
		Target Value
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
		Target Value
		Addr   Value
	}

	StoreInst struct {
		Addr Value
		Val  Value
	}

	PhiInst struct {
		Target Value
		Args   []*PHIArg
	}

	PHIArg struct {
		Block *Block // block where this value comes from
		Value Value  // value from the block
	}
)

func (a *AssignInst) Def() Value     { return a.Target }
func (a *AssignInst) Uses() []Value  { return []Value{a.Value} }
func (a *AssignInst) String() string { return fmt.Sprintf("%v = %v", a.Target, a.Value) }
func (a *AssignInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[a.Value.BaseName()]; ok {
		a.Value = nv
	}
}
func (a *AssignInst) ReplaceDef(t Value) { a.Target = t }

func (*JumpInst) Def() Value                     { return nil }
func (*JumpInst) Uses() []Value                  { return nil }
func (j *JumpInst) String() string               { return fmt.Sprintf("jmp %s", j.Target) }
func (j *JumpInst) ReplaceUses(map[string]Value) {}
func (j *JumpInst) ReplaceDef(Value)             {}

func (b *CondBrInst) Def() Value    { return nil }
func (b *CondBrInst) Uses() []Value { return []Value{b.Cond} }
func (b *CondBrInst) String() string {
	return fmt.Sprintf("br %s, %s, %s", b.Cond.Name(), b.TrueLabel, b.FalseLabel)
}
func (b *CondBrInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[b.Cond.BaseName()]; ok {
		b.Cond = nv
	}
}
func (b *CondBrInst) ReplaceDef(Value) {}

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
func (r *ReturnInst) ReplaceUses(m map[string]Value) {
	if r.Result == nil {
		return
	}

	if nv, ok := m[r.Result.BaseName()]; ok {
		r.Result = nv
	}
}
func (r *ReturnInst) ReplaceDef(Value) {}

func (c *CmpInst) Def() Value    { return c.Target }
func (c *CmpInst) Uses() []Value { return []Value{c.Left, c.Right} }
func (c *CmpInst) String() string {
	return fmt.Sprintf("%s := icmp %s %s, %s", c.Target.Name(), c.Op, c.Left.Name(), c.Right.Name())
}
func (c *CmpInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[c.Left.BaseName()]; ok {
		c.Left = nv
	}

	if nv, ok := m[c.Right.BaseName()]; ok {
		c.Right = nv
	}
}
func (c *CmpInst) ReplaceDef(t Value) { c.Target = t }

func (b *BinaryInst) Def() Value    { return b.Target }
func (b *BinaryInst) Uses() []Value { return []Value{b.Left, b.Right} }
func (b *BinaryInst) String() string {
	return fmt.Sprintf("%s := %s %s, %s", b.Target.Name(), b.Op, b.Left.Name(), b.Right.Name())
}
func (b *BinaryInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[b.Left.BaseName()]; ok {
		b.Left = nv
	}

	if nv, ok := m[b.Right.BaseName()]; ok {
		b.Right = nv
	}
}
func (b *BinaryInst) ReplaceDef(t Value) { b.Target = t }

func (u *UnaryInst) Def() Value    { return u.Target }
func (u *UnaryInst) Uses() []Value { return []Value{u.Operand} }
func (u *UnaryInst) String() string {
	return fmt.Sprintf("%s := %s %s", u.Target.Name(), u.Op, u.Operand.Name())
}
func (u *UnaryInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[u.Operand.BaseName()]; ok {
		u.Operand = nv
	}
}
func (u *UnaryInst) ReplaceDef(t Value) { u.Target = t }

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
func (c *CallInst) ReplaceUses(m map[string]Value) {
	for i, arg := range c.Args {
		if nv, ok := m[arg.BaseName()]; ok {
			c.Args[i] = nv
		}
	}
}
func (c *CallInst) ReplaceDef(t Value) { c.Target = t }

func (l *LoadInst) Def() Value    { return l.Target }
func (l *LoadInst) Uses() []Value { return []Value{l.Addr} }
func (l *LoadInst) String() string {
	return fmt.Sprintf("%s := load %s", l.Target.Name(), l.Addr.Name())
}
func (l *LoadInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[l.Addr.BaseName()]; ok {
		l.Addr = nv
	}
}
func (l *LoadInst) ReplaceDef(t Value) { l.Target = t }

func (s *StoreInst) Def() Value    { return nil }
func (s *StoreInst) Uses() []Value { return []Value{s.Addr, s.Val} }
func (s *StoreInst) String() string {
	return fmt.Sprintf("store %s, %s", s.Addr.Name(), s.Val.Name())
}
func (s *StoreInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[s.Addr.BaseName()]; ok {
		s.Addr = nv
	}

	if nv, ok := m[s.Val.BaseName()]; ok {
		s.Val = nv
	}
}
func (s *StoreInst) ReplaceDef(Value) {}

func (phi *PhiInst) Def() Value { return phi.Target }
func (phi *PhiInst) Uses() []Value {
	var uses []Value
	for _, arg := range phi.Args {
		uses = append(uses, arg.Value)
	}
	return uses
}
func (phi *PhiInst) String() string {
	var argNames []string
	for _, arg := range phi.Args {
		argNames = append(argNames, fmt.Sprintf("[%s %s]", arg.Block.Label, arg.Value.Name()))
	}
	return fmt.Sprintf("%s = phi %s", phi.Target, strings.Join(argNames, ", "))
}
func (phi *PhiInst) ReplaceUses(m map[string]Value) {
	for _, arg := range phi.Args {
		if nv, ok := m[arg.Value.BaseName()]; ok {
			arg.Value = nv
		}
	}
}
func (phi *PhiInst) AddArg(block *Block, value Value) {
	phi.Args = append(phi.Args, &PHIArg{Block: block, Value: value})
}
func (phi *PhiInst) ReplaceDef(t Value) { phi.Target = t }
