package minir

import "fmt"

// Instr is the common interface for all instructions.
type Instr interface {
	String() string
	Uses() []Value
	Def() *Temp
}

// Terminator marks an instruction that ends a basic block.
type Terminator interface {
	Instr
	isTerminator()
}

// PhiArm pairs a predecessor block with a value.
type PhiArm struct {
	// use the predecessor block's label to avoid file-order/type resolution issues
	BlockLabel string
	Val        Value
}

// PhiInst selects a value based on predecessor.
type PhiInst struct {
	Dst  *Temp
	Args []PhiArm
}

func (p *PhiInst) String() string { return fmt.Sprintf("%s = phi", p.Dst.Name()) }
func (p *PhiInst) Uses() []Value {
	out := make([]Value, 0, len(p.Args))
	for _, a := range p.Args {
		out = append(out, a.Val)
	}
	return out
}
func (p *PhiInst) Def() *Temp { return p.Dst }

// BinaryInst represents a binary arithmetic operation.
type BinaryInst struct {
	Dst         *Temp
	Op          string
	Left, Right Value
}

func (b *BinaryInst) String() string {
	return fmt.Sprintf("%s = %s %s, %s", b.Dst.Name(), b.Op, b.Left.String(), b.Right.String())
}
func (b *BinaryInst) Uses() []Value { return []Value{b.Left, b.Right} }
func (b *BinaryInst) Def() *Temp    { return b.Dst }

// ICmpInst represents an integer comparison, produces a bool temp.
type ICmpInst struct {
	Dst         *Temp
	Pred        string
	Left, Right Value
}

func (c *ICmpInst) String() string {
	return fmt.Sprintf("%s = icmp.%s %s, %s", c.Dst.Name(), c.Pred, c.Left.String(), c.Right.String())
}
func (c *ICmpInst) Uses() []Value { return []Value{c.Left, c.Right} }
func (c *ICmpInst) Def() *Temp    { return c.Dst }

// FCmpInst for floating comparisons.
type FCmpInst struct{ ICmpInst }

// LoadInst reads from memory via an address.
type LoadInst struct {
	Dst  *Temp
	Addr *Temp
}

func (l *LoadInst) String() string { return fmt.Sprintf("%s = load %s", l.Dst.Name(), l.Addr.Name()) }
func (l *LoadInst) Uses() []Value  { return []Value{l.Addr} }
func (l *LoadInst) Def() *Temp     { return l.Dst }

// StoreInst writes a value to an address.
// Val is Value (not *Temp) so that constant rvalues can be stored directly
// without a separate materialization instruction.
type StoreInst struct {
	Val  Value // the value to store (Temp or Constant)
	Addr *Temp // must be IsAddr=true
}

func (s *StoreInst) String() string {
	return fmt.Sprintf("store %s, %s", s.Val.String(), s.Addr.Name())
}
func (s *StoreInst) Uses() []Value { return []Value{s.Val, s.Addr} }
func (s *StoreInst) Def() *Temp    { return nil }

// AllocaInst allocates stack storage and returns an address.
type AllocaInst struct {
	Dst       *Temp
	AllocType Type
}

func (a *AllocaInst) String() string {
	return fmt.Sprintf("%s = alloca %s", a.Dst.Name(), a.AllocType.String())
}
func (a *AllocaInst) Uses() []Value { return nil }
func (a *AllocaInst) Def() *Temp    { return a.Dst }

// GEPInst computes an address from a base and integer offsets.
type GEPInst struct {
	Dst      *Temp
	Base     Value
	ElemType Type
	Offsets  []int
}

func (g *GEPInst) String() string { return fmt.Sprintf("%s = gep %s", g.Dst.Name(), g.Base.String()) }
func (g *GEPInst) Uses() []Value  { return []Value{g.Base} }
func (g *GEPInst) Def() *Temp     { return g.Dst }

// CallInst represents a function call; optional result stored in Dst.
type CallInst struct {
	Dst    *Temp  // nil if void
	Callee string // for simplicity use name; could be Temp for indirect
	Args   []Value
}

func (c *CallInst) String() string {
	if c.Dst == nil {
		return fmt.Sprintf("call %s", c.Callee)
	}
	return fmt.Sprintf("%s = call %s", c.Dst.Name(), c.Callee)
}
func (c *CallInst) Uses() []Value {
	out := make([]Value, 0, len(c.Args))
	for _, a := range c.Args {
		out = append(out, a)
	}
	return out
}
func (c *CallInst) Def() *Temp { return c.Dst }

// UnaryInst applies a single-operand operation.
// Op values: "not" (bitwise flip), "neg" (integer negate), "fneg" (float negate).
type UnaryInst struct {
	Dst *Temp
	Op  string
	Src Value
}

func (u *UnaryInst) String() string {
	return fmt.Sprintf("%s = %s %s", u.Dst.Name(), u.Op, u.Src.String())
}
func (u *UnaryInst) Uses() []Value { return []Value{u.Src} }
func (u *UnaryInst) Def() *Temp    { return u.Dst }

// CastInst converts a value to a different type.
// Op values: "trunc", "zext", "sext", "bitcast", "sitofp", "fptosi", "fpext", "fptrunc".
type CastInst struct {
	Dst *Temp
	Op  string
	Src Value
}

func (c *CastInst) String() string {
	return fmt.Sprintf("%s = %s %s", c.Dst.Name(), c.Op, c.Src.String())
}
func (c *CastInst) Uses() []Value { return []Value{c.Src} }
func (c *CastInst) Def() *Temp    { return c.Dst }

// HaltInst terminates execution with an integer exit code.
// It is a terminator — no successor blocks.
type HaltInst struct {
	Code Value // exit code; may be nil (defaults to 0)
}

func (h *HaltInst) String() string {
	if h.Code == nil {
		return "halt"
	}
	return fmt.Sprintf("halt %s", h.Code.String())
}
func (h *HaltInst) Uses() []Value {
	if h.Code == nil {
		return nil
	}
	return []Value{h.Code}
}
func (h *HaltInst) Def() *Temp    { return nil }
func (h *HaltInst) isTerminator() {}

// ReturnInst returns from a function.
type ReturnInst struct {
	Result *Temp // nil for void
}

func (r *ReturnInst) String() string {
	if r.Result == nil {
		return "ret"
	}
	return fmt.Sprintf("ret %s", r.Result.Name())
}
func (r *ReturnInst) Uses() []Value {
	if r.Result == nil {
		return nil
	}
	return []Value{r.Result}
}
func (r *ReturnInst) Def() *Temp    { return nil }
func (r *ReturnInst) isTerminator() {}

// JumpInst is an unconditional jump terminator.
type JumpInst struct{ Target string }

func (j *JumpInst) String() string { return fmt.Sprintf("jmp %s", j.Target) }
func (j *JumpInst) Uses() []Value  { return nil }
func (j *JumpInst) Def() *Temp     { return nil }
func (j *JumpInst) isTerminator()  {}

// CondBrInst is a conditional branch terminator.
type CondBrInst struct {
	Cond                  *Temp
	TrueLabel, FalseLabel string
}

func (c *CondBrInst) String() string {
	return fmt.Sprintf("br %s, %s, %s", c.Cond.Name(), c.TrueLabel, c.FalseLabel)
}
func (c *CondBrInst) Uses() []Value { return []Value{c.Cond} }
func (c *CondBrInst) Def() *Temp    { return nil }
func (c *CondBrInst) isTerminator() {}

type SwitchArm struct {
	Val   int
	Label string
}

// SwitchInst is a simple switch terminator.
type SwitchInst struct {
	Key     *Temp
	Default string
	Arms    []SwitchArm
}

func (s *SwitchInst) String() string { return fmt.Sprintf("switch %s", s.Key.Name()) }
func (s *SwitchInst) Uses() []Value  { return []Value{s.Key} }
func (s *SwitchInst) Def() *Temp     { return nil }
func (s *SwitchInst) isTerminator()  {}
