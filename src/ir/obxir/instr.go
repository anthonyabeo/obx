package obxir

import (
	"fmt"
	"strings"
)

// Instr is implemented by every three-address-code instruction.
type Instr interface {
	String() string
	Def() Value    // result variable (nil if none)
	Uses() []Value // operands this instruction uses
	ReplaceUses(map[string]Value)
	ReplaceDef(Value)
}

// Terminator is a marker interface satisfied by every instruction that
// ends a basic block.  A block's terminator is always its last instruction.
// Callers can gate terminator-specific logic on this interface rather than
// exhaustively type-switching on all Instr types.
type Terminator interface {
	Instr
	isTerminator()
}

// Foldable is an Instr that can be constant-folded.
type Foldable interface {
	Instr
	CanFold() bool
	Fold() Value
}

// ─── Instruction structs ──────────────────────────────────────────────────

type (
	HaltInst struct {
		Code Value
	}

	ICmpInst struct {
		Target Value
		Op     InstrOp
		Left   Value
		Right  Value
	}

	FCmpInst struct {
		Target Value
		Op     InstrOp
		Left   Value
		Right  Value
	}

	MoveInst struct {
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
		Result Value // nil if void
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
		Target Value // nil for void calls
		Callee Value // callee value (direct function symbol or pointer)
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

	AddrOf struct {
		Target Value
		Addr   Value
	}

	PhiInst struct {
		Target Value
		Args   []*PHIArg
	}

	PHIArg struct {
		Block *Block
		Value Value
	}

	// ─── GEPInst ──────────────────────────────────────────────────────────
	//
	// GEPInst (Get Element Pointer) computes the address of a field within a
	// record or an element within an array without emitting any explicit ADD
	// or MUL temporaries.  Each GEPIndex step is either a named record field
	// (Field != "") or an integer index into an array/pointer (Index != nil).
	//
	// The Target temp has IsAddr=true so that emitAssign correctly emits
	// StoreInst when the GEP result is used as a store destination.
	//
	// Example (record field access):
	//   t3 := GEP base {rec.x}         ; address of field x in record at base
	//
	// Example (array element access):
	//   t5 := GEP arr [i]              ; address of arr[i]
	//
	// Example (chained):
	//   t7 := GEP rec [i].name         ; address of rec[i].name

	GEPInst struct {
		Target   *Temp      // always has IsAddr=true
		Base     Value      // base pointer or addressable value
		ElemType Type       // element/record type being indexed into
		Indices  []GEPIndex // one or more address steps
	}

	// ─── CastInst ─────────────────────────────────────────────────────────
	//
	// CastInst performs an explicit type conversion.  The Op field encodes
	// the flavour (truncation, extension, float↔int, bitcast, ptr↔int).
	// Using a dedicated instruction — rather than overloading MoveInst —
	// gives the isel a single pattern per conversion kind and enables the
	// optimizer to reason about value ranges after zero/sign extensions.

	CastInst struct {
		Target  Value
		Op      CastOp
		Operand Value
		ToType  Type // result type (also carried by Target.Type(), but explicit here)
	}

	// ─── SwitchInst ───────────────────────────────────────────────────────
	//
	// SwitchInst replaces the chain-of-CondBrInst that CaseStmt used to
	// produce.  Each arm covers an inclusive integer range [Lo, Hi]; when
	// Lo == Hi it is a single-value match.  The isel can lower this to a
	// jump table for dense domains or a comparison chain for sparse ones.
	//
	// If Selector falls into none of the arms, control jumps to Default.

	SwitchInst struct {
		Selector Value
		Default  string
		Arms     []SwitchArm
	}
)

// GEPIndex is one step in a GEP address chain.
// Exactly one of Field or Index is non-zero/non-nil per GEPIndex:
//   - Field != ""  → descend into that named field of the current record type.
//   - Index != nil → step by Index elements into the current array/pointer type.
type GEPIndex struct {
	Field string // non-empty → record field step
	Index Value  // non-nil  → array/pointer element step
}

// SwitchArm is one branch of a SwitchInst.
// The arm matches when Lo ≤ Selector ≤ Hi (inclusive).
// When Hi is nil, the arm is a single-value match (Hi == Lo).
type SwitchArm struct {
	Lo    Value  // lower bound (inclusive)
	Hi    Value  // upper bound (inclusive); nil means same as Lo
	Label string // target block label
}

// ─── Terminator marker implementations ───────────────────────────────────

func (*JumpInst) isTerminator()   {}
func (*CondBrInst) isTerminator() {}
func (*ReturnInst) isTerminator() {}
func (*HaltInst) isTerminator()   {}
func (*SwitchInst) isTerminator() {}

// ─── HaltInst ─────────────────────────────────────────────────────────────

func (h *HaltInst) String() string { return fmt.Sprintf("HALT(%s)", h.Code) }
func (h *HaltInst) Def() Value     { return nil }
func (h *HaltInst) Uses() []Value  { return []Value{h.Code} }
func (h *HaltInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[h.Code.BaseName()]; ok {
		h.Code = nv
	}
}
func (h *HaltInst) ReplaceDef(Value) {}

// ─── MoveInst ─────────────────────────────────────────────────────────────

func (a *MoveInst) Def() Value     { return a.Target }
func (a *MoveInst) Uses() []Value  { return []Value{a.Value} }
func (a *MoveInst) String() string { return fmt.Sprintf("MOV %v, %v", a.Target, a.Value) }
func (a *MoveInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[a.Value.BaseName()]; ok {
		a.Value = nv
	}
}
func (a *MoveInst) ReplaceDef(t Value) { a.Target = t }

// ─── JumpInst ─────────────────────────────────────────────────────────────

func (*JumpInst) Def() Value                     { return nil }
func (*JumpInst) Uses() []Value                  { return nil }
func (j *JumpInst) String() string               { return fmt.Sprintf("JMP %s", j.Target) }
func (j *JumpInst) ReplaceUses(map[string]Value) {}
func (j *JumpInst) ReplaceDef(Value)             {}

// ─── CondBrInst ───────────────────────────────────────────────────────────

func (b *CondBrInst) Def() Value    { return nil }
func (b *CondBrInst) Uses() []Value { return []Value{b.Cond} }
func (b *CondBrInst) String() string {
	return fmt.Sprintf("BR %s, %s, %s", b.Cond.Name(), b.TrueLabel, b.FalseLabel)
}
func (b *CondBrInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[b.Cond.BaseName()]; ok {
		b.Cond = nv
	}
}
func (b *CondBrInst) ReplaceDef(Value) {}

// ─── ReturnInst ───────────────────────────────────────────────────────────

func (*ReturnInst) Def() Value { return nil }
func (r *ReturnInst) Uses() []Value {
	if r.Result == nil {
		return nil
	}
	return []Value{r.Result}
}
func (r *ReturnInst) String() string {
	if r.Result != nil {
		return fmt.Sprintf("RET %v", r.Result.Name())
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

// ─── ICmpInst ─────────────────────────────────────────────────────────────

func (c *ICmpInst) Def() Value    { return c.Target }
func (c *ICmpInst) Uses() []Value { return []Value{c.Left, c.Right} }
func (c *ICmpInst) String() string {
	return fmt.Sprintf("%s := ICMP.%s %s, %s", c.Target.Name(), c.Op, c.Left.Name(), c.Right.Name())
}
func (c *ICmpInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[c.Left.BaseName()]; ok {
		c.Left = nv
	}
	if nv, ok := m[c.Right.BaseName()]; ok {
		c.Right = nv
	}
}
func (c *ICmpInst) ReplaceDef(t Value) { c.Target = t }

// ─── FCmpInst ─────────────────────────────────────────────────────────────

func (c *FCmpInst) Def() Value    { return c.Target }
func (c *FCmpInst) Uses() []Value { return []Value{c.Left, c.Right} }
func (c *FCmpInst) String() string {
	return fmt.Sprintf("%s := FCMP.%s %s, %s", c.Target.Name(), c.Op, c.Left.Name(), c.Right.Name())
}
func (c *FCmpInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[c.Left.BaseName()]; ok {
		c.Left = nv
	}
	if nv, ok := m[c.Right.BaseName()]; ok {
		c.Right = nv
	}
}
func (c *FCmpInst) ReplaceDef(t Value) { c.Target = t }

// ─── BinaryInst ───────────────────────────────────────────────────────────

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

func (b *BinaryInst) CanFold() bool {
	_, okL := b.Left.(Constant)
	_, okR := b.Right.(Constant)
	return okL && okR
}
func (b *BinaryInst) Fold() Value {
	switch b.Op {
	case ADD:
		lc, okL := b.Left.(*IntegerLit)
		rc, okR := b.Right.(*IntegerLit)
		if okL && okR {
			return &IntegerLit{LitValue: lc.LitValue + rc.LitValue, Signed: lc.Signed, Bits: lc.Bits, Typ: lc.Typ}
		}
		sl, okL := b.Left.(*StrLit)
		sr, okR := b.Right.(*StrLit)
		if okL && okR {
			return &StrLit{LitValue: sl.LitValue + sr.LitValue, Typ: sr.Typ}
		}
	case SUB:
		lc, okL := b.Left.(*IntegerLit)
		rc, okR := b.Right.(*IntegerLit)
		if okL && okR {
			return &IntegerLit{LitValue: lc.LitValue - rc.LitValue, Signed: lc.Signed, Bits: lc.Bits, Typ: lc.Typ}
		}
	case MUL:
		lc, okL := b.Left.(*IntegerLit)
		rc, okR := b.Right.(*IntegerLit)
		if okL && okR {
			return &IntegerLit{LitValue: lc.LitValue * rc.LitValue, Signed: lc.Signed, Bits: lc.Bits, Typ: lc.Typ}
		}
	case IDIV:
		lc, okL := b.Left.(*IntegerLit)
		rc, okR := b.Right.(*IntegerLit)
		if okL && okR && rc.LitValue != 0 {
			return &IntegerLit{LitValue: lc.LitValue / rc.LitValue, Signed: lc.Signed, Bits: lc.Bits, Typ: lc.Typ}
		}
	case REM:
		lc, okL := b.Left.(*IntegerLit)
		rc, okR := b.Right.(*IntegerLit)
		if okL && okR && rc.LitValue != 0 {
			return &IntegerLit{LitValue: lc.LitValue % rc.LitValue, Signed: lc.Signed, Bits: lc.Bits, Typ: lc.Typ}
		}
	}
	return nil
}

// ─── UnaryInst ────────────────────────────────────────────────────────────

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

func (u *UnaryInst) CanFold() bool {
	_, isConst := u.Operand.(Constant)
	return isConst && (u.Op == NEG || u.Op == NOT || u.Op == FNEG)
}
func (u *UnaryInst) Fold() Value {
	switch u.Op {
	case NEG:
		if lit, ok := u.Operand.(*IntegerLit); ok {
			return &IntegerLit{LitValue: uint64(-int64(lit.LitValue)), Signed: lit.Signed, Bits: lit.Bits, Typ: lit.Typ}
		}
	case FNEG:
		if lit, ok := u.Operand.(*FloatLit); ok {
			return &FloatLit{LitValue: -lit.LitValue, Bits: lit.Bits, Typ: lit.Typ}
		}
	case NOT:
		if lit, ok := u.Operand.(*IntegerLit); ok {
			return &IntegerLit{LitValue: ^lit.LitValue, Signed: lit.Signed, Bits: lit.Bits, Typ: lit.Typ}
		}
	}
	return nil
}

// ─── CallInst ─────────────────────────────────────────────────────────────

func (c *CallInst) Def() Value { return c.Target }
func (c *CallInst) Uses() []Value {
	uses := make([]Value, 0, len(c.Args)+1)
	if c.Callee != nil {
		uses = append(uses, c.Callee)
	}
	uses = append(uses, c.Args...)
	return uses
}
func (c *CallInst) String() string {
	var names []string
	for _, a := range c.Args {
		names = append(names, a.Name())
	}
	calleeName := "<nil>"
	if c.Callee != nil {
		calleeName = c.Callee.Name()
	}
	call := fmt.Sprintf("CALL %s(%s)", calleeName, strings.Join(names, ", "))
	if c.Target == nil {
		return call
	}
	return fmt.Sprintf("%s := %s", c.Target.Name(), call)
}
func (c *CallInst) ReplaceUses(m map[string]Value) {
	if c.Callee != nil {
		if nv, ok := m[c.Callee.BaseName()]; ok {
			c.Callee = nv
		}
	}
	for i, a := range c.Args {
		if nv, ok := m[a.BaseName()]; ok {
			c.Args[i] = nv
		}
	}
}
func (c *CallInst) ReplaceDef(t Value) { c.Target = t }

// ─── LoadInst ─────────────────────────────────────────────────────────────

func (l *LoadInst) Def() Value    { return l.Target }
func (l *LoadInst) Uses() []Value { return []Value{l.Addr} }
func (l *LoadInst) String() string {
	return fmt.Sprintf("%s := LOAD %s", l.Target.Name(), l.Addr.Name())
}
func (l *LoadInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[l.Addr.BaseName()]; ok {
		l.Addr = nv
	}
}
func (l *LoadInst) ReplaceDef(t Value) { l.Target = t }

// ─── StoreInst ────────────────────────────────────────────────────────────

func (s *StoreInst) Def() Value    { return nil }
func (s *StoreInst) Uses() []Value { return []Value{s.Addr, s.Val} }
func (s *StoreInst) String() string {
	return fmt.Sprintf("STORE %s, %s", s.Val.Name(), s.Addr.Name())
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

// ─── AddrOf ───────────────────────────────────────────────────────────────

func (a *AddrOf) Def() Value    { return a.Target }
func (a *AddrOf) Uses() []Value { return []Value{a.Addr} }
func (a *AddrOf) String() string {
	return fmt.Sprintf("%s := ADDR %s", a.Target.Name(), a.Addr.Name())
}
func (a *AddrOf) ReplaceUses(m map[string]Value) {
	if nv, ok := m[a.Addr.BaseName()]; ok {
		a.Addr = nv
	}
}
func (a *AddrOf) ReplaceDef(t Value) { a.Target = t }

// ─── PhiInst ──────────────────────────────────────────────────────────────

func (phi *PhiInst) Def() Value { return phi.Target }
func (phi *PhiInst) Uses() []Value {
	var uses []Value
	for _, a := range phi.Args {
		uses = append(uses, a.Value)
	}
	return uses
}
func (phi *PhiInst) String() string {
	var parts []string
	for _, a := range phi.Args {
		parts = append(parts, fmt.Sprintf("[%s %s]", a.Block.Label, a.Value.Name()))
	}
	return fmt.Sprintf("%s = PHI %s", phi.Target, strings.Join(parts, ", "))
}
func (phi *PhiInst) ReplaceUses(m map[string]Value) {
	for _, a := range phi.Args {
		if nv, ok := m[a.Value.BaseName()]; ok {
			a.Value = nv
		}
	}
}
func (phi *PhiInst) AddArg(block *Block, value Value) {
	phi.Args = append(phi.Args, &PHIArg{Block: block, Value: value})
}
func (phi *PhiInst) ReplaceDef(t Value) { phi.Target = t }

// ─── GEPInst ──────────────────────────────────────────────────────────────

func (g *GEPInst) Def() Value { return g.Target }
func (g *GEPInst) Uses() []Value {
	uses := []Value{g.Base}
	for _, idx := range g.Indices {
		if idx.Index != nil {
			uses = append(uses, idx.Index)
		}
	}
	return uses
}
func (g *GEPInst) String() string {
	var steps []string
	for _, idx := range g.Indices {
		if idx.Field != "" {
			steps = append(steps, "."+idx.Field)
		} else {
			steps = append(steps, fmt.Sprintf("[%s]", idx.Index.Name()))
		}
	}
	return fmt.Sprintf("%s := GEP %s%s", g.Target.Name(), g.Base.Name(), strings.Join(steps, ""))
}
func (g *GEPInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[g.Base.BaseName()]; ok {
		g.Base = nv
	}
	for i, idx := range g.Indices {
		if idx.Index != nil {
			if nv, ok := m[idx.Index.BaseName()]; ok {
				g.Indices[i].Index = nv
			}
		}
	}
}
func (g *GEPInst) ReplaceDef(t Value) {
	if temp, ok := t.(*Temp); ok {
		g.Target = temp
	}
}

// ─── CastInst ─────────────────────────────────────────────────────────────

func (c *CastInst) Def() Value    { return c.Target }
func (c *CastInst) Uses() []Value { return []Value{c.Operand} }
func (c *CastInst) String() string {
	return fmt.Sprintf("%s := %s %s %s", c.Target.Name(), c.Op, c.ToType, c.Operand.Name())
}
func (c *CastInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[c.Operand.BaseName()]; ok {
		c.Operand = nv
	}
}
func (c *CastInst) ReplaceDef(t Value) { c.Target = t }

// ─── SwitchInst ───────────────────────────────────────────────────────────

func (s *SwitchInst) Def() Value { return nil }
func (s *SwitchInst) Uses() []Value {
	uses := []Value{s.Selector}
	for _, arm := range s.Arms {
		if arm.Lo != nil {
			uses = append(uses, arm.Lo)
		}
		if arm.Hi != nil {
			uses = append(uses, arm.Hi)
		}
	}
	return uses
}
func (s *SwitchInst) String() string {
	var sb strings.Builder
	fmt.Fprintf(&sb, "SWITCH %s [default → %s]", s.Selector.Name(), s.Default)
	for _, arm := range s.Arms {
		if arm.Hi == nil || arm.Hi == arm.Lo {
			fmt.Fprintf(&sb, " [%s → %s]", arm.Lo.Name(), arm.Label)
		} else {
			fmt.Fprintf(&sb, " [%s..%s → %s]", arm.Lo.Name(), arm.Hi.Name(), arm.Label)
		}
	}
	return sb.String()
}
func (s *SwitchInst) ReplaceUses(m map[string]Value) {
	if nv, ok := m[s.Selector.BaseName()]; ok {
		s.Selector = nv
	}
	for i, arm := range s.Arms {
		if arm.Lo != nil {
			if nv, ok := m[arm.Lo.BaseName()]; ok {
				s.Arms[i].Lo = nv
			}
		}
		if arm.Hi != nil {
			if nv, ok := m[arm.Hi.BaseName()]; ok {
				s.Arms[i].Hi = nv
			}
		}
	}
}
func (s *SwitchInst) ReplaceDef(Value) {}
