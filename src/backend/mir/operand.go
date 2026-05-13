package mir

import "fmt"

// Operand is any value that can appear as an instruction operand.
type Operand interface {
	Type() *Type
	String() string
}

// RegKind distinguishes virtual registers from target-assigned registers.
type RegKind int

const (
	VirtualReg RegKind = iota
	PhysicalReg
)

// Register is a machine register placeholder or a target-assigned register.
type Register struct {
	Name string
	Kind RegKind
	Ty   *Type
}

func NewRegister(name string, kind RegKind, ty *Type) *Register {
	return &Register{Name: name, Kind: kind, Ty: ty}
}

func (r *Register) String() string {
	if r == nil {
		return "<reg:nil>"
	}
	return r.Name
}

func (r *Register) Type() *Type {
	if r == nil {
		return nil
	}
	return r.Ty
}

// Label is a named control-flow target.
type Label struct {
	Name string
}

func NewLabel(name string) *Label { return &Label{Name: name} }

func (l *Label) String() string {
	if l == nil {
		return "<label:nil>"
	}
	return l.Name
}

func (*Label) Type() *Type { return nil }

// Symbol names a module-level global or external symbol.
type Symbol struct {
	Name string
	Ty   *Type
}

func NewSymbol(name string, ty *Type) *Symbol { return &Symbol{Name: name, Ty: ty} }

func (s *Symbol) String() string {
	if s == nil {
		return "<sym:nil>"
	}
	return s.Name
}

func (s *Symbol) Type() *Type {
	if s == nil {
		return nil
	}
	return s.Ty
}

// Immediate represents an immediate constant operand.
type Immediate struct {
	Value any
	Ty    *Type
}

func NewImmediate(v any, ty *Type) *Immediate { return &Immediate{Value: v, Ty: ty} }

func (i *Immediate) String() string {
	if i == nil {
		return "<imm:nil>"
	}
	return fmt.Sprint(i.Value)
}

func (i *Immediate) Type() *Type {
	if i == nil {
		return nil
	}
	return i.Ty
}

// Memory models a target-neutral address expression.
type Memory struct {
	Base   Operand
	Offset Operand
	Ty     *Type
}

func NewMemory(base Operand, offset Operand, ty *Type) *Memory {
	return &Memory{Base: base, Offset: offset, Ty: ty}
}

func (m *Memory) String() string {
	if m == nil {
		return "<mem:nil>"
	}
	if m.Offset == nil {
		return fmt.Sprintf("[%s]", m.Base)
	}
	return fmt.Sprintf("[%s + %s]", m.Base, m.Offset)
}

func (m *Memory) Type() *Type {
	if m == nil {
		return nil
	}
	return m.Ty
}

