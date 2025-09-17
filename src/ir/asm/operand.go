package asm

import "fmt"

type Operand interface {
	String() string
	Type() Type
}

type (
	MemAddr struct {
		Base   Operand // Base address (e.g., a pointer or array)
		Offset Operand // Offset from the base address
		Ty     Type    // Type of the value at the memory address
	}

	Imm struct {
		Value any // Immediate value
		Ty    Type
	}

	Register struct {
		Name string
		Mode RegMode
		Kind RegKind
		Ty   Type
	}

	Label struct {
		Name string
	}

	// RelocFunc denotes a RISC-V relocation function, e.g. %hi, %lo
	RelocFunc struct {
		Kind   RelocKind // %hi, %lo
		Symbol string
	}

	Global struct {
		Name  string
		Ty    Type
		Value any // Optional initial value
	}
)

func (r RelocFunc) String() string { return fmt.Sprintf("%s(%s)", r.Kind, r.Symbol) }
func (r RelocFunc) Type() Type {
	//TODO implement me
	panic("implement me")
}

func (l Label) String() string { return l.Name }
func (l Label) Type() Type {
	//TODO implement me
	panic("implement me")
}

func (g Global) String() string { return g.Name }
func (g Global) Type() Type     { return g.Ty }

func (r Register) String() string { return r.Name }
func (r Register) Type() Type     { return r.Ty }

func (i Imm) String() string { return fmt.Sprintf("%v", i.Value) }
func (i Imm) Type() Type     { return i.Ty }

func (m MemAddr) String() string { return fmt.Sprintf("%s(%s)", m.Offset.String(), m.Base) }
func (m MemAddr) Type() Type     { return m.Ty }
