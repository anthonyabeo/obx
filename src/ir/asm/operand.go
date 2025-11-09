package asm

import "fmt"

type Operand interface {
	String() string
	Type() Type
}

type SymbolKind int

const (
	ParamSK SymbolKind = iota
	LocalSK
	GlobalSK
	ConstSK
)

type (
	String struct {
		Name  string
		Value string
	}

	Argument struct {
		Index int
	}

	Symbol struct {
		Name      string
		Kind      SymbolKind
		Size      int
		Ty        Type
		ParamKind string // VAR/IN/VALUE, if symbol is a param
	}

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
)

func (s String) String() string { return s.Name }
func (s String) Type() Type     { panic("implement me") }

func (a Argument) String() string { return fmt.Sprintf("arg(#%d)", a.Index) }
func (a Argument) Type() Type     { panic("implement me") }

func (s Symbol) String() string { return s.Name }
func (s Symbol) Type() Type     { return s.Ty }

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

func (r Register) String() string { return r.Name }
func (r Register) Type() Type     { return r.Ty }

func (i Imm) String() string { return fmt.Sprintf("%v", i.Value) }
func (i Imm) Type() Type     { return i.Ty }

func (m MemAddr) String() string { return fmt.Sprintf("%s(%s)", m.Offset.String(), m.Base) }
func (m MemAddr) Type() Type     { return m.Ty }
