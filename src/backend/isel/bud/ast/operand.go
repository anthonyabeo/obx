package ast

import "fmt"

type OperandKind int

const (
	OKUnknown OperandKind = iota
	OKRegister
	OKMem
	OKLabel
	OKImm
	OKReloc
	OKGlobal
)

type Operand interface {
	Kind() OperandKind
	Desc() string
	fmt.Stringer
}

/////////////////////////////////////////////////////////////////////////////////////////
// Register denotes registers used in instructions. They could be general purpose,
// floating point or special purpose like stack pointer. The 'mode' field also states
// whether they are virtual or physical: this is to provide more contextual information
// to the register allocator.
/////////////////////////////////////////////////////////////////////////////////////////

type Register struct {
	Type string // e.g. GPR, SPR
	Mode string // e.g. virt or phys
	Name string // e.g. $rd, $ofs
}

func (r Register) Desc() string      { return r.Name }
func (r Register) Kind() OperandKind { return OKRegister }
func (r Register) String() string    { return fmt.Sprintf("%s:%s:$%s", r.Type, r.Mode, r.Name) }

/////////////////////////////////////////////////////////////////////////////////////////
// Mem denotes a memory location that is used as as operands to memory instructions
// such load and store. Mem constitutes a base register and an immediate offset value
/////////////////////////////////////////////////////////////////////////////////////////

type Mem struct {
	Base *Register
	Offs Operand
}

func (m Mem) Desc() string      { return m.String() }
func (m Mem) Kind() OperandKind { return OKMem }
func (m Mem) String() string    { return fmt.Sprintf("%d(%s)", m.Offs, m.Base) }

/////////////////////////////////////////////////////////////////////////////////////////
// Label denotes a named memory location. Label are used as target destinations for
// branch instructions, among other things.
/////////////////////////////////////////////////////////////////////////////////////////

type Label struct {
	Name  string
	Value string
}

func (l Label) Desc() string      { return l.Name }
func (l Label) Kind() OperandKind { return OKLabel }
func (l Label) String() string    { return l.Name }

/////////////////////////////////////////////////////////////////////////////////////////
// Immediate denotes a number literal
/////////////////////////////////////////////////////////////////////////////////////////

type Imm struct {
	Value any
	Name  string
}

func (i Imm) Desc() string      { return i.Name }
func (i Imm) Kind() OperandKind { return OKImm }
func (i Imm) String() string    { return i.Value.(string) }

/////////////////////////////////////////////////////////////////////////////////////////
// Reloc
/////////////////////////////////////////////////////////////////////////////////////////

type Reloc struct {
	Fxn    string
	Symbol string
}

func (r Reloc) Desc() string      { return r.String() }
func (r Reloc) Kind() OperandKind { return OKReloc }
func (r Reloc) String() string    { return fmt.Sprintf("%%%s($%s)", r.Fxn, r.Symbol) }

/////////////////////////////////////////////////////////////////////////////////////////
// Global denotes a symbol declared outside of all functions
/////////////////////////////////////////////////////////////////////////////////////////

type Global struct {
	Name string
}

func (g Global) Kind() OperandKind { return OKGlobal }
func (g Global) Desc() string      { return g.Name }
func (g Global) String() string    { return g.Name }
