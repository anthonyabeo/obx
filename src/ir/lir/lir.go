package lir

import "fmt"

// Operand represents any value that can be used as input to an instruction.
type Operand interface {
	String() string
	isOperand()
}

type Constant interface {
	Operand
	isConst()
}

type Instr interface {
	isInstr()
	fmt.Stringer
}

// Program : An entire LIR program — may have multiple modules or procedures
type Program struct {
	Procs   []*Proc      // All procedures defined
	Globals []GlobalDecl // Global variables (optional)
}

// Proc : A procedure/function in LIR.
type Proc struct {
	Name       string     // e.g. "main", "Insert"
	Params     []Register // input registers (may include return reg)
	Locals     []Register // all locals (excluding params); for allocation info
	Blocks     []*Block   // list of blocks; usually starts with "entry"
	IsExported bool       // for modules with export rules
}

// GlobalDecl : Global variable/constant declaration
type GlobalDecl struct {
	Name string // Global name
	Type string // Type (e.g. "i32")
	Init string // Optional init value or empty
}

type Op string

const (
	Add = "add"
	Sub = "sub"
	Mul = "mul"
	Div = "div"
	Mod = "mod"
	Not = "not"
	Eq  = "eq"
	Neq = "neq"
	Lt  = "lt"
	Leq = "leq"
	Gt  = "gt"
	Geq = "geq"
)
