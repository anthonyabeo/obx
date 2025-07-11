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

type Inst interface {
	isInstr()
	fmt.Stringer
}

type Type interface {
	isLIRType()
	fmt.Stringer
}

// Program : An entire LIR program — may have multiple modules or procedures
type Program struct {
	Modules []*Module
}

type Module struct {
	Name    string
	Procs   []*Procedure  // All procedures defined
	Globals []*GlobalDecl // Global variables (optional)
}

// GlobalDecl : Global variable/constant declaration
type GlobalDecl struct {
	Name string  // Global name
	Type Type    // Type (e.g. "i32")
	Init Operand // Optional init value or empty
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
