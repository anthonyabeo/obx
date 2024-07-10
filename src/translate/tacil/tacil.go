package tacil

import "fmt"

type Opcode int

func (op Opcode) String() string {
	return opcodes[op]
}

var opcodes = [...]string{
	Add: "add",
	Sub: "sub",
	Mul: "mul",
	Div: "div",

	Load:  "load",
	Store: "store",

	Xor:  "xor",
	Or:   "or",
	And:  "and",
	Shl:  "shl",
	LShr: "lshl",
	AShr: "ashr",

	Call: "call",
	Phi:  "phi",

	Eq:  "eq",
	Ne:  "ne",
	UGt: "ugt",
	UGe: "uge",
	ULt: "ult",
	ULe: "ule",
	SGt: "sgt",
	SGe: "sge",
	SLt: "slt",
	SLe: "sle",

	Br:  "br",
	Ret: "ret",
	Jmp: "jmp",
}

const (
	Invalid Opcode = iota

	binop_begin
	Add
	Sub
	Mul
	Div
	binop_end

	memop_begin
	Alloca
	Load
	Store
	memop_end

	bit_binop_begin
	Xor
	Or
	And
	Shl
	LShr
	AShr
	bit_binop_end

	other_op_begin
	Call
	Phi
	other_op_end

	Eq
	Ne
	UGt
	UGe
	ULt
	ULe
	SGt
	SGe
	SLt
	SLe

	termop_begin
	Br
	Ret
	Jmp
	termop_end
)

type Expr interface {
	expr()
	Name() string
	SetName(string)
	HasName() bool
	Operand(int) Expr
	NumOperands() int
	Type() Type
	fmt.Stringer
}

type Stmt interface {
	stmt()
	fmt.Stringer
}

var tmp = 0

func NextTemp() string {
	t := fmt.Sprintf("t%d", tmp)
	tmp += 1

	return t
}
