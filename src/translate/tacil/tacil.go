package tacil

import "fmt"

type Opcode int

func (op Opcode) String() string {
	return opcodes[op]
}

var opcodes = [...]string{
	Add: "+",
	Sub: "-",
	Mul: "*",
	Div: "div",

	Ld:  "load",
	Str: "store",

	Xor:  "^",
	Or:   "|",
	And:  "&",
	Shl:  "<<",
	LShr: ">>",
	AShr: ">>>",

	Call: "call",
	Phi:  "phi",

	Eq: "==",
	Ne: "!=",
	Gt: ">",
	Ge: ">=",
	Lt: "<",
	Le: "<=",

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
	Ld
	Str
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

	cmp_op_begin
	Eq
	Ne
	Gt
	Ge
	Lt
	Le
	cmp_op_end

	termop_begin
	Br
	Ret
	Jmp
	termop_end
)

type Expr interface {
	expr()
	Name() string
	BaseName() string
	SetName(string)
	HasName() bool
	Operand(int) Expr
	NumOperands() int
	Type() Type
	fmt.Stringer
}

type Stmt interface {
	stmt()
	Parent() *BasicBlock
	SetParent(*BasicBlock)
	fmt.Stringer
}

var tmp = 0

func NextTemp() string {
	t := fmt.Sprintf("t%d", tmp)
	tmp += 1

	return t
}
