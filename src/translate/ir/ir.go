package ir

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

	Eq: "eq",
	Lt: "lt",
	Gt: "gt",
	Le: "le",
	Ge: "gte",

	Br:  "br",
	Ret: "ret",
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

	Load
	Store

	memop_end

	other_op_begin
	call
	other_op_end

	Eq
	Lt
	Gt
	Le
	Ge

	termop_begin
	Br
	Ret
	termop_end
)
