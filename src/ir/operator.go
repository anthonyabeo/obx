package ir

type Operator int

func (op Operator) String() string {
	if int(op) < len(operators) {
		return operators[op]
	}
	return "unknown"
}

const (
	OpAdd Operator = iota
	OpSub
	OpMul
	OpQuot // Quotient operator for integer division
	OpDiv
	OpMod
	OpAnd
	OpOr
	OpXor
	OpNot
	OpNeg
	OpShl
	OpShr
	OpEq
	OpNeq
	OpLt
	OpLeq
	OpGt
	OpGeq
)

var operators = [...]string{
	OpAdd:  "add",
	OpSub:  "sub",
	OpMul:  "mul",
	OpDiv:  "div",
	OpMod:  "mod",
	OpAnd:  "and",
	OpOr:   "or",
	OpXor:  "xor",
	OpNot:  "not",
	OpNeg:  "neg",
	OpShl:  "shl",
	OpShr:  "shr",
	OpEq:   "eq",
	OpNeq:  "neq",
	OpLt:   "lt",
	OpLeq:  "leq",
	OpGt:   "gt",
	OpGeq:  "geq",
	OpQuot: "quot",
}
