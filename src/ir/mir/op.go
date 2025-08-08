package mir

type InstrOp int

const (
	UNKNOWN InstrOp = iota

	arith_binary_begin
	ADD
	SUB
	MUL
	DIV
	QUOT
	REM
	arith_binary_end

	NEG
	NOT

	cmp_begin
	EQ
	LT
	LTE
	GT
	GTE
	NEQ
	IS
	IN
	cmp_end
)

func (c InstrOp) String() string {
	switch c {
	case EQ:
		return "eq"
	case LT:
		return "lt"
	case LTE:
		return "lte"
	case GT:
		return "gt"
	case GTE:
		return "gte"
	case NEQ:
		return "neq"
	case IS:
		return "is"
	case IN:
		return "in"
	case ADD:
		return "add"
	case SUB:
		return "sub"
	case MUL:
		return "mul"
	case DIV:
		return "div"
	case QUOT:
		return "quot"
	case REM:
		return "rem"
	default:
		return "unknown"
	}
}
