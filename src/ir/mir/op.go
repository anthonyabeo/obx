package mir

type InstrOp int

const (
	UNKNOWN InstrOp = iota

	arith_binary_begin
	ADD
	SUB
	MUL
	DIV
	REM
	arith_binary_end

	NEG
	NOT

	OR
	AND

	cmp_begin
	EQ
	NE
	LT
	LE
	GT
	GE
	cmp_end

	LD
)

func (op InstrOp) IsCmpCondCode() bool { return cmp_begin < op && op < cmp_end }
func (op InstrOp) String() string {
	switch op {
	case ADD:
		return "add"
	case SUB:
		return "sub"
	case MUL:
		return "mul"
	case DIV:
		return "div"
	case REM:
		return "rem"
	case NOT:
		return "not"
	case NEG:
		return "neg"
	case EQ:
		return "eq"
	case NE:
		return "ne"
	case LT:
		return "lt"
	case LE:
		return "le"
	case GT:
		return "gt"
	case GE:
		return "ge"
	case OR:
		return "or"
	case AND:
		return "and"
	case LD:
		return "ld"

	default:
		return "unknown"
	}
}
