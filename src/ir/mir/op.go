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
	LSHL // Shift Left Logical
	LSHR // Shift Right Logical
	ASHR // Shift Right Arithmetic

	OR
	AND
	IN

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
		return "ADD"
	case SUB:
		return "SUB"
	case MUL:
		return "MUL"
	case DIV:
		return "DIV"
	case REM:
		return "REM"
	case NOT:
		return "NOT"
	case NEG:
		return "NEG"
	case EQ:
		return "EQ"
	case NE:
		return "NE"
	case LT:
		return "LT"
	case LE:
		return "LE"
	case GT:
		return "GT"
	case GE:
		return "GE"
	case OR:
		return "OR"
	case AND:
		return "AND"
	case LD:
		return "LD"
	case LSHL:
		return "LSHL"
	case LSHR:
		return "LSHR"
	case ASHR:
		return "ASHR"
	default:
		return "unknown"
	}
}
