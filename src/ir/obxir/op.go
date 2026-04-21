package obxir

// InstrOp is the opcode for a three-address-code instruction.
type InstrOp int

const (
	UNKNOWN InstrOp = iota

	arith_binary_begin
	ADD
	SUB
	MUL
	RDIV // Real division
	IDIV // Integer division
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
	IS
	XOR

	FNEG

	cmp_begin
	EQ
	NE
	LT
	LE
	GT
	GE

	FEQ
	FLT
	FLE
	FGT
	FGE
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
	case RDIV:
		return "RDIV"
	case IDIV:
		return "IDIV"
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
	case XOR:
		return "XOR"
	case FNEG:
		return "FNEG"
	case IN:
		return "IN"
	case IS:
		return "IS"
	case FEQ:
		return "FEQ"
	case FLT:
		return "FLT"
	case FLE:
		return "FLE"
	case FGT:
		return "FGT"
	case FGE:
		return "FGE"
	default:
		return "unknown"
	}
}
