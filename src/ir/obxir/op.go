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
	FNE
	FLT
	FLE
	FGT
	FGE
	cmp_end

	LD
)

func (op InstrOp) IsCmpCondCode() bool { return cmp_begin < op && op < cmp_end }
func (op InstrOp) IsFloatCmp() bool    { return op >= FEQ && op < cmp_end }

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
	case FNE:
		return "FNE"
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

// ─── CastOp ───────────────────────────────────────────────────────────────

// CastOp encodes the kind of type conversion a CastInst performs.
// Using a named enum (rather than a raw InstrOp) keeps the type-conversion
// vocabulary separate from the arithmetic/comparison opcode space.
type CastOp int

const (
	Trunc    CastOp = iota // integer narrowing (drop high bits)
	ZExt                   // zero-extend to wider integer
	SExt                   // sign-extend to wider integer
	FpToSI                 // float → signed integer (truncation toward zero)
	FpToUI                 // float → unsigned integer
	SIToFp                 // signed integer → float
	UIToFp                 // unsigned integer → float
	Bitcast                // reinterpret bits (same width, different type)
	PtrToInt               // pointer → integer (address as integer)
	IntToPtr               // integer → pointer
)

func (c CastOp) String() string {
	switch c {
	case Trunc:
		return "TRUNC"
	case ZExt:
		return "ZEXT"
	case SExt:
		return "SEXT"
	case FpToSI:
		return "FPTOSI"
	case FpToUI:
		return "FPTOUI"
	case SIToFp:
		return "SITOFP"
	case UIToFp:
		return "UITOFP"
	case Bitcast:
		return "BITCAST"
	case PtrToInt:
		return "PTRTOINT"
	case IntToPtr:
		return "INTTOPTR"
	default:
		return "UNKNOWN_CAST"
	}
}
