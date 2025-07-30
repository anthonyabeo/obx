package ir

type OpCode int

func (i OpCode) String() string {
	switch i {
	case Add:
		return "+"
	case FAdd:
		return "+"
	case Sub:
		return "-"
	case FSub:
		return "-"
	case Mul:
		return "*"
	case FMul:
		return "*"
	case SDiv:
		return "/"
	case UDiv:
		return "/"
	case FDiv:
		return "/"
	case SRem:
		return "%"
	case URem:
		return "%"
	case FRem:
		return "%"
	case And:
		return "&"
	case Or:
		return "|"
	case Ashr:
		return ">>>"
	case Shl:
		return "<<"
	case Shr:
		return ">>"
	case Xor:
		return "xor"
	case Lsl:
		return "lsl"
	case Neg:
		return "-"
	case FNeg:
		return "-"
	case Inc:
		return "inc"
	case Dec:
		return "dec"
	case ICmpEq:
		return "icmp.eq"
	case ICmpNeq:
		return "icmp.neq"
	case ICmpLt:
		return "icmp.lt"
	case ICmpGt:
		return "icmp.gt"
	case ICmpLe:
		return "icmp.le"
	case ICmpGe:
		return "icmp.ge"
	case Store:
		return "store"
	case Load:
		return "load"
	case Move:
		return "mov"
	case Jmp:
		return "jmp"
	case Br:
		return "br"
	case Ret:
		return "ret"
	case Call:
		return "call"
	default: // Nop
		return ""
	}
}

const (
	Invalid OpCode = iota

	// Arithmetic Binary operations
	Add
	FAdd
	Sub
	FSub
	Mul
	FMul
	SDiv
	UDiv
	FDiv
	SRem
	URem
	FRem

	// Bitwise Binary operations
	And
	Or
	Ashr
	Shl
	Shr
	Xor
	Lsl

	// Unary operations
	Neg
	FNeg
	Inc
	Dec

	ICmpEq
	ICmpNeq
	ICmpLt
	ICmpGt
	ICmpLe
	ICmpGe

	Store
	Load
	Move

	Jmp
	Br
	Ret
	Call

	Nop
	Lbl
	Halt
)
