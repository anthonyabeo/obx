package lir

import "fmt"

type Node interface {
	Children() []Node
	fmt.Stringer
}

// Operand represents any value that can be used as input to an instruction.
type Operand interface {
	Node
	isOperand()
}

type Constant interface {
	Operand
	isConst()
}

type Inst interface {
	Node
	isInstr()
}

type Type interface {
	Node
	isLIRType()
}

type InstrOpCode int

func (i InstrOpCode) String() string {
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
	// Arithmetic Binary operations
	Add InstrOpCode = iota
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

// Label represents a named location in the LIR, typically used for
// jump targets, procedure entry points or data (e.g. variables)
type Label struct {
	Name string
	Kind string
}
