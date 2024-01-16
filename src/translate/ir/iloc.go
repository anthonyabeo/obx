package ir

type Opcode int

const (
	Invalid Opcode = iota

	Add
	Sub
	Mult
	Div
	LShift
	RShift
)

type InstrKind int

const (
	Error InstrKind = iota

	Labeled
	CtrlFlow
	Normal
)

type Instruction interface {
	Opcode() Opcode
	Kind() InstrKind
	String() string
}

type OperandKind int

const (
	Err OperandKind = iota

	Register
	Number
	Label
)

type Operand struct {
	Name string
	Kind OperandKind
}

func CreateOperand(name string, kind OperandKind) *Operand {
	return &Operand{Name: name, Kind: kind}
}
