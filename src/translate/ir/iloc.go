package translate

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
