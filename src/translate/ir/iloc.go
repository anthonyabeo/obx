package ir

type Opcode int

func (op Opcode) String() string {
	return opcodes[op]
}

var opcodes = [...]string{
	Add: "add",

	Load: "load",
}

const (
	Invalid Opcode = iota

	Add
	Sub
	Mult
	Div
	LShift
	RShift

	Load
)
