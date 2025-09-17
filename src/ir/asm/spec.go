package asm

type Module struct {
	Name    string
	Funcs   []*Function
	Globals map[string]*Global
}

type Function struct {
	Name     string
	Result   Type
	Exported bool
	Params   map[string]Operand
	Locals   map[string]Operand
	Blocks   []*Block
}

type Block struct {
	Label string
	Instr []*Instr
}

func NewBlock(label string) *Block {
	return &Block{Label: label}
}
