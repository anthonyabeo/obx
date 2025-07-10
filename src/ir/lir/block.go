package lir

// Block : A basic block of LIR instructions with a label and a list of instructions.
type Block struct {
	Label  string
	Instrs []Instr
}
