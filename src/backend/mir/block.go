package mir

// Block is a backend MIR basic block with explicit instruction and CFG links.
type Block struct {
	ID     int
	Label  string
	Instrs []Instr
	Term   Terminator
	Preds  map[int]*Block
	Succs  map[int]*Block
}

func NewBlock(id int, label string) *Block {
	return &Block{
		ID:     id,
		Label:  label,
		Instrs: make([]Instr, 0),
		Preds:  make(map[int]*Block),
		Succs:  make(map[int]*Block),
	}
}

func (b *Block) AddInstr(instr Instr) {
	if b == nil || instr == nil {
		return
	}
	b.Instrs = append(b.Instrs, instr)
}

func (b *Block) SetTerminator(term Terminator) {
	if b == nil {
		return
	}
	b.Term = term
}

func (b *Block) AddPred(pred *Block) {
	if b == nil || pred == nil {
		return
	}
	b.Preds[pred.ID] = pred
}

func (b *Block) AddSucc(succ *Block) {
	if b == nil || succ == nil {
		return
	}
	b.Succs[succ.ID] = succ
}

func (b *Block) HasPred(pred *Block) bool {
	if b == nil || pred == nil {
		return false
	}
	_, ok := b.Preds[pred.ID]
	return ok
}

func (b *Block) HasSucc(succ *Block) bool {
	if b == nil || succ == nil {
		return false
	}
	_, ok := b.Succs[succ.ID]
	return ok
}

func (b *Block) String() string {
	if b == nil {
		return "<block:nil>"
	}
	return b.Label
}
