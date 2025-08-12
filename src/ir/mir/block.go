package mir

type SSAInfo struct {
	// Current version for each variable (used during renaming)
	VersionCounter map[string]int

	// Stack of SSA names per source name (used in recursive renaming)
	VersionStack map[string][]*Temp

	// Map of block labels to φ-functions inserted there
	PhiFuncs map[string][]*Phi

	// Optional: Def-to-Use or Use-to-Def chains
	DefSites map[*Temp]*Block
	UseSites map[*Temp][]Instr
}

type Phi struct {
	Target Temp
	Args   map[*Block]Value // one incoming value per predecessor block
}

type DominatorTree struct {
	IDom    map[*Block]*Block   // Immediate dominators
	DomTree map[*Block][]*Block // Dominator tree children
	DF      map[*Block][]*Block // Dominance frontiers
}

func NewDominatorTree() *DominatorTree {
	return &DominatorTree{
		IDom:    make(map[*Block]*Block),
		DomTree: make(map[*Block][]*Block),
		DF:      make(map[*Block][]*Block),
	}
}

var BlockID int

type Block struct {
	ID     int
	Label  string         // e.g. "L1"
	Instrs []Instr        // IR instructions
	Term   Instr          // Final terminator: br, goto, return
	Preds  map[int]*Block // Filled during CFG construction
	Succs  map[int]*Block
}

func NewBlock(name string) *Block {
	BlockID++

	return &Block{
		ID:     BlockID,
		Label:  name,
		Instrs: make([]Instr, 0),
		Preds:  make(map[int]*Block),
		Succs:  make(map[int]*Block),
	}
}

func (b *Block) IsJoinBlock() bool {
	return len(b.Preds) > 1
}

func (b *Block) IsBrBlock() bool {
	return len(b.Succs) > 1
}

func (b *Block) HasPred(block *Block) bool {
	_, exists := b.Preds[block.ID]
	return exists
}

func (b *Block) Predecessors() []*Block {
	values := make([]*Block, 0, len(b.Preds))
	for _, v := range b.Preds {
		values = append(values, v)
	}

	return values
}
