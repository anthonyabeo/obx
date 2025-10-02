package asm

type Module struct {
	Name    string
	Funcs   []*Function
	Globals map[string]*Global
}

type Function struct {
	Name       string
	Result     Type
	Exported   bool
	FrameInfo  *FrameInfo
	StackSlots map[string]StackSlot // map from temp ID to offset in frame
	Blocks     []*Block
	Entry      *Block
}

// DFSOrder returns the blocks in depth-first order starting from entry.
func (fn *Function) DFSOrder() []*Block {
	visited := make(map[int]bool)
	order := []*Block{}

	var dfs func(b *Block)
	dfs = func(b *Block) {
		if visited[b.ID] {
			return
		}
		visited[b.ID] = true
		order = append(order, b)
		for _, succ := range b.Succ {
			dfs(succ)
		}
	}

	if len(fn.Blocks) > 0 {
		dfs(fn.Entry)
	}
	return order
}

func (fn *Function) ReversePostOrder() []*Block {
	visited := make(map[int]bool)
	order := []*Block{}

	var dfs func(b *Block)
	dfs = func(b *Block) {
		if visited[b.ID] {
			return
		}
		visited[b.ID] = true
		for _, succ := range b.Succ {
			dfs(succ)
		}
		order = append(order, b)
	}

	if len(fn.Blocks) > 0 {
		dfs(fn.Entry)
	}

	// reverse the order to get reverse post-order
	for i, j := 0, len(order)-1; i < j; i, j = i+1, j-1 {
		order[i], order[j] = order[j], order[i]
	}

	return order
}

type StackSlot struct {
	ID     string
	Size   int
	Align  int
	Offset int
}

type FrameInfo struct {
	Size  int
	Align int
	Spill map[string]int // map from temp ID to offset in frame

}
type Block struct {
	ID    int
	Label string
	Instr []*Instr
	Term  *Instr
	Succ  []*Block
	Pred  []*Block

	//Def     RegSet
	//Use     RegSet
	//LiveIn  RegSet
	//LiveOut RegSet
}

//func (bb *Block) ComputeUseDef() {
//	bb.Use = NewRegSet()
//	bb.Def = NewRegSet()
//
//	for _, instr := range bb.Instr {
//		if instr.Def != nil && !bb.Use.Contains(instr.Def.Name) {
//			bb.Def.Add(instr.Def.Name)
//		}
//		for _, u := range instr.Uses {
//			if !bb.Def.Contains(u.Name) {
//				bb.Use.Add(u.Name)
//			}
//		}
//	}
//}

func NewBlock(id int, label string) *Block {
	return &Block{ID: id, Label: label}
}
