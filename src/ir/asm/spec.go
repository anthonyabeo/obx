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
	Blocks   []*Block
	Entry    *Block
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

type Block struct {
	ID    int
	Label string
	Instr []*Instr
	Term  *Instr
	Succ  []*Block
	Pred  []*Block
}

func NewBlock(id int, label string) *Block {
	return &Block{ID: id, Label: label}
}
