package mir

import "fmt"

type SSAInfo struct {
	// Current version for each variable (used during renaming)
	VersionCounter map[string]int

	// Stack of SSA names per source name (used in recursive renaming)
	VersionStack map[string][]string

	// Map of block labels to φ-functions inserted there
	PhiFuncs map[string][]*PhiInst

	// Optional: Def-to-Use or Use-to-Def chains
	DefSites map[string]map[*Block]struct{}
	UseSites map[string]map[*Block]struct{}
}

func NewSSAInfo() *SSAInfo {
	return &SSAInfo{
		VersionCounter: make(map[string]int),
		VersionStack:   make(map[string][]string),
		PhiFuncs:       make(map[string][]*PhiInst),
		DefSites:       make(map[string]map[*Block]struct{}),
		UseSites:       make(map[string]map[*Block]struct{}),
	}
}

func (r *SSAInfo) NewValue(v Value) Value {
	switch value := v.(type) {
	case *Temp:
		return &Temp{
			Ident:    r.Current(value.OrigName),
			OrigName: value.OrigName,
			Typ:      value.Typ,
			Size:     value.Size,
		}
	case *GlobalVariable:
		return &GlobalVariable{
			Ident:    r.Current(value.OrigName),
			OrigName: value.OrigName,
			Typ:      value.Typ,
			Size:     value.Size,
		}
	case *IntegerLit, *FloatLit, *CharLit, *StrLit, *NamedConst:
		return value
	default:
		panic(fmt.Sprintf("unknown value type in SSA: %T", v))
	}
}

func (r *SSAInfo) Push(v string) string {
	id := r.VersionCounter[v]
	r.VersionCounter[v]++

	newName := fmt.Sprintf("%s.%d", v, id)
	r.VersionStack[v] = append(r.VersionStack[v], newName)
	return newName
}

// Current name (top of stack) or original if none
func (r *SSAInfo) Current(v string) string {
	s := r.VersionStack[v]
	if len(s) == 0 {
		return v
	}
	return s[len(s)-1]
}

// Pop name
func (r *SSAInfo) Pop(v string) {
	s := r.VersionStack[v]
	if len(s) == 0 {
		return
	}
	r.VersionStack[v] = s[:len(s)-1]
}

// BaseName returns original var base (strip ".N" suffix if present)
func (r *SSAInfo) BaseName(v string) string {
	for i := len(v) - 1; i >= 0; i-- {
		if v[i] == '.' {
			return v[:i]
		}
	}
	return v
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

func (b *Block) HasPhi(v string) bool {
	for _, ins := range b.Instrs {
		if p, ok := ins.(*PhiInst); ok {
			if p.Target.Name() == v {
				return true
			}
		}
	}
	return false
}

// AddPhi inserts a phi node for v at the top of b (preserving order)
func (b *Block) AddPhi(p *PhiInst) {
	// Insert at beginning to ensure φs come before other instructions
	b.Instrs = append([]Instr{p}, b.Instrs...)
}
