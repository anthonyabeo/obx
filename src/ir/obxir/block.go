package obxir

import "fmt"

// ─── SSAInfo ──────────────────────────────────────────────────────────────

// SSAInfo tracks SSA renaming state for a function.
type SSAInfo struct {
	VersionCounter map[string]int
	VersionStack   map[string][]string
	PhiFuncs       map[string][]*PhiInst
	DefSites       map[string]map[*Block]struct{}
	UseSites       map[string]map[*Block]struct{}
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
		return &Temp{Ident: r.Current(value.OrigName), OrigName: value.OrigName, Typ: value.Typ, Size: value.Size}
	case *Local:
		return &Local{Ident: r.Current(value.OrigName), OrigName: value.OrigName, Typ: value.Typ, Size: value.Size}
	case *Param:
		return value
	case *GlobalVariable:
		return &GlobalVariable{Ident: r.Current(value.OrigName), OrigName: value.OrigName, Typ: value.Typ, Size: value.Size}
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

func (r *SSAInfo) Current(v string) string {
	s := r.VersionStack[v]
	if len(s) == 0 {
		return v
	}
	return s[len(s)-1]
}

func (r *SSAInfo) Pop(v string) {
	s := r.VersionStack[v]
	if len(s) == 0 {
		return
	}
	r.VersionStack[v] = s[:len(s)-1]
}

func (r *SSAInfo) BaseName(v string) string {
	for i := len(v) - 1; i >= 0; i-- {
		if v[i] == '.' {
			return v[:i]
		}
	}
	return v
}

// ─── DominatorTree ────────────────────────────────────────────────────────

type DominatorTree struct {
	IDom    map[*Block]*Block
	DomTree map[*Block][]*Block
	DF      map[*Block][]*Block
}

func NewDominatorTree() *DominatorTree {
	return &DominatorTree{
		IDom:    make(map[*Block]*Block),
		DomTree: make(map[*Block][]*Block),
		DF:      make(map[*Block][]*Block),
	}
}

// ─── Block ────────────────────────────────────────────────────────────────

// BlockID is a package-level counter for unique block IDs.
// Use NewBlock() rather than constructing Block directly.
var BlockID int

type Block struct {
	ID     int
	Label  string
	Instrs []Instr
	Term   Instr
	Preds  map[int]*Block
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

func (b *Block) IsJoinBlock() bool { return len(b.Preds) > 1 }
func (b *Block) IsBrBlock() bool   { return len(b.Succs) > 1 }

func (b *Block) HasPred(block *Block) bool {
	_, exists := b.Preds[block.ID]
	return exists
}

func (b *Block) Predecessors() []*Block {
	out := make([]*Block, 0, len(b.Preds))
	for _, v := range b.Preds {
		out = append(out, v)
	}
	return out
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

// AddPhi inserts a phi node for v at the top of b (preserving phi-first order).
func (b *Block) AddPhi(p *PhiInst) {
	b.Instrs = append([]Instr{p}, b.Instrs...)
}
