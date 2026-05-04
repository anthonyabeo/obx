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
		return &Temp{Ident: r.Current(value.OrigName), OrigName: value.OrigName, Typ: value.Typ, Size: value.Size, IsAddr: value.IsAddr}
	case *Local:
		return &Local{Ident: r.Current(value.OrigName), OrigName: value.OrigName, Typ: value.Typ, Size: value.Size}
	case *Param:
		return value // params are not renamed by SSA construction
	case *GlobalVariable:
		return &GlobalVariable{Ident: r.Current(value.OrigName), OrigName: value.OrigName, Typ: value.Typ, Size: value.Size}
	case *IntegerLit, *FloatLit, *CharLit, *StrLit, *NamedConst:
		return value // compile-time constants are immutable; no renaming needed
	case *Function:
		return value
	case *Mem:
		// Mem holds a computed address; rename the base but not the Mem wrapper itself.
		// The base will be renamed when it is processed as a separate value.
		return value
	case *TypeValue:
		// TypeValue is a compile-time type operand; never renamed.
		return value
	default:
		panic(fmt.Sprintf("SSAInfo.NewValue: unhandled value kind %T", v))
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

// blockIDSeq is a package-level counter for block IDs.  It is intentionally
// unexported; all block creation should go through IRBuilder.NewBlock so that
// each build session uses a builder-local counter (IRBuilder.blockIDSeq),
// keeping IDs deterministic across test runs.
var blockIDSeq int

// newBlock allocates a Block using the provided counter.  Callers should
// prefer IRBuilder.NewBlock (which uses a per-builder counter) over calling
// this directly.
func newBlock(name string, counter *int) *Block {
	*counter++
	return &Block{
		ID:     *counter,
		Label:  name,
		Instrs: make([]Instr, 0),
		Preds:  make(map[int]*Block),
		Succs:  make(map[int]*Block),
	}
}

// NewBlock creates a Block using the package-level counter.  Prefer
// IRBuilder.NewBlock for all builder-driven construction.
func NewBlock(name string) *Block {
	return newBlock(name, &blockIDSeq)
}

type Block struct {
	ID     int
	Label  string
	Instrs []Instr
	Term   Instr // terminator; always the last element of Instrs
	Preds  map[int]*Block
	Succs  map[int]*Block
}

func (b *Block) IsJoinBlock() bool { return len(b.Preds) > 1 }
func (b *Block) IsBrBlock() bool   { return len(b.Succs) > 1 }

// IsTerminated reports whether this block has a terminator instruction set.
func (b *Block) IsTerminated() bool { return b.Term != nil }

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
