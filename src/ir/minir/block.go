package minir

import "sort"

// BasicBlock and Function containers.

// Block is a basic block: a list of instructions with an explicit terminator.
type Block struct {
	ID     int
	Label  string
	Instrs []Instr
	Term   Terminator // should equal last instruction in Instrs

	// fast lookup maps
	Preds map[int]*Block
	Succs map[int]*Block

	// ordering of preds/succs for deterministic iteration (stores block IDs)
	PredOrder []int
	SuccOrder []int
}

// Function is a collection of blocks with an entry block and signature.
type Function struct {
	FnName string
	Params []*Temp
	Result Type
	Entry  *Block
	Exit   *Block
	Blocks map[int]*Block
	SymTab SymbolTable // function-local symbol table (params + alloca defs)
}

// Module is the top-level container for a single translation unit.
// It mirrors LLVM's Module: it aggregates global variables, read-only
// constants, external function declarations, and function definitions,
// all indexed by a unified symbol table.
type Module struct {
	Name      string
	Globals   []*GlobalVar    // mutable module-scope variables
	Constants []*GlobalConst  // read-only module-scope constants
	Externals []*ExternalFunc // imported / FFI function declarations
	Functions []*Function     // function definitions (with bodies)
	SymTab    SymbolTable     // module-scope symbol table
}

// Program is a collection of minir Modules produced by lowering a
// desugar.Program.  Each source module maps to exactly one minir.Module.
// This mirrors the relationship between desugar.Program → []*desugar.Module
// at the level of the minir IR.
type Program struct {
	Modules []*Module
}

// GetBlock finds a block by label; returns nil if not found.
func (f *Function) GetBlock(label string) *Block {
	for _, b := range f.Blocks {
		if b.Label == label {
			return b
		}
	}
	return nil
}

// AddSucc links s as a successor of b (and records ordering). It does not
// update the corresponding pred on s; callers should maintain both sides.
func (b *Block) AddSucc(s *Block) {
	if b.Succs == nil {
		b.Succs = make(map[int]*Block)
	}
	if _, ok := b.Succs[s.ID]; ok {
		return
	}
	b.Succs[s.ID] = s
	b.SuccOrder = append(b.SuccOrder, s.ID)
}

// AddPred links p as a predecessor of b (and records ordering). It does not
// update the corresponding succ on p; callers should maintain both sides.
func (b *Block) AddPred(p *Block) {
	if b.Preds == nil {
		b.Preds = make(map[int]*Block)
	}
	if _, ok := b.Preds[p.ID]; ok {
		return
	}
	b.Preds[p.ID] = p
	b.PredOrder = append(b.PredOrder, p.ID)
}

// SortedSuccs returns successors in deterministic order: if SuccOrder is
// populated it is used; otherwise the map keys are sorted and returned.
func (b *Block) SortedSuccs() []*Block {
	var out []*Block
	if len(b.SuccOrder) > 0 {
		for _, id := range b.SuccOrder {
			if s, ok := b.Succs[id]; ok {
				out = append(out, s)
			}
		}
		return out
	}
	// fallback: gather keys and sort
	ids := make([]int, 0, len(b.Succs))
	for id := range b.Succs {
		ids = append(ids, id)
	}
	if len(ids) == 0 {
		return nil
	}
	sort.Ints(ids)
	for _, id := range ids {
		out = append(out, b.Succs[id])
	}
	return out
}

// SortedPreds similar to SortedSuccs.
func (b *Block) SortedPreds() []*Block {
	var out []*Block
	if len(b.PredOrder) > 0 {
		for _, id := range b.PredOrder {
			if p, ok := b.Preds[id]; ok {
				out = append(out, p)
			}
		}
		return out
	}
	ids := make([]int, 0, len(b.Preds))
	for id := range b.Preds {
		ids = append(ids, id)
	}
	if len(ids) == 0 {
		return nil
	}
	sort.Ints(ids)
	for _, id := range ids {
		out = append(out, b.Preds[id])
	}
	return out
}
