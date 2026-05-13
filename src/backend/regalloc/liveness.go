package regalloc

import (
	"sort"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

type functionAnalysis struct {
	blocks []*blockAnalysis
	byID   map[int]*blockAnalysis
	byName map[string]*blockAnalysis
}

type blockAnalysis struct {
	block *mir.Block
	items []*itemAnalysis
	phis  []*mir.PhiInstr

	blockIn  map[string]bool
	blockOut map[string]bool

	edgeUses map[int]map[string]bool
}

type itemAnalysis struct {
	instr   mir.Instr
	liveIn  map[string]bool
	liveOut map[string]bool
	defs    []string
	uses    []string
}

// liveness analysis only; coordinator lives in register_allocation.go.

func analyzeFunction(fn *mir.Function) *functionAnalysis {
	fa := &functionAnalysis{
		blocks: make([]*blockAnalysis, 0, len(fn.Blocks)),
		byID:   make(map[int]*blockAnalysis, len(fn.Blocks)),
		byName: make(map[string]*blockAnalysis, len(fn.Blocks)),
	}

	blocks := append([]*mir.Block(nil), fn.Blocks...)
	sort.Slice(blocks, func(i, j int) bool { return blocks[i].ID < blocks[j].ID })

	for _, blk := range blocks {
		if blk == nil {
			continue
		}
		ba := &blockAnalysis{
			block:    blk,
			items:    make([]*itemAnalysis, 0, len(blk.Instrs)+1),
			phis:     make([]*mir.PhiInstr, 0),
			blockIn:  make(map[string]bool),
			blockOut: make(map[string]bool),
			edgeUses: make(map[int]map[string]bool),
		}
		for _, instr := range blk.Instrs {
			if phi, ok := instr.(*mir.PhiInstr); ok {
				ba.phis = append(ba.phis, phi)
			}
			ba.items = append(ba.items, &itemAnalysis{
				instr: instr,
				defs:  virtualDefs(instr),
				uses:  virtualUses(instr),
			})
		}
		if blk.Term != nil {
			ba.items = append(ba.items, &itemAnalysis{
				instr: blk.Term,
				defs:  virtualDefs(blk.Term),
				uses:  virtualUses(blk.Term),
			})
		}
		fa.blocks = append(fa.blocks, ba)
		fa.byID[blk.ID] = ba
		fa.byName[blk.Label] = ba
	}

	for _, ba := range fa.blocks {
		for _, succ := range ba.block.Succs {
			if succ == nil {
				continue
			}
			succBA := fa.byID[succ.ID]
			if succBA == nil {
				continue
			}
			for _, phi := range succBA.phis {
				if phi == nil {
					continue
				}
				for _, arm := range phi.Arms {
					if arm.BlockLabel != ba.block.Label {
						continue
					}
					if arm.Value == nil {
						continue
					}
					if ba.edgeUses[succ.ID] == nil {
						ba.edgeUses[succ.ID] = make(map[string]bool)
					}
					for _, reg := range operandRegs(arm.Value) {
						ba.edgeUses[succ.ID][reg] = true
					}
				}
			}
		}
	}

	changed := true
	for changed {
		changed = false
		for _, ba := range fa.blocks {
			oldIn := cloneSet(ba.blockIn)
			oldOut := cloneSet(ba.blockOut)

			newOut := make(map[string]bool)
			for _, succ := range ba.block.Succs {
				if succ == nil {
					continue
				}
				succBA := fa.byID[succ.ID]
				if succBA == nil {
					continue
				}
				unionInto(newOut, succBA.blockIn)
				unionInto(newOut, ba.edgeUses[succ.ID])
			}

			newIn := make(map[string]bool)
			for name := range ba.useSet() {
				newIn[name] = true
			}
			for name := range newOut {
				if !ba.defSet()[name] {
					newIn[name] = true
				}
			}

			ba.blockIn = newIn
			ba.blockOut = newOut
			if !equalSets(oldIn, ba.blockIn) || !equalSets(oldOut, ba.blockOut) {
				changed = true
			}
		}
	}

	for _, ba := range fa.blocks {
		live := cloneSet(ba.blockOut)
		for i := len(ba.items) - 1; i >= 0; i-- {
			item := ba.items[i]
			item.liveOut = cloneSet(live)
			for _, d := range item.defs {
				delete(live, d)
			}
			for _, u := range item.uses {
				live[u] = true
			}
			item.liveIn = cloneSet(live)
		}
	}

	return fa
}

func (ba *blockAnalysis) defSet() map[string]bool {
	defs := make(map[string]bool)
	for _, item := range ba.items {
		for _, d := range item.defs {
			defs[d] = true
		}
	}
	return defs
}

func (ba *blockAnalysis) useSet() map[string]bool {
	uses := make(map[string]bool)
	seenDef := make(map[string]bool)
	for _, item := range ba.items {
		if isPhi(item.instr) {
			for _, d := range item.defs {
				seenDef[d] = true
			}
			continue
		}
		for _, u := range item.uses {
			if !seenDef[u] {
				uses[u] = true
			}
		}
		for _, d := range item.defs {
			seenDef[d] = true
		}
	}
	return uses
}
