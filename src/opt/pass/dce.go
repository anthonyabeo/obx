package pass

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type DeadCodeElimination struct {
	Nom string
}

func (dce DeadCodeElimination) Name() string { return dce.Nom }

func (dce DeadCodeElimination) Run(program *ir.Program) {
	for _, mod := range program.Modules {
		for _, f := range mod.GetFunctionList() {
			cfg := f.CFG()
			eliminateUselessControlFlow(cfg)
			eliminateUselessCode(cfg)
		}
	}
}

func eliminateUselessCode(cfg *ir.ControlFlowGraph) {

}

func eliminateUselessControlFlow(cfg *ir.ControlFlowGraph) {
	changed := true
	for changed {
		changed = false

		post := cfg.PostOrder()
		changed = makePass(cfg, post)
	}
}

func makePass(cfg *ir.ControlFlowGraph, post []*ir.BasicBlock) bool {
	for _, BB := range post {
		if BB == cfg.Entry {
			jmp := BB.LastInst().(*ir.BranchInst)
			if jmp == nil || jmp.IsConditional() {
				panic("last (and only) instruction in 'entry' block should be an unconditional branch")
			}

			jmp.IfTrue = cfg.Succ["entry"].Pop()
			continue
		}

		lastInst, ok := BB.LastInst().(*ir.BranchInst)
		if !ok {
			continue
		}

		//if both targets are identical then replace the branch with a jump
		if lastInst.IsConditional() && (lastInst.IfTrue == lastInst.IfFalse) {
			BB.Instr().Remove(BB.Instr().Back())
			BB.Instr().PushBack(ir.CreateBr(lastInst.IfTrue))
			return true
		}

		if !lastInst.IsConditional() {
			Dst := lastInst.IfTrue
			if BB.Empty() {
				removeEmptyBlock(BB, Dst, cfg)
				return true
			}

			if cfg.Pred[Dst.Name()].Size() == 1 {
				mergeBlocks(BB, Dst, cfg)
				return true
			}

			br, ok := Dst.LastInst().(*ir.BranchInst)
			if Dst.Empty() && (ok && br.IsConditional()) {
				hoistBranch(BB, Dst, cfg)
				return true
			}
		}
	}

	return false
}

func mergeBlocks(pred, succ *ir.BasicBlock, cfg *ir.ControlFlowGraph) *ir.BasicBlock {
	// create a new block to hold the content of 'pred' and 'succ'
	mergeBlock := ir.CreateBasicBlock("merge", pred.Parent())
	cfg.Succ["merge"] = adt.NewHashSet[*ir.BasicBlock]()
	cfg.Pred["merge"] = adt.NewHashSet[*ir.BasicBlock]()

	// copy the instructions from pred (minus the last branch instruction) and succ
	// to the new block
	for i := pred.Instr().Front(); i != nil; i = i.Next() {
		if i == pred.Instr().Back() {
			continue
		}

		mergeBlock.AddInstr(i.Value.(ir.Instruction))
	}

	for j := succ.Instr().Front(); j != nil; j = j.Next() {
		mergeBlock.AddInstr(j.Value.(ir.Instruction))
	}

	// flow-graph accounting
	// the predecessors of pred become the predecessors of mergeBlock
	if cfg.Pred[pred.Name()] != nil {
		for _, p := range cfg.Pred[pred.Name()].Elems() {
			cfg.Pred[mergeBlock.Name()].Add(p)
			cfg.Succ[p.Name()].Add(mergeBlock)
		}
	}

	// the successors of succ become the successors of mergeBlock
	if cfg.Succ[succ.Name()] != nil {
		for _, s := range cfg.Succ[succ.Name()].Elems() {
			cfg.Succ[mergeBlock.Name()].Add(s)
			cfg.Pred[s.Name()].Add(mergeBlock)
		}
	}

	cfg.DeleteBlocks(pred, succ)

	return mergeBlock
}

func removeEmptyBlock(i, j *ir.BasicBlock, cfg *ir.ControlFlowGraph) {
	if cfg.Pred[i.Name()] != nil {
		for _, p := range cfg.Pred[i.Name()].Elems() {
			cfg.Pred[j.Name()].Add(p)
			cfg.Succ[p.Name()].Add(j)

			lastInst, ok := p.LastInst().(*ir.BranchInst)
			if !ok {
				panic(fmt.Sprintf("last instruction must be a branch, got '%s'", lastInst))
			}

			if lastInst.IsConditional() {
				if i == lastInst.IfTrue {
					lastInst.IfTrue = j
				} else {
					lastInst.IfFalse = j
				}
			} else {
				lastInst.IfTrue = j
			}
		}
	}

	cfg.DeleteBlocks(i)
}

func hoistBranch(i, j *ir.BasicBlock, cfg *ir.ControlFlowGraph) {
	cfg.Succ[i.Name()].Add(j)
}
