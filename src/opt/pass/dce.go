package pass

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

type DeadCodeElimination struct {
	Nom string
}

func (dce DeadCodeElimination) Name() string { return dce.Nom }

func (dce DeadCodeElimination) Run(program *tacil.Program, symbols *tacil.SymbolTable) {
	for _, mod := range program.Modules {
		for _, f := range mod.GetFunctionList() {
			cfg := f.CFG()
			eliminateUselessControlFlow(cfg)
			eliminateUselessCode(cfg)
		}
	}
}

func eliminateUselessCode(cfg *tacil.ControlFlowGraph) {

}

func eliminateUselessControlFlow(cfg *tacil.ControlFlowGraph) {
	changed := true
	for changed {
		changed = false

		post := cfg.PostOrder()
		changed = makePass(cfg, post)
	}
}

func makePass(cfg *tacil.ControlFlowGraph, post []*tacil.BasicBlock) bool {
	for _, BB := range post {
		if BB == cfg.Entry {
			jmp := BB.LastInst().(*tacil.Jump)
			if jmp == nil {
				panic("last (and only) instruction in 'entry' block should be an unconditional branch")
			}

			jmp.Dst = cfg.Succ["entry"].Pop()
			continue
		}

		lastInst := BB.LastInst()

		//if both targets are identical then replace the branch with a jump
		condBr, ok := lastInst.(*tacil.CondBr)
		if ok && (condBr.IfTrue == condBr.IfFalse) {
			BB.Instr().Remove(BB.Instr().Back())
			BB.Instr().PushBack(tacil.CreateJmp(condBr.IfTrue))
			return true
		}

		if jmp, ok := lastInst.(*tacil.Jump); ok {
			Dst := jmp.Dst
			if BB.Empty() {
				removeEmptyBlock(BB, Dst, cfg)
				return true
			}

			if cfg.Pred[Dst.Name()].Size() == 1 {
				mergeBlocks(BB, Dst, cfg)
				return true
			}

			_, isCondBr := Dst.LastInst().(*tacil.CondBr)
			if Dst.Empty() && isCondBr {
				hoistBranch(BB, Dst, cfg)
				return true
			}
		}
	}

	return false
}

func mergeBlocks(pred, succ *tacil.BasicBlock, cfg *tacil.ControlFlowGraph) *tacil.BasicBlock {
	// create a new block to hold the content of 'pred' and 'succ'
	mergeBlock := tacil.CreateBasicBlock("merge", pred.Parent())
	cfg.Succ["merge"] = adt.NewHashSet[*tacil.BasicBlock]()
	cfg.Pred["merge"] = adt.NewHashSet[*tacil.BasicBlock]()

	// copy the instructions from pred (minus the last branch instruction) and succ
	// to the new block
	for i := pred.Instr().Front(); i != nil; i = i.Next() {
		if i == pred.Instr().Back() {
			continue
		}

		mergeBlock.AddInstr(i.Value.(tacil.Stmt))
	}

	for j := succ.Instr().Front(); j != nil; j = j.Next() {
		mergeBlock.AddInstr(j.Value.(tacil.Stmt))
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

func removeEmptyBlock(i, j *tacil.BasicBlock, cfg *tacil.ControlFlowGraph) {
	if cfg.Pred[i.Name()] != nil {
		for _, p := range cfg.Pred[i.Name()].Elems() {
			cfg.Pred[j.Name()].Add(p)
			cfg.Succ[p.Name()].Add(j)

			switch br := p.LastInst().(type) {
			case *tacil.CondBr:
				if i == br.IfTrue {
					br.IfTrue = j
				} else {
					br.IfFalse = j
				}
			case *tacil.Jump:
				br.Dst = j
			default:
				panic(fmt.Sprintf("last instruction in block %s must be a Jmp or CondBr. Got %s", p.Name(), br))
			}
		}
	}

	cfg.DeleteBlocks(i)
}

func hoistBranch(i, j *tacil.BasicBlock, cfg *tacil.ControlFlowGraph) {
	for _, s := range cfg.Succ[j.Name()].Elems() {
		cfg.Succ[i.Name()].Add(s)
		cfg.Pred[s.Name()].Add(i)
	}

	cfg.Succ[i.Name()].Remove(j)
	cfg.Pred[j.Name()].Remove(i)
}
