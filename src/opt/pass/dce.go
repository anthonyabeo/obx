package pass

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/meer"
)

type DeadCodeElimination struct {
	Nom string
}

func (dce DeadCodeElimination) Name() string { return dce.Nom }

func (dce DeadCodeElimination) Run(program *meer.Program) {
	for _, unit := range program.Units {
		eliminateUselessControlFlow(unit.CFG)
		eliminateUselessCode(unit.CFG)
	}
}

func eliminateUselessCode(cfg *meer.ControlFlowGraph) {

}

func eliminateUselessControlFlow(cfg *meer.ControlFlowGraph) {
	changed := true
	for changed {
		changed = false

		post := cfg.PostOrder()
		changed = makePass(cfg, post)
	}
}

func makePass(cfg *meer.ControlFlowGraph, post []meer.BasicBlockID) bool {
	for _, BB := range post {
		block := cfg.Blocks[BB]
		if BB == cfg.Entry.ID() {
			jmp := cfg.Blocks[BB].LastInst().(*meer.JumpInst)
			if jmp == nil {
				panic("last (and only) instruction in 'entry' block should be an unconditional branch")
			}

			id := cfg.Suc[cfg.Entry.ID()].Pop()
			jmp.Dst = cfg.Blocks[id].Label()
			continue
		}

		lastInst := block.LastInst()

		//if both targets are identical then replace the branch with a jump
		condBr, ok := lastInst.(*meer.CondBrInst)
		if ok && (condBr.IfTrue == condBr.IfFalse) {
			block.Instr().Remove(block.Instr().Back())
			block.Instr().PushBack(meer.CreateJmp(condBr.IfTrue))
			return true
		}

		if jmp, ok := lastInst.(*meer.JumpInst); ok {
			Dst := cfg.Blocks[jmp.Dst.BlockID]
			if block.Empty() {
				removeEmptyBlock(block, Dst, cfg)
				return true
			}

			if cfg.Pred[Dst.ID()].Size() == 1 {
				mergeBlocks(block, Dst, cfg)
				return true
			}

			_, isCondBr := Dst.LastInst().(*meer.CondBrInst)
			if Dst.Empty() && isCondBr {
				hoistBranch(block, Dst, cfg)
				return true
			}
		}
	}

	return false
}

func mergeBlocks(pred, succ *meer.BasicBlock, cfg *meer.ControlFlowGraph) *meer.BasicBlock {
	// create a new block to hold the content of 'pred' and 'succ'
	mergeBlock := meer.CreateBasicBlock(meer.NewLabel("merge"))
	cfg.Suc[mergeBlock.ID()] = adt.NewHashSet[meer.BasicBlockID]()
	cfg.Pred[mergeBlock.ID()] = adt.NewHashSet[meer.BasicBlockID]()

	// copy the instructions from pred (minus the last branch instruction) and succ
	// to the new block
	for i := pred.Instr().Front(); i != nil; i = i.Next() {
		if i == pred.Instr().Back() {
			continue
		}

		mergeBlock.AddInstr(i.Value.(meer.Instruction))
	}

	for j := succ.Instr().Front(); j != nil; j = j.Next() {
		mergeBlock.AddInstr(j.Value.(meer.Instruction))
	}

	// flow-graph accounting
	// the predecessors of pred become the predecessors of mergeBlock
	if cfg.Pred[pred.ID()] != nil {
		for _, p := range cfg.Pred[pred.ID()].Elems() {
			cfg.Pred[mergeBlock.ID()].Add(p)
			cfg.Suc[p].Add(mergeBlock.ID())
		}
	}

	// the successors of succ become the successors of mergeBlock
	if cfg.Suc[succ.ID()] != nil {
		for _, s := range cfg.Suc[succ.ID()].Elems() {
			cfg.Suc[mergeBlock.ID()].Add(s)
			cfg.Pred[s].Add(mergeBlock.ID())
		}
	}

	cfg.DeleteBlocks(pred, succ)

	return mergeBlock
}

func removeEmptyBlock(i, j *meer.BasicBlock, cfg *meer.ControlFlowGraph) {
	if cfg.Pred[i.ID()] != nil {
		for _, p := range cfg.Pred[i.ID()].Elems() {
			cfg.Pred[j.ID()].Add(p)
			cfg.Suc[p].Add(j.ID())

			switch br := cfg.Blocks[p].LastInst().(type) {
			case *meer.CondBrInst:
				if i.ID() == br.IfTrue.BlockID {
					br.IfTrue = j.Label()
				} else {
					br.IfFalse = j.Label()
				}
			case *meer.JumpInst:
				br.Dst = j.Label()
			default:
				panic(fmt.Sprintf("last instruction in block %s must be a Jmp or CondBr. Got %s", cfg.Blocks[p].Name(), br))
			}
		}
	}

	cfg.DeleteBlocks(i)
}

func hoistBranch(i, j *meer.BasicBlock, cfg *meer.ControlFlowGraph) {
	for _, s := range cfg.Suc[j.ID()].Elems() {
		cfg.Suc[i.ID()].Add(s)
		cfg.Pred[s].Add(i.ID())
	}

	cfg.Suc[i.ID()].Remove(j.ID())
	cfg.Pred[j.ID()].Remove(i.ID())
}
