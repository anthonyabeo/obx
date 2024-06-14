package opt

import "github.com/anthonyabeo/obx/src/translate/ir"

func GetAllocInstr(cfg *ir.ControlFlowGraph) []*ir.AllocaInst {
	var Alloc []*ir.AllocaInst

	for _, BB := range cfg.Nodes {
		Instr := BB.Instr()
		for inst := Instr.Front(); inst != nil; inst = inst.Next() {
			alloc, ok := inst.Value.(*ir.AllocaInst)
			if !ok {
				continue
			}

			Alloc = append(Alloc, alloc)
		}
	}

	return Alloc
}

func GetBlocksThatContainStore(cfg *ir.ControlFlowGraph, dst string) ir.SetOfBBs {
	storeBlocks := ir.SetOfBBs{}

	for _, BB := range cfg.Nodes {
		Instr := BB.Instr()
		for inst := Instr.Front(); inst != nil; inst = inst.Next() {
			if store, ok := inst.Value.(*ir.StoreInst); ok && store.Operand(2).Name() == dst {
				storeBlocks.Add(BB)
				break
			}
		}
	}

	return storeBlocks
}

func ComputePhiInsertLocations(cfg *ir.ControlFlowGraph) map[string]ir.SetOfBBs {
	locations := map[string]ir.SetOfBBs{}

	DF := DominanceFrontier(cfg)
	for _, variable := range GetAllocInstr(cfg) {
		storeBlocks := GetBlocksThatContainStore(cfg, variable.Name())
		workList := storeBlocks.Clone()

		for !workList.Empty() {
			block := workList.Pop()

			for Name, BB := range DF[block.Name()] {
				if _, exists := locations[variable.Name()]; !exists {
					locations[variable.Name()] = ir.SetOfBBs{BB.Name(): BB}
				} else {
					locations[variable.Name()].Add(BB)
				}

				if storeBlocks.Contains(Name) {
					workList.Add(BB)
				}
			}
		}
	}

	for variable, BBs := range locations {
		for _, BB := range BBs {
			phi := ir.CreateEmptyPHINode(variable)
			BB.InsertInstrBegin(phi)
			BB.Phi = append(BB.Phi, phi)
		}
	}

	return locations
}

// RegisterPromotion
// ---------------------------------------------------------------
func RegisterPromotion(cfg *ir.ControlFlowGraph, blk *ir.BasicBlock, vst map[string]bool, stack *Stack) {
	for _, phi := range blk.Phi {
		Block, V := stack.BlockValuePair(phi.Name())
		phi.AddIncoming(V, Block)

		top := stack.Top()
		top[phi.Name()] = &BlockValuePair{blk: blk, val: phi}
	}

	if _, found := vst[blk.Name()]; found {
		return
	}

	vst[blk.Name()] = true
	l := blk.Instr()
	for inst := l.Front(); inst != nil; {
		switch instr := inst.Value.(type) {
		case *ir.BranchInst:
			if !instr.IsConditional() {
				stack.Push(map[string]*BlockValuePair{})
				RegisterPromotion(cfg, instr.IfTrue, vst, stack)
				return
			}

			stack.Push(map[string]*BlockValuePair{})
			RegisterPromotion(cfg, instr.IfTrue, vst, stack)
			stack.Pop()

			stack.Push(map[string]*BlockValuePair{})
			RegisterPromotion(cfg, instr.IfFalse, vst, stack)
			stack.Pop()

			inst = inst.Next()
		case *ir.StoreInst:
			stack.Top()[instr.Dst.Name()] = &BlockValuePair{blk: blk, val: instr.Value}

			next := inst.Next()
			blk.RemoveInstr(inst)
			inst = next

		case *ir.LoadInst:
			_, LoadV := stack.BlockValuePair(instr.Ptr.Name())

			next := inst.Next()
			blk.RemoveInstr(inst)
			inst = next

			cfg.Replace(instr, LoadV)
		case *ir.AllocaInst:
			next := inst.Next()
			blk.RemoveInstr(inst)
			inst = next
		default:
			inst = inst.Next()
		}
	}
}
