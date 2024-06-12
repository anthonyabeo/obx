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
		}
	}

	return locations
}
