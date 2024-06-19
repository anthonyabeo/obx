package pass

import (
	"github.com/anthonyabeo/obx/src/opt/analy"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type Mem2Reg struct {
	name string
}

func (m Mem2Reg) Name() string { return m.name }
func (m Mem2Reg) Run(program *ir.Program) bool {
	for _, module := range program.Modules {
		for _, f := range module.GetFunctionList() {
			cfg := f.CFG()
			stack := &Stack{}
			vst := map[string]bool{}

			ComputePhiInsertLocations(cfg)
			RegisterPromotion(cfg, cfg.Entry, vst, stack)
		}
	}

	return true
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

	DF := analy.DominanceFrontier(cfg)
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

				if !storeBlocks.Contains(Name) {
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

// BlockValuePair
// ---------------------------------------------------------------
type BlockValuePair struct {
	blk *ir.BasicBlock
	val ir.Value
}

// Stack
// ---------------------------------------------------------------
type Stack struct {
	data []map[string]*BlockValuePair
}

func (s *Stack) Push(tup map[string]*BlockValuePair) {
	s.data = append(s.data, tup)
}

func (s *Stack) Pop() map[string]*BlockValuePair {
	item := s.data[len(s.data)-1]
	s.data = s.data[:len(s.data)-1]

	return item
}

func (s *Stack) Top() map[string]*BlockValuePair {
	if len(s.data) > 0 {
		return s.data[len(s.data)-1]
	}
	return nil
}

func (s *Stack) Size() int { return len(s.data) }

func (s *Stack) BlockValuePair(f string) (*ir.BasicBlock, ir.Value) {
	for i := s.Size() - 1; i >= 0; i-- {
		frame := s.data[i]
		if tuple, ok := frame[f]; ok {
			return tuple.blk, tuple.val
		}
	}

	return nil, nil
}
