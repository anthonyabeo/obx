package pass

import (
	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/opt/analy"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type MapOfBlockValuePair map[string]*BlockValuePair

type Mem2Reg struct {
	Nom string
}

func (m Mem2Reg) Name() string { return m.Nom }

func (m Mem2Reg) Run(program *ir.Program) {
	for _, module := range program.Modules {
		for _, f := range module.GetFunctionList() {
			cfg := f.CFG()
			stack := adt.NewStack[MapOfBlockValuePair]()
			vst := map[string]bool{}

			ComputePhiInsertLocations(cfg)
			RegisterPromotion(cfg, cfg.Entry, vst, stack)
		}
	}

	//return true
}

// RegisterPromotion
// ---------------------------------------------------------------
func RegisterPromotion[T MapOfBlockValuePair](cfg *ir.ControlFlowGraph, blk *ir.BasicBlock, vst map[string]bool, stack *adt.Stack[T]) {
	for _, nodes := range blk.Phi {
		for _, phi := range nodes {
			Block, V := GetBlockValuePair(stack, phi.Name())
			phi.AddIncoming(V, Block)

			top := stack.Top()
			top[phi.Name()] = &BlockValuePair{blk: blk, val: phi}
		}
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
				RegisterPromotion[T](cfg, instr.IfTrue, vst, stack)
				return
			}

			stack.Push(map[string]*BlockValuePair{})
			RegisterPromotion[T](cfg, instr.IfTrue, vst, stack)
			stack.Pop()

			stack.Push(map[string]*BlockValuePair{})
			RegisterPromotion[T](cfg, instr.IfFalse, vst, stack)
			stack.Pop()

			inst = inst.Next()
		case *ir.StoreInst:
			stack.Top()[instr.Dst.Name()] = &BlockValuePair{blk: blk, val: instr.Value}

			next := inst.Next()
			blk.RemoveInstr(inst)
			inst = next

		case *ir.LoadInst:
			_, LoadV := GetBlockValuePair(stack, instr.Ptr.Name())

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

	for _, BB := range cfg.Nodes.Elems() {
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

func GetBlocksThatContainStore(cfg *ir.ControlFlowGraph, dst string) adt.Set[*ir.BasicBlock] {
	storeBlocks := adt.NewHashSet[*ir.BasicBlock]()

	for _, BB := range cfg.Nodes.Elems() {
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

func ComputePhiInsertLocations(cfg *ir.ControlFlowGraph) {
	locations := make(map[string]adt.Set[*ir.BasicBlock])

	DF := analy.DominanceFrontier(cfg)
	for _, variable := range GetAllocInstr(cfg) {
		locations[variable.Name()] = adt.NewHashSet[*ir.BasicBlock]()

		storeBlocks := GetBlocksThatContainStore(cfg, variable.Name())
		workList := storeBlocks.Clone()

		for !workList.Empty() {
			block := workList.Pop()

			for _, BB := range DF[block.Name()].Elems() {
				if locations[variable.Name()].Empty() {
					if _, exists := locations[variable.Name()]; !exists {
						set := adt.NewHashSet[*ir.BasicBlock]()
						set.Add(BB)
						locations[variable.Name()] = set
					} else {
						locations[variable.Name()].Add(BB)
					}

					if !storeBlocks.Contains(BB) {
						workList.Add(BB)
					}
				}
			}
		}
	}

	for variable, BBs := range locations {
		for _, BB := range BBs.Elems() {
			phi := ir.CreateEmptyPHINode(variable)
			BB.InsertInstrBegin(phi)
			BB.Phi[variable] = append(BB.Phi[variable], phi)
		}
	}

}

// BlockValuePair
// ---------------------------------------------------------------
type BlockValuePair struct {
	blk *ir.BasicBlock
	val ir.Value
}

func GetBlockValuePair[T MapOfBlockValuePair](s *adt.Stack[T], f string) (*ir.BasicBlock, ir.Value) {
	for i := s.Size() - 1; i >= 0; i-- {
		frame := s.Items[i]
		if tuple, ok := frame[f]; ok {
			return tuple.blk, tuple.val
		}
	}

	return nil, nil
}

// ComputeGlobalNames
// ---------------------------------------------------------------
func ComputeGlobalNames(cfg *ir.ControlFlowGraph) ([]string, map[string]adt.Set[*ir.BasicBlock]) {
	Globals := make([]string, 0)
	Blocks := map[string]adt.Set[*ir.BasicBlock]{}

	for _, BB := range cfg.Nodes.Elems() {
		VarKill := map[string]bool{}

		for i := BB.Instr().Front(); i != nil; i = i.Next() {
			inst := i.Value.(ir.Instruction)
			for i := 1; i < inst.NumOperands()+1; i++ {
				if _, contains := VarKill[inst.Operand(i).Name()]; contains {
					Globals = append(Globals, inst.Operand(i).Name())
				}
			}

			VarKill[inst.Name()] = true
			if _, ok := Blocks[inst.Name()]; !ok {
				Blocks[inst.Name()] = adt.NewHashSet[*ir.BasicBlock]()
				Blocks[inst.Name()].Add(BB)
			} else {
				Blocks[inst.Name()].Add(BB)
			}
		}
	}

	return Globals, Blocks
}

// InsertPhiFunctions
// -----------------------------------------------------------------
func InsertPhiFunctions(Globals []string, Blocks, DF map[string]adt.Set[*ir.BasicBlock]) {
	for _, name := range Globals {
		WorkList := Blocks[name]
		for !WorkList.Empty() {
			block := WorkList.Pop()
			for _, BB := range DF[block.Name()].Elems() {
				if len(BB.Phi[name]) == 0 {
					phi := ir.CreateEmptyPHINode("")
					BB.InsertInstrBegin(phi)
					BB.Phi[name] = append(BB.Phi[name], phi)

					WorkList.Add(BB)
				}
			}
		}
	}
}
