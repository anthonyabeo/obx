package analy

import (
	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/meer"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

// IterativeDataFlow implements an iterative data-flow algorithm for the
// reaching definitions problem.
//
// It used a lattice of 8-bit-wide-vectors. It takes in as input, 'Init' the
// initial value of the 'entry' node and F: BV^8 -> BV^8, a set of flow functions
// as explained in Muchnick 8.4
// ----------------------------------------------
func IterativeDataFlow(cfg *tacil.ControlFlowGraph, F map[string]func(tacil.BitVector) tacil.BitVector, Init tacil.BitVector) map[string]tacil.BitVector {
	rpo := cfg.ReversePostOrder()
	rpo = rpo[1:]

	workList := adt.NewQueueFrom[*tacil.BasicBlock](rpo)

	DFIn := map[string]tacil.BitVector{"entry": Init}
	for _, BB := range cfg.Nodes.Elems() {
		if BB.Name() != "entry" {
			DFIn[BB.Name()] = tacil.BitVector(255)
		}
	}

	for !workList.Empty() {
		B := workList.Dequeue()

		TotalEffect := tacil.BitVector(0)
		for _, name := range cfg.Pred[B.Name()].Elems() {
			in := DFIn[name.Name()]
			Effect := F[name.Name()](in)
			TotalEffect |= Effect
		}

		if DFIn[B.Name()] != TotalEffect {
			DFIn[B.Name()] = TotalEffect
			for _, succ := range cfg.Succ[B.Name()].Elems() {
				workList.Enqueue(succ)
			}
		}
	}

	return DFIn
}

// IterativeDataflowDragonBook an iterative data-flow algorithm for the reaching definitions problem.
// as defined at https://en.wikipedia.org/wiki/Reaching_definition
func IterativeDataflowDragonBook(cfg *tacil.ControlFlowGraph, GEN, KILL map[string]tacil.BitVector) (map[string]tacil.BitVector, map[string]tacil.BitVector) {
	rpo := cfg.ReversePostOrder()
	rpo = rpo[1:]

	workList := adt.NewQueueFrom[*tacil.BasicBlock](rpo)

	IN := map[string]tacil.BitVector{}
	OUT := map[string]tacil.BitVector{}
	for _, BB := range cfg.Nodes.Elems() {
		OUT[BB.Name()] = tacil.BitVector(0)
	}

	for !workList.Empty() {
		B := workList.Dequeue()

		IN[B.Name()] = tacil.BitVector(0)
		for _, name := range cfg.Pred[B.Name()].Elems() {
			IN[B.Name()] |= OUT[name.Name()]
		}

		prevOut := OUT[B.Name()]
		OUT[B.Name()] = GEN[B.Name()] | (IN[B.Name()] & ^KILL[B.Name()])

		if prevOut != OUT[B.Name()] {
			for _, succ := range cfg.Succ[B.Name()].Elems() {
				workList.Enqueue(succ)
			}
		}
	}

	return IN, OUT
}

func ReachingDefinition(cfg *tacil.ControlFlowGraph, GEN, KILL map[string]adt.Set[uint]) (map[string]adt.Set[uint], map[string]adt.Set[uint]) {
	rpo := cfg.ReversePostOrder()
	rpo = rpo[1:]

	workList := adt.NewQueueFrom[*tacil.BasicBlock](rpo)

	IN := map[string]adt.Set[uint]{}
	OUT := map[string]adt.Set[uint]{}
	for _, BB := range cfg.Nodes.Elems() {
		OUT[BB.Name()] = &adt.BitVector[uint]{}
	}

	for !workList.Empty() {
		B := workList.Dequeue()

		IN[B.Name()] = &adt.BitVector[uint]{}
		for _, name := range cfg.Pred[B.Name()].Elems() {
			IN[B.Name()] = IN[B.Name()].Union(OUT[name.Name()])
		}

		prevOut := OUT[B.Name()]
		diff := IN[B.Name()].Diff(KILL[B.Name()])
		OUT[B.Name()] = GEN[B.Name()].Union(diff)

		if prevOut != OUT[B.Name()] {
			for _, succ := range cfg.Succ[B.Name()].Elems() {
				workList.Enqueue(succ)
			}
		}
	}

	return IN, OUT
}

func LiveVariable(cfg *tacil.ControlFlowGraph, DEF, USE map[string]adt.Set[uint]) (map[string]adt.Set[uint], map[string]adt.Set[uint]) {
	rpo := cfg.Reverse().ReversePostOrder()
	rpo = rpo[1:]

	workList := adt.NewQueueFrom[*tacil.BasicBlock](rpo)

	IN := map[string]adt.Set[uint]{}
	OUT := map[string]adt.Set[uint]{}
	for _, BB := range cfg.Nodes.Elems() {
		IN[BB.Name()] = &adt.BitVector[uint]{}
	}

	changed := true
	for changed {
		changed = false

		for !workList.Empty() {
			B := workList.Dequeue()

			OUT[B.Name()] = &adt.BitVector[uint]{}
			for _, S := range cfg.Succ[B.Name()].Elems() {
				OUT[B.Name()] = OUT[B.Name()].Union(IN[S.Name()])
			}

			prevIN := IN[B.Name()]
			IN[B.Name()] = USE[B.Name()].Union(OUT[B.Name()].Diff(DEF[B.Name()]))
			if !prevIN.Equal(IN[B.Name()]) {
				changed = true
			}

		}
	}

	return IN, OUT
}

func AvailableExpressions(cfg *tacil.ControlFlowGraph, GEN, KILL map[string]adt.Set[uint]) (map[string]adt.Set[uint], map[string]adt.Set[uint]) {
	rpo := cfg.ReversePostOrder()
	rpo = rpo[1:]

	workList := adt.NewQueueFrom[*tacil.BasicBlock](rpo)

	IN := map[string]adt.Set[uint]{}
	OUT := map[string]adt.Set[uint]{}
	for _, BB := range cfg.Nodes.Elems() {
		OUT[BB.Name()] = &adt.BitVector[uint]{}
	}

	for !workList.Empty() {
		B := workList.Dequeue()

		IN[B.Name()] = &adt.BitVector[uint]{}
		for _, P := range cfg.Pred[B.Name()].Elems() {
			IN[B.Name()] = IN[B.Name()].Intersect(OUT[P.Name()])
		}

		prevOut := OUT[B.Name()]
		OUT[B.Name()] = GEN[B.Name()].Union(IN[B.Name()].Diff(KILL[B.Name()]))

		if prevOut != OUT[B.Name()] {
			for _, S := range cfg.Succ[B.Name()].Elems() {
				workList.Enqueue(S)
			}
		}
	}

	return IN, OUT
}

func AnticipatableExpressions(cfg *tacil.ControlFlowGraph) {

}

func ComputeDefUse(cfg *meer.ControlFlowGraph) (map[string]meer.Instruction, map[string]adt.Set[meer.Instruction]) {
	defs := make(map[string]meer.Instruction)
	uses := make(map[string]adt.Set[meer.Instruction])

	for _, block := range cfg.Blocks {
		for i := block.Instr().Front(); i != nil; i = i.Next() {
			switch inst := i.Value.(type) {
			case *meer.AssignInst:
				defs[inst.Dst.Name()] = inst
				for i := 1; i < inst.Value.NumOperands()+1; i++ {
					op, ok := inst.Value.Operand(i).(meer.NamedOperand)
					if !ok {
						continue
					}
					if uses[op.Name()] == nil {
						uses[op.Name()] = adt.NewHashSet[meer.Instruction]()
					}
					uses[op.Name()].Add(inst)
				}
			case *meer.JumpInst:
			case *meer.CondBrInst:
				if foo, ok := inst.Cond.(meer.NamedOperand); ok {
					if uses[foo.Name()] == nil {
						uses[foo.Name()] = adt.NewHashSet[meer.Instruction]()
					}
					uses[foo.Name()].Add(inst)
				}
			case *meer.ProcCallInstr:
				for _, arg := range inst.Args {
					if foo, ok := arg.(meer.NamedOperand); ok {
						if uses[foo.Name()] == nil {
							uses[foo.Name()] = adt.NewHashSet[meer.Instruction]()
						}
						uses[foo.Name()].Add(inst)
					}
				}
			case *meer.ReturnInst:
				if value, ok := inst.Value.(meer.NamedOperand); ok {
					if uses[value.Name()] == nil {
						uses[value.Name()] = adt.NewHashSet[meer.Instruction]()
					}
					uses[value.Name()].Add(inst)
				}
			}
		}
	}

	return defs, uses
}
