package analy

import (
	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

// IterativeDataFlow implements an iterative data-flow algorithm for the
// reaching definitions problem.
//
// It used a lattice of 8-bit-wide-vectors. It takes in as input, 'Init' the
// initial value of the 'entry' node and F: BV^8 -> BV^8, a set of flow functions
// as explained in Muchnick 8.4
// ----------------------------------------------
func IterativeDataFlow(cfg *ir.ControlFlowGraph, F map[string]func(ir.BitVector) ir.BitVector, Init ir.BitVector) map[string]ir.BitVector {
	rpo := cfg.ReversePostOrder()
	rpo = rpo[1:]

	workList := adt.NewQueueFrom[*ir.BasicBlock](rpo)

	DFIn := map[string]ir.BitVector{"entry": Init}
	for _, BB := range cfg.Nodes.Elems() {
		if BB.Name() != "entry" {
			DFIn[BB.Name()] = ir.BitVector(255)
		}
	}

	for !workList.Empty() {
		B := workList.Dequeue()

		TotalEffect := ir.BitVector(0)
		for _, name := range cfg.Pred[B.Name()] {
			in := DFIn[name.Name()]
			Effect := F[name.Name()](in)
			TotalEffect |= Effect
		}

		if DFIn[B.Name()] != TotalEffect {
			DFIn[B.Name()] = TotalEffect
			for _, succ := range cfg.Succ[B.Name()] {
				workList.Enqueue(succ)
			}
		}
	}

	return DFIn
}

// IterativeDataflowDragonBook an iterative data-flow algorithm for the reaching definitions problem.
// as defined at https://en.wikipedia.org/wiki/Reaching_definition
func IterativeDataflowDragonBook(cfg *ir.ControlFlowGraph, GEN, KILL map[string]ir.BitVector) (map[string]ir.BitVector, map[string]ir.BitVector) {
	rpo := cfg.ReversePostOrder()
	rpo = rpo[1:]

	workList := adt.NewQueueFrom[*ir.BasicBlock](rpo)

	IN := map[string]ir.BitVector{}
	OUT := map[string]ir.BitVector{}
	for _, BB := range cfg.Nodes.Elems() {
		OUT[BB.Name()] = ir.BitVector(0)
	}

	for !workList.Empty() {
		B := workList.Dequeue()

		IN[B.Name()] = ir.BitVector(0)
		for _, name := range cfg.Pred[B.Name()] {
			IN[B.Name()] |= OUT[name.Name()]
		}

		prevOut := OUT[B.Name()]
		OUT[B.Name()] = GEN[B.Name()] | (IN[B.Name()] & ^KILL[B.Name()])

		if prevOut != OUT[B.Name()] {
			for _, succ := range cfg.Succ[B.Name()] {
				workList.Enqueue(succ)
			}
		}
	}

	return IN, OUT
}

func ReachingDefinition(cfg *ir.ControlFlowGraph, GEN, KILL map[string]adt.Set[uint]) (map[string]adt.Set[uint], map[string]adt.Set[uint]) {
	rpo := cfg.ReversePostOrder()
	rpo = rpo[1:]

	workList := adt.NewQueueFrom[*ir.BasicBlock](rpo)

	IN := map[string]adt.Set[uint]{}
	OUT := map[string]adt.Set[uint]{}
	for _, BB := range cfg.Nodes.Elems() {
		OUT[BB.Name()] = &adt.BitVector[uint]{}
	}

	for !workList.Empty() {
		B := workList.Dequeue()

		IN[B.Name()] = &adt.BitVector[uint]{}
		for _, name := range cfg.Pred[B.Name()] {
			IN[B.Name()] = IN[B.Name()].Union(OUT[name.Name()])
		}

		prevOut := OUT[B.Name()]
		diff := IN[B.Name()].Diff(KILL[B.Name()])
		OUT[B.Name()] = GEN[B.Name()].Union(diff)

		if prevOut != OUT[B.Name()] {
			for _, succ := range cfg.Succ[B.Name()] {
				workList.Enqueue(succ)
			}
		}
	}

	return IN, OUT
}

func LiveVariable(cfg *ir.ControlFlowGraph) {

}

func AvailableExpressions(cfg *ir.ControlFlowGraph) {

}

func AnticipableExpressions(cfg *ir.ControlFlowGraph) {

}
