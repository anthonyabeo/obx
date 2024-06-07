package opt

import (
	"github.com/anthonyabeo/obx/src/translate/ir"
)

// WorkList
// ----------------------------------------------
type WorkList struct {
	queue []string
}

func NewWorkList(elems []string) *WorkList {
	return &WorkList{queue: elems}
}

func (w *WorkList) Add(elem string) {
	w.queue = append(w.queue, elem)
}

func (w *WorkList) Dequeue() string {
	elem := w.queue[0]
	w.queue = w.queue[1:]

	return elem
}

func (w *WorkList) Empty() bool {
	return len(w.queue) == 0
}

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

	workList := NewWorkList(rpo)

	DFIn := map[string]ir.BitVector{"entry": Init}
	for name := range cfg.Nodes {
		if name != "entry" {
			DFIn[name] = ir.BitVector(255)
		}
	}

	for !workList.Empty() {
		B := workList.Dequeue()

		TotalEffect := ir.BitVector(0)
		for _, name := range cfg.Pred[B] {
			in := DFIn[name]
			Effect := F[name](in)
			TotalEffect |= Effect
		}

		if DFIn[B] != TotalEffect {
			DFIn[B] = TotalEffect
			for _, succ := range cfg.Succ[B] {
				workList.Add(succ)
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

	workList := NewWorkList(rpo)

	IN := map[string]ir.BitVector{}
	OUT := map[string]ir.BitVector{}
	for name := range cfg.Nodes {
		OUT[name] = ir.BitVector(0)
	}

	for !workList.Empty() {
		B := workList.Dequeue()

		IN[B] = ir.BitVector(0)
		for _, name := range cfg.Pred[B] {
			IN[B] |= OUT[name]
		}

		prevOut := OUT[B]
		OUT[B] = GEN[B] | (IN[B] & ^KILL[B])

		if prevOut != OUT[B] {
			for _, succ := range cfg.Succ[B] {
				workList.Add(succ)
			}
		}
	}

	return IN, OUT
}
