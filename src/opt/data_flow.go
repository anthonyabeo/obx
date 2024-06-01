package opt

import (
	"github.com/anthonyabeo/obx/src/translate/ir"
)

// WorkList
// ----------------------------------------------
type WorkList struct {
	queue []string
	set   map[string]bool
}

func NewWorkList(elems []string) *WorkList {
	set := map[string]bool{}
	for _, elem := range elems {
		set[elem] = true
	}

	return &WorkList{queue: elems, set: set}
}

func (w *WorkList) Add(elem string) {
	if _, exist := w.set[elem]; !exist {
		w.set[elem] = true
		w.queue = append(w.queue, elem)
	}
}

func (w *WorkList) Dequeue() string {
	elem := w.queue[0]
	w.queue = w.queue[1:]

	delete(w.set, elem)
	return elem
}

func (w *WorkList) Empty() bool {
	return len(w.queue) == 0
}

// IterativeDataFlow
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
