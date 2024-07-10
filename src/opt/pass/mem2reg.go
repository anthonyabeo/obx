package pass

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/opt/analy"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

type Mem2Reg struct {
	Nom string
}

func (m Mem2Reg) Name() string { return m.Nom }

func (m Mem2Reg) Run(program *tacil.Program) {
	for _, module := range program.Modules {
		for _, f := range module.GetFunctionList() {
			cfg := f.CFG()

			DF := analy.DominanceFrontier(cfg)

			Globals, Blocks := ComputeGlobalNames(cfg)
			InsertPhiFunctions(Globals, Blocks, DF)
			Rename(Globals, cfg)
		}
	}
}

// ComputeGlobalNames
// ---------------------------------------------------------------
func ComputeGlobalNames(cfg *tacil.ControlFlowGraph) (map[string]bool, map[string]adt.Set[Pair]) {
	Globals := map[string]bool{}
	Blocks := map[string]adt.Set[Pair]{}

	for _, BB := range cfg.Nodes.Elems() {
		VarKill := map[string]bool{}

		for i := BB.Instr().Front(); i != nil; i = i.Next() {
			if assign, ok := i.Value.(*tacil.Assign); ok {
				for i := 1; i < assign.Value.NumOperands()+1; i++ {
					operand := assign.Value.Operand(i)
					if !operand.HasName() {
						continue
					}

					if !VarKill[operand.Name()] {
						Globals[operand.Name()] = true
					}
				}

				VarKill[assign.Dst.Name()] = true
				if _, ok := Blocks[assign.Dst.Name()]; !ok {
					Blocks[assign.Dst.Name()] = adt.NewHashSet[Pair]()
				}

				Blocks[assign.Dst.Name()].Add(Pair{
					expr:  assign.Dst,
					block: BB,
				})
			}
		}
	}

	return Globals, Blocks
}

// Pair
// ---------------------------
type Pair struct {
	expr  tacil.Expr
	block *tacil.BasicBlock
}

// InsertPhiFunctions
// -----------------------------------------------------------------
func InsertPhiFunctions(Globals map[string]bool, Blocks map[string]adt.Set[Pair], DF map[string]adt.Set[*tacil.BasicBlock]) {
	for name := range Globals {
		WorkList := Blocks[name]

		workList := adt.NewQueue[*tacil.BasicBlock]()
		for _, inc := range WorkList.Elems() {
			workList.Enqueue(inc.block)
		}

		for !workList.Empty() {
			inc := WorkList.Pop()
			blk := workList.Dequeue()
			for _, BB := range DF[blk.Name()].Elems() {
				var phi *tacil.PHINode
				if BB.Phi[inc.expr.Name()] == nil {
					phi = tacil.CreateEmptyPHINode()
					phi.AddIncoming(inc.expr, inc.block)

					assign := tacil.CreateAssign(phi, tacil.NewTemp(inc.expr.Name(), inc.expr.Type()))
					BB.InsertInstrBegin(assign)
					BB.Phi[inc.expr.Name()] = assign

					workList.Enqueue(BB)
				} else {
					BB.Phi[inc.expr.Name()].Value.(*tacil.PHINode).AddIncoming(inc.expr, inc.block)
				}
			}
		}
	}
}

// Rename
// -----------------------------
func Rename(Globals map[string]bool, cfg *tacil.ControlFlowGraph) {
	counter := make(map[string]int)
	stack := make(map[string]*adt.Stack[string])
	vst := make(map[string]bool)

	for name := range Globals {
		counter[name] = 0
		stack[name] = adt.NewStack[string]()
	}

	rename(Globals, vst, cfg.Entry, counter, stack, cfg)
}

func rename(Globals map[string]bool, vst map[string]bool, block *tacil.BasicBlock, counter map[string]int, stack map[string]*adt.Stack[string], cfg *tacil.ControlFlowGraph) {
	if vst[block.Name()] {
		return
	}
	vst[block.Name()] = true

	for name, phi := range block.Phi {
		nom := newName(name, counter, stack)
		phi.Dst.SetName(nom)
	}

	for i := block.Instr().Front(); i != nil; i = i.Next() {
		assign, ok := i.Value.(*tacil.Assign)
		if !ok {
			continue
		}

		for j := 1; j < assign.Value.NumOperands()+1; j++ {
			operand := assign.Value.Operand(j)
			if Globals[operand.Name()] {
				operand.SetName(stack[operand.Name()].Top())
			}
		}

		if Globals[assign.Dst.Name()] {
			assign.Dst.SetName(newName(assign.Dst.Name(), counter, stack))
		}
	}

	// for each successor of b in the CFG do
	//		fill in φ-function parameters
	if cfg.Succ[block.Name()] != nil {
		for _, s := range cfg.Succ[block.Name()].Elems() {
			for name, assign := range s.Phi {
				if phi, ok := assign.Dst.(*tacil.PHINode); ok {
					for _, inc := range phi.Incoming {
						if inc.Blk == block {
							inc.V.SetName(stack[name].Top())
						}
					}
				}
			}
		}
	}

	if cfg.Succ[block.Name()] != nil {
		for _, s := range cfg.Succ[block.Name()].Elems() {
			rename(Globals, vst, s, counter, stack, cfg)
		}
	}

	for i := block.Instr().Front(); i != nil; i = i.Next() {
		switch instr := i.Value.(type) {
		case *tacil.Assign:
			if stack[instr.Dst.Name()] != nil {
				stack[instr.Dst.Name()].Pop()
			}

		case *tacil.PHINode:
			if stack[instr.Name()] != nil {
				stack[instr.Name()].Pop()
			}
		default:
			continue
		}
	}
}

func newName(name string, counter map[string]int, stack map[string]*adt.Stack[string]) string {
	i := counter[name]
	counter[name] += 1

	nom := fmt.Sprintf("%s%d", name, i)
	stack[name].Push(nom)

	return nom
}
