package pass

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/meer"
	"github.com/anthonyabeo/obx/src/opt/analy"
)

type SSA struct {
	Nom string
}

func (s SSA) Name() string { return s.Nom }

func (s SSA) Run(program *meer.Program) {
	for _, unit := range program.Units {
		DF := analy.DominanceFrontier(unit.CFG)

		Globals, Blocks := ComputeGlobalNames(unit.CFG)
		InsertPhiFunctions(unit.CFG, Globals, Blocks, DF)
		Rename(Globals, unit.CFG)
	}
}

// ComputeGlobalNames
// ---------------------------------------------------------------
func ComputeGlobalNames(cfg *meer.ControlFlowGraph) (map[string]meer.Type, map[string]adt.Set[meer.BasicBlockID]) {
	Globals := map[string]meer.Type{}
	Blocks := map[string]adt.Set[meer.BasicBlockID]{}

	for _, BB := range cfg.Nodes.Elems() {
		VarKill := map[string]bool{}

		for i := BB.Instr().Front(); i != nil; i = i.Next() {
			if assign, ok := i.Value.(*meer.AssignInst); ok {
				for i := 1; i < assign.Value.NumOperands()+1; i++ {
					operand := assign.Value.Operand(i)
					if tmp, ok := operand.(*meer.Ident); ok {
						if !VarKill[tmp.Id] {
							Globals[tmp.Id] = tmp.Ty
						}
					}
				}

				VarKill[assign.Dst.Name()] = true
				if _, ok := Blocks[assign.Dst.Name()]; !ok {
					Blocks[assign.Dst.Name()] = adt.NewHashSet[meer.BasicBlockID]()
				}

				Blocks[assign.Dst.Name()].Add(BB.ID())
			}
		}
	}

	return Globals, Blocks
}

// InsertPhiFunctions
// -----------------------------------------------------------------
func InsertPhiFunctions(
	cfg *meer.ControlFlowGraph,
	Globals map[string]meer.Type,
	Blocks map[string]adt.Set[meer.BasicBlockID],
	DF map[meer.BasicBlockID]adt.Set[*meer.BasicBlock],
) {

	for name, ty := range Globals {
		WorkList := Blocks[name]

		for !WorkList.Empty() {
			block := WorkList.Pop()
			for _, BB := range DF[block].Elems() {

				if BB.Phi[name] == nil {
					phi := meer.CreateEmptyPHINode()

					for _, pred := range cfg.Pred[BB.ID()].Elems() {
						tmp := meer.CreateIdent(name, ty)
						phi.AddIncoming(tmp, cfg.Blocks[pred])
					}

					assign := meer.CreateAssign(phi, meer.CreateIdent(name, ty))
					BB.InsertInstrBegin(assign)
					BB.Phi[name] = assign

					WorkList.Add(BB.ID())
				}
			}
		}
	}
}

// Rename
// -----------------------------
func Rename(Globals map[string]meer.Type, cfg *meer.ControlFlowGraph) {
	counter := make(map[string]int)
	stack := make(map[string]*adt.Stack[string])
	vst := make(map[meer.BasicBlockID]bool)

	for name := range Globals {
		counter[name] = 0
		stack[name] = adt.NewStack[string]()
	}

	rename(Globals, vst, cfg.Entry.ID(), counter, stack, cfg /*defs, uses*/)
}

func rename(
	Globals map[string]meer.Type,
	vst map[meer.BasicBlockID]bool,
	block meer.BasicBlockID,
	counter map[string]int,
	stack map[string]*adt.Stack[string],
	cfg *meer.ControlFlowGraph,
) {

	if vst[block] {
		return
	}
	vst[block] = true

	for name, phi := range cfg.Blocks[block].Phi {
		nom := newName(name, counter, stack)
		phi.Dst.SetName(nom)
	}

	for i := cfg.Blocks[block].Instr().Front(); i != nil; i = i.Next() {
		assign, ok := i.Value.(*meer.AssignInst)
		if !ok {
			continue
		}

		for j := 1; j < assign.Value.NumOperands()+1; j++ {
			operand := assign.Value.Operand(j)
			if op, ok := operand.(meer.NamedOperand); ok {
				if Globals[op.Name()] != nil {
					op.SetName(stack[op.Name()].Top())
				}
			}
		}

		if Globals[assign.Dst.Name()] != nil {
			nom := newName(assign.Dst.Name(), counter, stack)
			assign.Dst.SetName(nom)
		}
	}

	// for each successor of b in the CFG do
	//		fill in φ-function parameters
	if cfg.Suc[block] != nil {
		for _, s := range cfg.Suc[block].Elems() {
			for name, assign := range cfg.Blocks[s].Phi {
				if phi, ok := assign.Value.(*meer.PHINode); ok {
					for _, inc := range phi.Incoming {
						if inc.Blk.ID() == block {
							inc.V.SetName(stack[name].Top())
						}
					}
				}
			}
		}

		for _, s := range cfg.Suc[block].Elems() {
			rename(Globals, vst, s, counter, stack, cfg)
		}
	}

	for i := cfg.Blocks[block].Instr().Front(); i != nil; i = i.Next() {
		switch instr := i.Value.(type) {
		case *meer.AssignInst:
			if stack[instr.Dst.BaseName()] != nil {
				stack[instr.Dst.BaseName()].Pop()
			}
		default:
			continue
		}
	}

	return
}

func newName(name string, counter map[string]int, stack map[string]*adt.Stack[string]) string {
	i := counter[name]
	counter[name] += 1

	nom := fmt.Sprintf("%s%d", name, i)
	stack[name].Push(nom)

	return nom
}
