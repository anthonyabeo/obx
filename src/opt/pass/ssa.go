package pass

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/opt/analy"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

type SSA struct {
	Nom string
}

func (s SSA) Name() string { return s.Nom }

func (s SSA) Run(program *tacil.Program, symbols *tacil.SymbolTable) {
	for _, module := range program.Modules {
		for _, f := range module.GetFunctionList() {
			cfg := f.CFG()

			DF := analy.DominanceFrontier(cfg)

			Globals, Blocks := ComputeGlobalNames(cfg)
			InsertPhiFunctions(cfg, Globals, Blocks, DF, symbols)
			defs, uses := Rename(Globals, cfg)

			cfg.Defs = defs
			cfg.Uses = uses
		}
	}
}

// ComputeGlobalNames
// ---------------------------------------------------------------
func ComputeGlobalNames(cfg *tacil.ControlFlowGraph) (map[string]bool, map[string]adt.Set[*tacil.BasicBlock]) {
	Globals := map[string]bool{}
	Blocks := map[string]adt.Set[*tacil.BasicBlock]{}

	for _, BB := range cfg.Nodes.Elems() {
		VarKill := map[string]bool{}

		for i := BB.Instr().Front(); i != nil; i = i.Next() {
			if assign, ok := i.Value.(*tacil.Assign); ok {
				for i := 1; i < assign.Value.NumOperands()+1; i++ {
					operand := assign.Value.Operand(i)
					if tmp, ok := operand.(*tacil.Temp); ok && !tacil.PredeclaredRegisters[tmp.Name()] {
						if !VarKill[tmp.Name()] {
							Globals[tmp.Name()] = true
						}
					}
				}

				VarKill[assign.Dst.Name()] = true
				if _, ok := Blocks[assign.Dst.Name()]; !ok {
					Blocks[assign.Dst.Name()] = adt.NewHashSet[*tacil.BasicBlock]()
				}

				Blocks[assign.Dst.Name()].Add(BB)
			}
		}
	}

	return Globals, Blocks
}

// InsertPhiFunctions
// -----------------------------------------------------------------
func InsertPhiFunctions(
	cfg *tacil.ControlFlowGraph,
	Globals map[string]bool,
	Blocks map[string]adt.Set[*tacil.BasicBlock],
	DF map[string]adt.Set[*tacil.BasicBlock],
	symbols *tacil.SymbolTable,
) {

	for name := range Globals {
		WorkList := Blocks[name]

		for !WorkList.Empty() {
			block := WorkList.Pop()
			for _, BB := range DF[block.Name()].Elems() {

				if BB.Phi[name] == nil {
					phi := tacil.CreateEmptyPHINode()

					obj := symbols.Lookup(name)
					if obj == nil {
						panic(fmt.Sprintf("stack allocation for name '%s' not found", name))
					}

					for _, pred := range cfg.Pred[BB.Name()].Elems() {
						tmp := tacil.NewTemp(name, obj.Type())
						phi.AddIncoming(tmp, pred)
					}

					assign := tacil.CreateAssign(phi, tacil.NewTemp(name, obj.Type()))
					BB.InsertInstrBegin(assign)
					BB.Phi[name] = assign

					WorkList.Add(BB)
				}
			}
		}
	}
}

// Rename
// -----------------------------
func Rename(Globals map[string]bool, cfg *tacil.ControlFlowGraph) (map[string]tacil.Stmt, map[string]adt.Set[tacil.Stmt]) {
	counter := make(map[string]int)
	stack := make(map[string]*adt.Stack[string])
	vst := make(map[string]bool)
	defs := map[string]tacil.Stmt{}
	uses := map[string]adt.Set[tacil.Stmt]{}

	for name := range Globals {
		counter[name] = 0
		stack[name] = adt.NewStack[string]()
	}

	rename(Globals, vst, cfg.Entry, counter, stack, cfg, defs, uses)

	return defs, uses
}

func rename(
	Globals map[string]bool,
	vst map[string]bool,
	block *tacil.BasicBlock,
	counter map[string]int,
	stack map[string]*adt.Stack[string],
	cfg *tacil.ControlFlowGraph,
	defs map[string]tacil.Stmt,
	uses map[string]adt.Set[tacil.Stmt],
) {

	if vst[block.Name()] {
		return
	}
	vst[block.Name()] = true

	for name, phi := range block.Phi {
		nom := newName(name, counter, stack)
		phi.Dst.SetName(nom)

		defs[nom] = phi
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

				if uses[stack[operand.BaseName()].Top()] == nil {
					uses[stack[operand.BaseName()].Top()] = adt.NewHashSet[tacil.Stmt]()
				}
				uses[stack[operand.BaseName()].Top()].Add(assign)
			}
		}

		if Globals[assign.Dst.Name()] {
			nom := newName(assign.Dst.Name(), counter, stack)
			assign.Dst.SetName(nom)
			defs[nom] = assign
		}
	}

	// for each successor of b in the CFG do
	//		fill in φ-function parameters
	if cfg.Succ[block.Name()] != nil {
		for _, s := range cfg.Succ[block.Name()].Elems() {
			for name, assign := range s.Phi {
				if phi, ok := assign.Value.(*tacil.PHINode); ok {
					for _, inc := range phi.Incoming {
						if inc.Blk == block {
							inc.V.SetName(stack[name].Top())
							if uses[stack[name].Top()] == nil {
								uses[stack[name].Top()] = adt.NewHashSet[tacil.Stmt]()
							}
							uses[stack[name].Top()].Add(assign)
						}
					}
				}
			}
		}

		for _, s := range cfg.Succ[block.Name()].Elems() {
			rename(Globals, vst, s, counter, stack, cfg, defs, uses)
		}
	}

	for i := block.Instr().Front(); i != nil; i = i.Next() {
		switch instr := i.Value.(type) {
		case *tacil.Assign:
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
