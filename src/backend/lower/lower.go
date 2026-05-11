package lower

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
	selector "github.com/anthonyabeo/obx/src/backend/select"
	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/minir"
)

// LoweredProgram is the result of lowering minir into backend MIR and then
// deriving target-specific lowering plans from that MIR.
type LoweredProgram struct {
	MIR   *mir.Program
	Plans map[string]*selector.FunctionPlans
}

// LowerProgram lowers a minir.Program into backend MIR.
func LowerProgram(prog *minir.Program) (*mir.Program, error) {
	if prog == nil {
		return mir.NewProgram(), nil
	}
	out := mir.NewProgram()
	for _, mod := range prog.Modules {
		m, err := LowerModule(mod)
		if err != nil {
			return nil, err
		}
		out.AddModule(m)
	}
	return out, nil
}

// LowerModule lowers a minir.Module into backend MIR.
func LowerModule(mod *minir.Module) (*mir.Module, error) {
	if mod == nil {
		return nil, fmt.Errorf("lower module: nil module")
	}
	out := mir.NewModule(mod.Name)
	globalsByName := make(map[string]*mir.Symbol)

	for _, g := range mod.Globals {
		decl, sym, err := lowerGlobalVar(g)
		if err != nil {
			return nil, fmt.Errorf("lower global %q: %w", g.Name, err)
		}
		out.AddGlobal(decl)
		globalsByName[g.Name] = sym
	}
	for _, c := range mod.Constants {
		decl, sym, err := lowerGlobalConst(c)
		if err != nil {
			return nil, fmt.Errorf("lower const %q: %w", c.Name, err)
		}
		out.AddConst(decl)
		globalsByName[c.Name] = sym
	}
	for _, e := range mod.Externals {
		decl, err := lowerExtern(e)
		if err != nil {
			return nil, fmt.Errorf("lower extern %q: %w", e.Name, err)
		}
		out.AddExtern(decl)
	}
	for _, fn := range mod.Functions {
		mfn, err := LowerFunction(fn, globalsByName)
		if err != nil {
			return nil, fmt.Errorf("lower function %q: %w", fn.FnName, err)
		}
		out.AddFunction(mfn)
	}
	return out, nil
}

// LowerFunction lowers a minir.Function into backend MIR.
func LowerFunction(fn *minir.Function, globals map[string]*mir.Symbol) (*mir.Function, error) {
	if fn == nil {
		return nil, fmt.Errorf("lower function: nil function")
	}
	resultTy, err := lowerType(fn.Result)
	if err != nil {
		return nil, fmt.Errorf("function %s result: %w", fn.FnName, err)
	}
	out := mir.NewFunction(fn.FnName, resultTy)
	regByTemp := make(map[*minir.Temp]*mir.Register)
	blockByLabel := make(map[string]*mir.Block)

	for i, p := range fn.Params {
		if p == nil {
			return nil, fmt.Errorf("function %s has nil parameter %d", fn.FnName, i)
		}
		ty, err := lowerType(p.Type())
		if err != nil {
			return nil, fmt.Errorf("parameter %s: %w", p.String(), err)
		}
		out.AddParam(&mir.Param{Name: lowerTempName(p), Type: ty, Kind: lowerParamKind(fn.ParamKinds, i)})
		regByTemp[p] = mir.NewRegister(lowerTempName(p), mir.VirtualReg, ty)
	}

	blocks := fn.SortedBlocks()
	for _, b := range blocks {
		mb := mir.NewBlock(b.ID, b.Label)
		out.AddBlock(mb)
		blockByLabel[b.Label] = mb
		if fn.Entry != nil && b.ID == fn.Entry.ID {
			out.SetEntry(mb)
		}
		if fn.Exit != nil && b.ID == fn.Exit.ID {
			out.SetExit(mb)
		}
	}

	for _, b := range blocks {
		mb := blockByLabel[b.Label]
		if mb == nil {
			return nil, fmt.Errorf("function %s: missing lowered block for %s", fn.FnName, b.Label)
		}
		for _, inst := range b.Instrs {
			if term, ok := inst.(minir.Terminator); ok {
				lowered, err := lowerTerminator(term, regByTemp, globals)
				if err != nil {
					return nil, fmt.Errorf("block %s terminator: %w", b.Label, err)
				}
				mb.SetTerminator(lowered)
				continue
			}
			lowered, err := lowerInstr(inst, regByTemp, globals)
			if err != nil {
				return nil, fmt.Errorf("block %s instruction: %w", b.Label, err)
			}
			mb.AddInstr(lowered)
		}
		if mb.Term == nil && b.Term != nil {
			lowered, err := lowerTerminator(b.Term, regByTemp, globals)
			if err != nil {
				return nil, fmt.Errorf("block %s terminator: %w", b.Label, err)
			}
			mb.SetTerminator(lowered)
		}
	}

	for _, b := range blocks {
		mb := blockByLabel[b.Label]
		for _, succ := range b.SortedSuccs() {
			msucc := blockByLabel[succ.Label]
			if msucc == nil {
				return nil, fmt.Errorf("function %s: unknown successor %s", fn.FnName, succ.Label)
			}
			mb.AddSucc(msucc)
			msucc.AddPred(mb)
		}
	}

	return out, nil
}

func lowerInstr(inst minir.Instr, regByTemp map[*minir.Temp]*mir.Register, globals map[string]*mir.Symbol) (mir.Instr, error) {
	switch i := inst.(type) {
	case *minir.BinaryInst:
		left, err := lowerOperand(i.Left, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		right, err := lowerOperand(i.Right, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		dst, err := lowerTemp(i.Dst, regByTemp)
		if err != nil {
			return nil, err
		}
		return &mir.BinaryInstr{Dst: dst, Op: i.Op, Left: left, Right: right}, nil
	case *minir.UnaryInst:
		x, err := lowerOperand(i.Src, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		dst, err := lowerTemp(i.Dst, regByTemp)
		if err != nil {
			return nil, err
		}
		return &mir.UnaryInstr{Dst: dst, Op: i.Op, X: x}, nil
	case *minir.ICmpInst:
		left, err := lowerOperand(i.Left, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		right, err := lowerOperand(i.Right, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		dst, err := lowerTemp(i.Dst, regByTemp)
		if err != nil {
			return nil, err
		}
		return &mir.CompareInstr{Dst: dst, Pred: strings.ToLower(i.Pred), Left: left, Right: right}, nil
	case *minir.LoadInst:
		addr, err := lowerOperand(i.Addr, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		dst, err := lowerTemp(i.Dst, regByTemp)
		if err != nil {
			return nil, err
		}
		return &mir.LoadInstr{Dst: dst, Addr: addr}, nil
	case *minir.StoreInst:
		addr, err := lowerOperand(i.Addr, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		val, err := lowerOperand(i.Val, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		return &mir.StoreInstr{Addr: addr, Value: val}, nil
	case *minir.CallInst:
		args := make([]mir.Operand, 0, len(i.Args))
		for _, arg := range i.Args {
			lop, err := lowerOperand(arg, regByTemp, globals)
			if err != nil {
				return nil, err
			}
			args = append(args, lop)
		}
		var dst *mir.Register
		if i.Dst != nil {
			callDst, callErr := lowerTemp(i.Dst, regByTemp)
			if callErr != nil {
				return nil, callErr
			}
			dst = callDst
		}
		return &mir.CallInstr{Dst: dst, Callee: mir.NewSymbol(i.Callee, nil), Args: args}, nil
	case *minir.PhiInst:
		dst, err := lowerTemp(i.Dst, regByTemp)
		if err != nil {
			return nil, err
		}
		arms := make([]mir.PhiArm, 0, len(i.Args))
		for _, arm := range i.Args {
			val, err := lowerOperand(arm.Val, regByTemp, globals)
			if err != nil {
				return nil, err
			}
			arms = append(arms, mir.PhiArm{BlockLabel: arm.BlockLabel, Value: val})
		}
		return &mir.PhiInstr{Dst: dst, Arms: arms}, nil
	case *minir.GEPInst:
		return nil, fmt.Errorf("gep lowering not implemented in first bridge pass")
	case *minir.AllocaInst:
		return nil, fmt.Errorf("alloca lowering not implemented in first bridge pass")
	case *minir.CastInst:
		return nil, fmt.Errorf("cast lowering not implemented in first bridge pass")
	case *minir.FCmpInst:
		return nil, fmt.Errorf("floating-point comparison lowering not implemented in first bridge pass")
	default:
		return nil, fmt.Errorf("unsupported instruction type %T", inst)
	}
}

func lowerTerminator(term minir.Terminator, regByTemp map[*minir.Temp]*mir.Register, globals map[string]*mir.Symbol) (mir.Terminator, error) {
	switch t := term.(type) {
	case *minir.JumpInst:
		return &mir.JumpInstr{Target: t.Target}, nil
	case *minir.CondBrInst:
		cond, err := lowerTemp(t.Cond, regByTemp)
		if err != nil {
			return nil, err
		}
		return &mir.CondBrInstr{Cond: cond, TrueLabel: t.TrueLabel, FalseLabel: t.FalseLabel}, nil
	case *minir.ReturnInst:
		if t.Result == nil {
			return &mir.ReturnInstr{}, nil
		}
		val, err := lowerTemp(t.Result, regByTemp)
		if err != nil {
			return nil, err
		}
		return &mir.ReturnInstr{Value: val}, nil
	case *minir.HaltInst:
		if t.Code == nil {
			return &mir.HaltInstr{}, nil
		}
		val, err := lowerOperand(t.Code, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		return &mir.HaltInstr{Code: val}, nil
	case *minir.SwitchInst:
		key, err := lowerTemp(t.Key, regByTemp)
		if err != nil {
			return nil, err
		}
		arms := make([]mir.SwitchArm, 0, len(t.Arms))
		for _, arm := range t.Arms {
			arms = append(arms, mir.SwitchArm{Value: mir.NewImmediate(arm.Val, lowerTempType(t.Key)), Label: arm.Label})
		}
		return &mir.SwitchInstr{Value: key, Default: t.Default, Arms: arms}, nil
	default:
		return nil, fmt.Errorf("unsupported terminator type %T", term)
	}
}

func lowerOperand(v minir.Value, regByTemp map[*minir.Temp]*mir.Register, globals map[string]*mir.Symbol) (mir.Operand, error) {
	switch x := v.(type) {
	case *minir.Temp:
		return lowerTemp(x, regByTemp)
	case *minir.Constant:
		return lowerConstant(x)
	case *minir.GlobalRef:
		if sym, ok := globals[x.GlobalName]; ok {
			return sym, nil
		}
		return mir.NewSymbol(x.GlobalName, lowerPointerElemType(x.Ty)), nil
	default:
		return nil, fmt.Errorf("unsupported operand type %T", v)
	}
}

func lowerTemp(t *minir.Temp, regByTemp map[*minir.Temp]*mir.Register) (*mir.Register, error) {
	if t == nil {
		return nil, fmt.Errorf("nil temp")
	}
	if r, ok := regByTemp[t]; ok {
		return r, nil
	}
	ty, err := lowerType(t.Type())
	if err != nil {
		return nil, fmt.Errorf("temp %s: %w", t.String(), err)
	}
	r := mir.NewRegister(lowerTempName(t), mir.VirtualReg, ty)
	regByTemp[t] = r
	return r, nil
}

func lowerConstant(c *minir.Constant) (*mir.Immediate, error) {
	if c == nil {
		return nil, fmt.Errorf("nil constant")
	}
	ty, err := lowerType(c.Type())
	if err != nil {
		return nil, err
	}
	return mir.NewImmediate(c.Val, ty), nil
}

func lowerGlobalVar(g *minir.GlobalVar) (*mir.GlobalDecl, *mir.Symbol, error) {
	if g == nil {
		return nil, nil, fmt.Errorf("nil global")
	}
	ty, err := lowerType(g.Ty)
	if err != nil {
		return nil, nil, err
	}
	decl := mir.NewGlobalDecl(g.Name, ty, lowerLinkage(g.Linkage), nil)
	if g.Init != nil {
		init, err := lowerConstant(g.Init)
		if err != nil {
			return nil, nil, err
		}
		decl.Init = init
	}
	return decl, mir.NewSymbol(g.Name, ty), nil
}

func lowerGlobalConst(g *minir.GlobalConst) (*mir.ConstDecl, *mir.Symbol, error) {
	if g == nil {
		return nil, nil, fmt.Errorf("nil const")
	}
	ty, err := lowerType(g.Ty)
	if err != nil {
		return nil, nil, err
	}
	init, err := lowerConstant(g.Init)
	if err != nil {
		return nil, nil, err
	}
	decl := mir.NewConstDecl(g.Name, ty, init, lowerLinkage(g.Linkage))
	return decl, mir.NewSymbol(g.Name, ty), nil
}

func lowerExtern(e *minir.ExternalFunc) (*mir.ExternDecl, error) {
	if e == nil {
		return nil, fmt.Errorf("nil extern")
	}
	decl := mir.NewExternDecl(e.Name, nil)
	decl.Linkage = lowerLinkage(e.Linkage)
	if e.Attrs != nil {
		decl.DLLName = e.Attrs.DLLName
		decl.Variadic = e.Attrs.Variadic
		decl.CallConv = e.Attrs.CallConv
	}
	return decl, nil
}

func lowerType(ty minir.Type) (*mir.Type, error) {
	switch t := ty.(type) {
	case nil:
		return nil, nil
	case *minir.PrimitiveType:
		switch strings.ToLower(t.String()) {
		case "i1":
			return mir.NewScalarType("i1", 1), nil
		case "i32":
			return mir.NewScalarType("i32", 4), nil
		case "i64":
			return mir.NewScalarType("i64", 8), nil
		default:
			return nil, fmt.Errorf("unsupported primitive type %s", t.String())
		}
	case *minir.PointerType:
		elem, err := lowerType(t.Elem)
		if err != nil {
			return nil, err
		}
		return mir.NewPointerType(elem, 8), nil
	case *minir.ArrayType:
		return nil, fmt.Errorf("array types are not supported in the first bridge pass")
	case *minir.RecordType:
		return nil, fmt.Errorf("record types are not supported in the first bridge pass")
	case *minir.FunctionType:
		return nil, fmt.Errorf("function types are not supported in the first bridge pass")
	default:
		return nil, fmt.Errorf("unsupported type %T", ty)
	}
}

func lowerPointerElemType(ty *minir.PointerType) *mir.Type {
	if ty == nil {
		return nil
	}
	elem, _ := lowerType(ty.Elem)
	return elem
}

func lowerTempType(t *minir.Temp) *mir.Type {
	if t == nil {
		return nil
	}
	res, _ := lowerType(t.Type())
	return res
}

func lowerParamKind(kinds []desugar.ParamKind, idx int) string {
	if idx < 0 || idx >= len(kinds) {
		return "value"
	}
	switch kinds[idx] {
	case desugar.VarParam:
		return "var"
	case desugar.InParam:
		return "in"
	default:
		return "value"
	}
}

func lowerTempName(t *minir.Temp) string {
	if t == nil {
		return ""
	}
	if n := t.Name(); n != "" {
		return n
	}
	return fmt.Sprintf("t%d", t.ID)
}

func lowerLinkage(l minir.Linkage) mir.Linkage {
	switch l {
	case minir.ExternalLinkage:
		return mir.ExternalLinkage
	case minir.PrivateLinkage:
		return mir.PrivateLinkage
	default:
		return mir.InternalLinkage
	}
}
