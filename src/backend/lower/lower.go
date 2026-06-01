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
	out.IsEntry = mod.IsEntry
	out.DLLName = mod.DLLName
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
		return lowerGEP(i, regByTemp, globals)
	case *minir.AllocaInst:
		if i.Dst == nil {
			return nil, fmt.Errorf("alloca: destination temp is nil")
		}
		// Alloca instructions survive to the register allocation stage where they are
		// assigned frame slots. Here we just lower the minir alloca to backend alloca.
		dst, err := lowerTemp(i.Dst, regByTemp)
		if err != nil {
			return nil, err
		}

		// Compute the size of the allocated type
		allocType, err := lowerType(i.AllocType)
		if err != nil {
			return nil, fmt.Errorf("alloca type: %w", err)
		}

		allocSize := 8 // default word size
		if allocType != nil && allocType.Size > 0 {
			allocSize = allocType.Size
		}

		return &mir.AllocaInstr{Dst: dst, Size: allocSize}, nil
	case *minir.AddrInstr:
		of, err := lowerOperand(i.Of, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		dst, err := lowerTemp(i.Dst, regByTemp)
		if err != nil {
			return nil, err
		}
		return &mir.UnaryInstr{Dst: dst, Op: "addr", X: of}, nil
	case *minir.CastInst:
		src, err := lowerOperand(i.Src, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		dst, err := lowerTemp(i.Dst, regByTemp)
		if err != nil {
			return nil, err
		}
		return &mir.UnaryInstr{Dst: dst, Op: i.Op, X: src}, nil
	case *minir.FCmpInst:
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
	default:
		return nil, fmt.Errorf("unsupported instruction type %T", inst)
	}
}

func lowerTerminator(term minir.Terminator, regByTemp map[*minir.Temp]*mir.Register, globals map[string]*mir.Symbol) (mir.Terminator, error) {
	switch t := term.(type) {
	case *minir.JumpInst:
		return &mir.JumpInstr{Target: t.Target}, nil
	case *minir.CondBrInst:
		cond, err := lowerOperand(t.Cond, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		return &mir.CondBrInstr{Cond: cond, TrueLabel: t.TrueLabel, FalseLabel: t.FalseLabel}, nil
	case *minir.ReturnInst:
		if t.Result == nil {
			return &mir.ReturnInstr{}, nil
		}
		val, err := lowerOperand(t.Result, regByTemp, globals)
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
		key, err := lowerOperand(t.Key, regByTemp, globals)
		if err != nil {
			return nil, err
		}
		arms := make([]mir.SwitchArm, 0, len(t.Arms))
		for _, arm := range t.Arms {
			ty, err := lowerType(t.Key.Type())
			if err != nil {
				return nil, err
			}
			arms = append(arms, mir.SwitchArm{Value: mir.NewImmediate(arm.Val, ty), Label: arm.Label})
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
	case minir.Constant:
		return lowerConstant(x)
	case *minir.GlobalRef:
		if sym, ok := globals[x.GlobalName]; ok {
			return sym, nil
		}
		ty, _ := lowerType(x.Ty)
		return mir.NewSymbol(x.GlobalName, ty), nil
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

func lowerConstant(c minir.Constant) (*mir.Immediate, error) {
	if c == nil {
		return nil, fmt.Errorf("nil constant")
	}
	ty, err := lowerType(c.Type())
	if err != nil {
		return nil, err
	}
	// Extract a concrete Go value suitable for mir.Immediate.
	switch x := c.(type) {
	case *minir.IntegerConst:
		if x.Signed {
			return mir.NewImmediate(int64(x.Value), ty), nil
		}
		return mir.NewImmediate(x.Value, ty), nil
	case *minir.FloatConst:
		return mir.NewImmediate(x.Value, ty), nil
	case *minir.StringConst:
		return mir.NewImmediate(x.Value, ty), nil
	case *minir.NilConst:
		return mir.NewImmediate(int64(0), ty), nil
	case *minir.AggregateConst:
		return mir.NewImmediate(x.Val, ty), nil
	default:
		return mir.NewImmediate(int64(0), ty), nil
	}
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
	decl.IsExternRef = g.IsExternRef
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
	if e.Sig != nil {
		decl.FixedArgCount = len(e.Sig.Params)
	}
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
		case "i8":
			return mir.NewScalarType("i8", 1), nil
		case "i16":
			return mir.NewScalarType("i16", 2), nil
		case "i32":
			return mir.NewScalarType("i32", 4), nil
		case "i64":
			return mir.NewScalarType("i64", 8), nil
		case "u8":
			return mir.NewScalarType("u8", 1), nil
		case "u16":
			return mir.NewScalarType("u16", 2), nil
		case "u32":
			return mir.NewScalarType("u32", 4), nil
		case "u64":
			return mir.NewScalarType("u64", 8), nil
		case "f32":
			return mir.NewScalarType("f32", 4), nil
		case "f64":
			return mir.NewScalarType("f64", 8), nil
		case "void":
			return mir.NewScalarType("void", 0), nil
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
		elem, err := lowerType(t.Elem)
		if err != nil {
			return nil, fmt.Errorf("array element type: %w", err)
		}
		arrTy := mir.NewArrayType(elem, t.Len)
		arrTy.Size = t.Len * mirSizeOf(elem)
		return arrTy, nil
	case *minir.RecordType:
		return nil, fmt.Errorf("record types are not supported in the first bridge pass")
	case *minir.FunctionType:
		return nil, fmt.Errorf("function types are not supported in the first bridge pass")
	case *minir.OpenArrayType:
		elem, err := lowerType(t.Elem)
		if err != nil {
			return nil, fmt.Errorf("open array element type: %w", err)
		}
		// Open arrays are represented as a pointer to the first element, so they
		// have the same type as a pointer to the element type.
		return mir.NewPointerType(elem, 8), nil
	default:
		return nil, fmt.Errorf("unsupported type %T", ty)
	}
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

// computeStrides computes byte strides for each dimension of an array type.
// For nested arrays, strides are computed bottom-up (innermost first).
// Returns nil for non-array types.
// Example: for [3 x [4 x i32]], returns [16, 4] (stride of outermost dim is 4*4, innermost is 4).
func computeStrides(elemType minir.Type) []int {
	if elemType == nil {
		return nil
	}

	arr, ok := elemType.(*minir.ArrayType)
	if !ok {
		return nil // scalar type, no strides needed
	}

	var strides []int

	// Walk through nested arrays and collect lengths.
	var dims []int
	cur := arr
	for cur != nil {
		dims = append(dims, cur.Len)
		nextArr, ok := cur.Elem.(*minir.ArrayType)
		if !ok {
			break
		}
		cur = nextArr
	}

	// Compute base elem size (the innermost type).
	baseSize := 8 // default word size
	if cur != nil && cur.Elem != nil {
		if pt, ok := cur.Elem.(*minir.PrimitiveType); ok {
			switch strings.ToLower(pt.String()) {
			case "i1":
				baseSize = 1
			case "i8", "u8":
				baseSize = 1
			case "i16", "u16":
				baseSize = 2
			case "i32", "u32", "f32":
				baseSize = 4
			case "i64", "u64", "f64":
				baseSize = 8
			}
		}
	}

	// Compute strides: stride[i] = stride[i+1] * dims[i+1], or baseSize for innermost.
	strides = make([]int, len(dims))
	for i := len(dims) - 1; i >= 0; i-- {
		if i == len(dims)-1 {
			strides[i] = baseSize
		} else {
			// stride for this dimension = stride of next dimension * length of next dimension
			strides[i] = strides[i+1] * dims[i+1]
		}
	}

	return strides
}

// lowerGEP converts a minir.GEPInst to mir.GEPInstr, computing strides and lowering operands.
func lowerGEP(gepInst *minir.GEPInst, regByTemp map[*minir.Temp]*mir.Register, globals map[string]*mir.Symbol) (mir.Instr, error) {
	if gepInst == nil {
		return nil, fmt.Errorf("gep: nil instruction")
	}

	dst, err := lowerTemp(gepInst.Dst, regByTemp)
	if err != nil {
		return nil, fmt.Errorf("gep dst: %w", err)
	}

	base, err := lowerOperand(gepInst.Base, regByTemp, globals)
	if err != nil {
		return nil, fmt.Errorf("gep base: %w", err)
	}

	// Convert ElemType from minir to mir.Type; compute strides.
	elemType, err := lowerType(gepInst.ElemType)
	if err != nil {
		return nil, fmt.Errorf("gep elem type: %w", err)
	}

	strides := computeStrides(gepInst.ElemType)

	// Lower runtime indices.
	indices := make([]mir.Operand, 0, len(gepInst.Indices))
	for _, idx := range gepInst.Indices {
		midx, err := lowerOperand(idx, regByTemp, globals)
		if err != nil {
			return nil, fmt.Errorf("gep index: %w", err)
		}
		indices = append(indices, midx)
	}

	return &mir.GEPInstr{
		Dst:      dst,
		Base:     base,
		ElemType: elemType,
		Strides:  strides,
		Offsets:  gepInst.Offsets,
		Indices:  indices,
	}, nil
}

// mirSizeOf returns the byte size of a lowered MIR type
func mirSizeOf(ty *mir.Type) int {
	if ty == nil {
		return 8
	}
	if ty.Size > 0 {
		return ty.Size
	}
	if ty.Len > 0 && ty.Elem != nil {
		return ty.Len * mirSizeOf(ty.Elem)
	}
	return 8
}
