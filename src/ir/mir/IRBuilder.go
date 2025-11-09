package mir

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
)

type IRBuilder struct {
	ctx *report.Context

	Func  *Function // Current function being built
	Block *Block    // Current basic block

	TempGen    int // To generate fresh temps
	labelCount int // To generate new labels
	curExit    string
}

func NewIRBuilder(ctx *report.Context) *IRBuilder {
	return &IRBuilder{ctx: ctx}
}

func (b *IRBuilder) Build(MIRProgram *hir.Program) *Program {
	program := &Program{}

	for _, HIRModule := range MIRProgram.Modules {
		module := b.lowerModule(HIRModule)
		program.Modules = append(program.Modules, module)
	}

	return program
}

func (b *IRBuilder) lowerModule(HIRModule *hir.Module) *Module {
	env := NewSymbolTable(GlobalEnv)
	functions := make([]*Function, 0)
	globals := make(map[string]*Global)

	for _, decl := range HIRModule.Decls {
		switch d := decl.(type) {
		case *hir.Function:
			fn := b.lowerFunction(d, NewSymbolTable(env))
			functions = append(functions, fn)
			env.Define(d.Name, fn)
		case *hir.Constant:
			global := &Global{
				NameStr: d.Name,
				BName:   d.Name,
				Kind:    "CONST",
				Typ:     b.lowerType(d.Type),
				Value:   b.ensureValue(d.Value),
				Offset:  d.Offset,
				Size:    d.Size,
			}
			globals[d.Name] = global
			env.Define(global.NameStr, global)
		case *hir.Variable:
			global := &Global{
				NameStr: d.Name,
				BName:   d.Name,
				Kind:    "VAR",
				Typ:     b.lowerType(d.Type),
				Offset:  d.Offset,
				Size:    d.Size,
			}
			globals[d.Name] = global
			env.Define(global.NameStr, global)
		}
	}

	functions = append(functions, b.lowerFunction(HIRModule.Init, NewSymbolTable(env)))

	return &Module{
		Name:    HIRModule.Name,
		IsEntry: HIRModule.IsEntry,
		Globals: globals,
		Funcs:   functions,
		Env:     env,
	}
}

func (b *IRBuilder) lowerFunction(fxn *hir.Function, env *SymbolTable) *Function {
	fn := NewFunction(fxn.FnName(), fxn.IsExport, b.lowerType(fxn.Result), env)

	currentFn := b.Func
	defer func() { b.Func = currentFn }()

	b.Func = fn

	for _, param := range fxn.Params {
		t := &Param{
			ID:    param.Name,
			BName: param.Name,
			Typ:   b.lowerType(param.Typ),
		}

		switch param.Kind {
		case hir.ValueParam:
			t.Kind = "VALUE"
		case hir.InParam:
			t.Kind = "IN"
		case hir.VarParam:
			t.Kind = "VAR"
		}

		fn.Params = append(fn.Params, t)
		fn.Env.Define(t.ID, t)
	}

	for _, local := range fxn.Locals {
		switch d := local.(type) {
		case *hir.Variable:
			lcl := &Local{
				ID:     d.Name,
				BName:  d.Name,
				Typ:    b.lowerType(d.Type),
				Offset: d.Offset,
				Size:   d.Size,
			}
			fn.Locals = append(fn.Locals, lcl)
			fn.Env.Define(lcl.ID, lcl)
		case *hir.Constant:
			c := &NamedConst{
				ID:         d.Name,
				ConstValue: b.ensureValue(d.Value),
				Typ:        b.lowerType(d.Type),
				Offset:     d.Offset,
				Size:       d.Size,
			}
			fn.Constants[d.Mangled] = c
			fn.Env.Define(c.ID, c)
		case *hir.Function:
		}
	}

	entry := NewBlock("entry")

	fn.Blocks[entry.ID] = entry
	fn.Entry = entry

	// mark "entry" as currently active block for inserting instructions
	b.SetBlock(entry)

	b.lowerCompoundStmt(fxn.Body)

	exit := NewBlock(fn.FnName + "_exit")
	fn.Blocks[exit.ID] = exit
	fn.Exit = exit

	return fn
}

func (b *IRBuilder) lowerCompoundStmt(stmt *hir.CompoundStmt) {
	for _, st := range stmt.Stmts {
		switch s := st.(type) {
		case *hir.AssignStmt:
			b.AssignStmt(s)
		case *hir.ReturnStmt:
			b.ReturnStmt(s)
		case *hir.IfStmt:
			b.IfStmt(s)
		case *hir.LoopStmt:
			b.LoopStmt(s)
		case *hir.ExitStmt:
			b.ExitStmt()
		case *hir.CompoundStmt:
			b.lowerCompoundStmt(s)
		case *hir.FuncCall:
			b.FuncCall(s)
		case *hir.CaseStmt:
		case *hir.WithStmt:
		default:
		}
	}
}

func (b *IRBuilder) lowerType(ty types.Type) Type {
	if ty == nil {
		return Void
	}

	switch ty := ty.(type) {
	case *types.BasicType:
		switch ty.Kind {
		case types.BYTE, types.CHAR:
			return UInt8Type
		case types.INT8:
			return Int8Type
		case types.INT16, types.SHORTINT:
			return Int16Type
		case types.INT32, types.INTEGER:
			return Int32Type
		case types.INT64, types.LONGINT:
			return Int64Type
		case types.REAL:
			return Float32Type
		case types.LONGREAL:
			return Float64Type
		case types.WCHAR:
			return UInt16Type
		case types.BOOLEAN:
			return Int1Type
		case types.SET:
			return UInt32Type
		default:
			panic("unhandled basic type: " + fmt.Sprintf("%v", ty.Kind))
		}
	case *types.ArrayType:
		return &ArrayType{
			Len:  ty.Length,
			Elem: b.lowerType(ty.Elem),
		}
	case *types.StringType:
		return &StringType{Length: ty.Length}
	case types.EnumType:
	case *types.RecordType:
	case *types.ProcedureType:
	case *types.PointerType:
	case *types.NamedType:
	default:
		panic("unhandled type: " + fmt.Sprintf("%T", ty))
	}

	return Void
}

func (b *IRBuilder) AssignStmt(assign *hir.AssignStmt) {
	addr := b.ensureAddr(assign.Left)
	value := b.ensureValue(assign.Right)

	b.emitAssign(addr, value)
}

func (b *IRBuilder) emitAssign(dst Value, value Value) {
	if dst.IsMem() {
		b.Emit(&StoreInst{Addr: dst, Val: value})
	} else {
		b.Emit(&MoveInst{Target: dst, Value: value})
	}
}

func (b *IRBuilder) ensureValue(expr hir.Expr) Value {
	switch e := expr.(type) {
	case *hir.Literal:
		return b.lowerConst(e)
	case *hir.BinaryExpr:
		return b.lowerBinary(e)
	case *hir.UnaryExpr:
	case *hir.VariableRef:
		v, found := b.Func.Env.Lookup(e.Name)
		if !found {
			panic(fmt.Sprintf("undefined variable: '%s'", e.Name))
		}

		t := b.NewTemp(v.Type())
		b.Emit(&LoadInst{Addr: v, Target: t})

		return t
	case *hir.ConstantRef:
		return b.Func.Constants[e.Mangled]
	case *hir.Param:
		v, found := b.Func.Env.Lookup(e.Name)
		if !found {
			panic(fmt.Sprintf("undefined parameter: '%s'", e.Name))
		}

		param, ok := v.(*Param)
		if !ok {
			panic(fmt.Sprintf("symbol '%s' is not a parameter", e.Name))
		}

		if param.Kind == "VALUE" {
			return param
		}

		t := b.NewTemp(param.Type())
		b.Emit(&LoadInst{Addr: param, Target: t})

		return t
	case *hir.SetExpr:
	case *hir.RangeExpr:
	case *hir.FieldAccess:
	case *hir.IndexExpr:
		addr := b.lowerIndexExpr(e)

		t := b.NewTemp(addr.Type())
		b.Emit(&LoadInst{Addr: addr, Target: t})

		return t

	case *hir.DerefExpr:
	case *hir.TypeGuardExpr:
	default:
		panic("unhandled expr: " + fmt.Sprintf("%T", e))
	}
	return nil
}

func (b *IRBuilder) ensureAddr(expr hir.Expr) Value {
	switch e := expr.(type) {
	case *hir.Param:
		v, found := b.Func.Env.Lookup(e.Name)
		if !found {
			panic(fmt.Sprintf("undefined parameter: '%s'", e.Name))
		}

		param, ok := v.(*Param)
		if !ok {
			panic(fmt.Sprintf("symbol '%s' is not a parameter", e.Name))
		}

		return param
	case *hir.VariableRef:
		v, found := b.Func.Env.Lookup(e.Name)
		if !found {
			panic(fmt.Sprintf("undefined variable: '%s'", e.Name))
		}
		return v
	case *hir.FieldAccess:
	case *hir.IndexExpr:
		return b.lowerIndexExpr(e)
	case *hir.DerefExpr:
	default:
		panic("ensureAddr: expr is not addressable: " + fmt.Sprintf("%T", expr))
	}
	return nil
}

func (b *IRBuilder) ExitStmt() {
	if b.curExit == "" {
		panic("EXIT used outside loop")
	}

	jmp := &JumpInst{Target: b.curExit}
	b.SetTerm(jmp)
	b.Emit(jmp)
}

func (b *IRBuilder) IfStmt(s *hir.IfStmt) {
	endLabel := b.NewLabel("if_end")

	// Track all conditional branches (initial + elsif)
	allConds := append([]*hir.ElseIfBranch{
		{Cond: s.Cond, Body: s.Then},
	}, s.ElseIfs...)

	var trueLabel, falseLabel string

	for i, branch := range allConds {
		cond := b.ensureValue(branch.Cond)
		falseLabel = b.NewLabel(fmt.Sprintf("if_next_%d", i))
		trueLabel = b.NewLabel(fmt.Sprintf("if_true_%d", i))

		// set conditional branch as the terminator instruction for this block
		// and add it to the list of instructions in the block
		br := &CondBrInst{Cond: cond, TrueLabel: trueLabel, FalseLabel: falseLabel}
		b.SetTerm(br)
		b.Emit(br)

		// Emit the conditional label for the true path block
		nextBlk := b.NewBlock(trueLabel)
		b.SetBlock(nextBlk)

		b.lowerCompoundStmt(branch.Body)

		if !b.BlockTermIsSet() {
			jmp := &JumpInst{Target: endLabel}
			b.SetTerm(jmp)
			b.Emit(jmp)
		}

		// Emit the false path conditional label block
		nextBlk = b.NewBlock(falseLabel)
		b.SetBlock(nextBlk)
	}

	// ELSE branch or fallthrough
	if s.Else != nil {
		b.lowerCompoundStmt(s.Else)
	}

	if !b.BlockTermIsSet() {
		jmp := &JumpInst{Target: endLabel}
		b.SetTerm(jmp)
		b.Emit(jmp)
	}

	// Final end block
	endBlk := b.NewBlock(endLabel)
	b.SetBlock(endBlk)
}

func (b *IRBuilder) LoopStmt(s *hir.LoopStmt) {
	loopLabel := b.NewLabel(s.Label)
	exitLabel := b.NewLabel(s.Label + "_exit")

	// Register exit label for EXIT lowering
	prevExit := b.curExit
	b.curExit = exitLabel
	defer func() { b.curExit = prevExit }()

	// Emit loop label
	loopBlk := b.NewBlock(loopLabel)
	b.SetBlock(loopBlk)

	// Emit loop body
	b.lowerCompoundStmt(s.Body)

	// Jump back to loop
	jmp := &JumpInst{Target: loopLabel}
	b.SetTerm(jmp)
	b.Emit(jmp)

	// Emit loop exit label
	exitBlk := b.NewBlock(exitLabel)
	b.SetBlock(exitBlk)
}

func (b *IRBuilder) FuncCall(s *hir.FuncCall) Value {
	var args []Value
	fxn, ok := b.Func.Env.Lookup(s.Func.Name)
	if !ok {
		panic("undefined function: " + s.Func.Name)
	}

	fn, ok := fxn.(*Function)
	if !ok {
		panic("symbol is not a function: " + s.Func.Name)
	}

	for idx, arg := range s.Args {
		param := fn.Params[idx].(*Param)

		var v Value

		switch param.Kind {
		case "VAR", "IN":
			v = b.ensureAddr(arg)
		default:
			v = b.ensureValue(arg)
		}

		b.Emit(&Arg{Index: idx, Value: v})
		args = append(args, v)
	}

	var t Value
	if s.RetType != nil {
		t = b.NewTemp(s.RetType)
	} else {
		t = b.NewTemp(Void)
	}

	name := s.Func.Mangled
	if name == "" {
		name = s.Func.Name
	}

	b.Emit(&CallInst{
		Target: t,
		Callee: name,
		Args:   args,
	})

	b.Func.IsLeaf = false

	return t
}

func (b *IRBuilder) ReturnStmt(s *hir.ReturnStmt) {
	var result Value
	if s.Result != nil {
		result = b.ensureValue(s.Result)
	}

	b.CreateReturn(result)
}

func (b *IRBuilder) lowerOp(op token.Kind) InstrOp {
	switch op {
	case token.PLUS:
		return ADD
	case token.MINUS:
		return SUB
	case token.STAR:
		return MUL
	case token.DIV:
		return DIV
	case token.MOD:
		return REM
	case token.NOT:
		return NOT
	case token.EQUAL:
		return EQ
	case token.NEQ:
		return NE
	case token.LESS:
		return LT
	case token.LEQ:
		return LE
	case token.GREAT:
		return GT
	case token.GEQ:
		return GE
	case token.OR:
		return OR

	default:
		panic("unknown operator " + op.String())
	}
}

func (b *IRBuilder) lowerBinary(expr *hir.BinaryExpr) Value {
	left := b.ensureValue(expr.Left)
	right := b.ensureValue(expr.Right)
	op := b.lowerOp(expr.Op)

	return b.CreateBinary(op, left, right, expr.SemaType)
}

func (b *IRBuilder) lowerConst(c *hir.Literal) Value {
	var v Value

	switch c.Kind {
	case token.BYTE_LIT:
		value, _ := strconv.ParseUint(c.Value, 10, 8)
		v = &IntegerLit{LitValue: value, Bits: 8, Signed: false, Typ: b.lowerType(c.SemaType)}
	case token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
		value, _ := strconv.ParseUint(c.Value, 10, 64)

		var bits uint
		if c.Kind == token.INT8_LIT {
			bits = 8
		} else if c.Kind == token.INT16_LIT {
			bits = 16
		} else if c.Kind == token.INT32_LIT {
			bits = 32
		} else {
			bits = 64
		}

		v = &IntegerLit{LitValue: value, Bits: bits, Signed: true, Typ: b.lowerType(c.SemaType)}
	case token.REAL_LIT, token.LONGREAL_LIT:
		value, _ := strconv.ParseFloat(c.Value, 64)

		var bits uint
		if c.Kind == token.REAL_LIT {
			bits = 32
		} else {
			bits = 64
		}

		v = &FloatLit{LitValue: value, Bits: bits, Typ: b.lowerType(c.SemaType)}
	case token.CHAR_LIT, token.WCHAR_LIT:
		v = &CharLit{LitValue: []rune(c.Value), Typ: b.lowerType(c.SemaType) /* additional info? */}
	case token.HEX_STR_LIT, token.STR_LIT:
		name := "str_const_" + strconv.Itoa(len(b.Func.Constants))
		str := &StrLit{LitName: name, LitValue: c.Value, Typ: b.lowerType(c.SemaType)}

		b.Func.Constants[name] = str
		v = str
	case token.TRUE:
		v = True
	case token.FALSE:
		v = False
	default:
		panic("unhandled literal kind: " + c.Kind.String())
	}

	t := b.NewTemp(v.Type())
	b.Emit(&MoveInst{Target: t, Value: v})

	return t
}

func (b *IRBuilder) lowerIndexExpr(e *hir.IndexExpr) Value {
	// Generate the base array or pointer value
	arr := b.ensureValue(e.Array)
	arrayType := arr.Type().(*ArrayType)

	var indices []Value
	for _, idx := range e.Index {
		indices = append(indices, b.ensureValue(idx))
	}

	// 2) Compute offset in BYTES using strides
	strides := arrayType.Strides()
	if len(strides) != len(indices) {
		panic("LowerArrayIndexAddr: index count does not match array rank")
	}

	// acc = 0
	var acc Value
	acc = b.NewTemp(Int64Type)
	b.emitAssign(acc, &IntegerLit{LitValue: 0})

	for k := 0; k < len(indices); k++ {
		// term = indices[k] * strides[k]
		tMul := b.CreateBinary(MUL, indices[k], &IntegerLit{LitValue: uint64(strides[k])}, Int64Type)

		// acc = acc + term
		acc = b.CreateBinary(ADD, acc, tMul, Int64Type)
	}

	// 3) addr = baseAddr + acc
	addr := b.CreateBinary(ADD, arr, acc, Int64Type)
	return &Mem{Base: addr}
}

func (b *IRBuilder) CreateBinary(op InstrOp, left, right Value, ty Type) Value {
	t := b.NewTemp(ty)

	if op.IsCmpCondCode() {
		b.Emit(&CmpInst{Target: t, Op: op, Left: left, Right: right})
	} else {
		b.Emit(&BinaryInst{Target: t, Op: op, Left: left, Right: right})
	}

	return t
}

func (b *IRBuilder) CreateAddrOf(addr Value) Value {
	t := b.NewTemp(addr.Type())
	b.Emit(&AddrOf{
		Target: t,
		Addr:   addr,
	})

	return t
}

func (b *IRBuilder) CreateReturn(value Value) {
	ret := &ReturnInst{Result: value}
	b.SetTerm(ret)
	b.Emit(ret)
}

func (b *IRBuilder) Emit(instr Instr) {
	b.Block.Instrs = append(b.Block.Instrs, instr)
}

func (b *IRBuilder) NewTemp(ty Type) *Temp {
	b.TempGen++
	name := fmt.Sprintf("t%d", b.TempGen)
	return &Temp{ID: name, BName: name, Typ: ty}
}

func (b *IRBuilder) NewLabel(prefix string) string {
	b.labelCount++
	return prefix + "_" + strconv.Itoa(b.labelCount)
}

func (b *IRBuilder) SetTerm(term Instr) {
	b.Block.Term = term
}

func (b *IRBuilder) BlockTermIsSet() bool {
	return b.Block.Term != nil
}

func (b *IRBuilder) NewBlock(label string) *Block {
	blk := NewBlock(label)
	b.Func.Blocks[blk.ID] = blk

	return blk
}

func (b *IRBuilder) SetBlock(block *Block) {
	b.Block = block
}
