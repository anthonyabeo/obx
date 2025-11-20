package mir

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/sema"
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
	globals := make(map[string]*GlobalVariable)
	constants := make(map[string]Constant)

	for _, decl := range HIRModule.Decls {
		switch d := decl.(type) {
		case *hir.Function:
			fn := b.lowerFunction(d, NewSymbolTable(env))
			functions = append(functions, fn)
			env.Define(d.Name, fn)
		case *hir.Constant:
			nameConst := &NamedConst{
				OrigName:   d.Name,
				Ident:      d.Mangled,
				Typ:        b.lowerType(d.Type),
				ConstValue: b.ensureValue(d.Value),
				Size:       d.Size,
			}
			constants[nameConst.Ident] = nameConst
			env.Define(nameConst.Ident, nameConst)
		case *hir.Variable:
			global := &GlobalVariable{
				Ident:    d.Mangled,
				OrigName: d.Name,
				Typ:      b.lowerType(d.Type),
				Size:     d.Size,
			}
			globals[global.Ident] = global
			env.Define(global.Ident, global)
		}
	}

	functions = append(functions, b.lowerFunction(HIRModule.Init, NewSymbolTable(env)))

	return &Module{
		Name:    HIRModule.Name,
		IsEntry: HIRModule.IsEntry,
		Globals: globals,
		Consts:  constants,
		Funcs:   functions,
		Env:     env,
	}
}

func (b *IRBuilder) lowerFunction(HIRFxn *hir.Function, env *SymbolTable) *Function {
	fn := NewFunction(HIRFxn.FnName(), HIRFxn.IsExport, b.lowerType(HIRFxn.Result), env)

	currentFn := b.Func
	defer func() { b.Func = currentFn }()

	b.Func = fn

	for _, param := range HIRFxn.Params {
		var kind string
		switch param.Kind {
		case hir.ValueParam:
			kind = "VALUE"
		case hir.InParam:
			kind = "IN"
		case hir.VarParam:
			kind = "VAR"
		}

		p := &Param{Ident: param.Name, OrigName: param.Name, Typ: b.lowerType(param.Typ), Kind: kind}

		fn.Params = append(fn.Params, p)
		fn.Env.Define(p.Ident, p)
	}

	for _, local := range HIRFxn.Locals {
		switch d := local.(type) {
		case *hir.Variable:
			lcl := &Local{
				Ident:    d.Mangled,
				OrigName: d.Name,
				Typ:      b.lowerType(d.Type),
				Size:     d.Size,
			}
			fn.Locals = append(fn.Locals, lcl)
			fn.Env.Define(lcl.Ident, lcl)
		case *hir.Constant:
			c := &NamedConst{
				Ident:      d.Name,
				ConstValue: b.ensureValue(d.Value),
				Typ:        b.lowerType(d.Type),
				Size:       d.Size,
			}
			fn.Constants[d.Mangled] = c
			fn.Env.Define(d.Mangled, c)
		case *hir.Function:
		}
	}

	entry := NewBlock("entry")

	fn.Blocks[entry.ID] = entry
	fn.Entry = entry

	// mark "entry" as currently active block for inserting instructions
	b.SetBlock(entry)

	b.CompoundStmt(HIRFxn.Body)

	exit := NewBlock(fn.FnName + "_exit")
	fn.Blocks[exit.ID] = exit
	fn.Exit = exit

	return fn
}

func (b *IRBuilder) CompoundStmt(stmt *hir.CompoundStmt) {
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
			b.CompoundStmt(s)
		case *hir.FuncCall:
			b.FuncCall(s)
		case *hir.CaseStmt:
			b.CaseStmt(s)
		case *hir.WithStmt:
			b.WithStmt(s)
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
	case *types.EnumType:
	case *types.RecordType:
		fields := make(map[string]RecordField)
		offset := 0
		for _, f := range ty.Fields {
			fieldType := b.lowerType(f.Type)
			fields[f.Name] = RecordField{
				Name:   f.Name,
				Type:   fieldType,
				Offset: offset,
			}
			offset += sema.AlignTo(f.Type.Width(), f.Type.Alignment())
		}

		return &RecordType{
			Fields: fields,
			Size:   offset,
		}
	case *types.ProcedureType:
	case *types.PointerType:
		return &PointerType{
			Ref: b.lowerType(ty.Base),
		}
	case *types.NamedType:
		return b.lowerType(ty.Def)
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
		return b.lowerUnaryExpr(e)
	case *hir.VariableRef:
		v, found := b.Func.Env.Lookup(e.Mangled)
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
		addr := b.lowerFieldAccess(e)
		t := b.NewTemp(addr.Type())
		b.Emit(&LoadInst{Addr: addr, Target: t})

		return t
	case *hir.IndexExpr:
		addr := b.lowerIndexExpr(e)

		t := b.NewTemp(addr.Type())
		b.Emit(&LoadInst{Addr: addr, Target: t})

		return t
	case *hir.DerefExpr:
		addr := b.lowerDerefExpr(e)

		t := b.NewTemp(addr.Type())
		b.Emit(&LoadInst{Addr: &Mem{Base: addr}, Target: t})

		return t
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
		v, found := b.Func.Env.Lookup(e.Mangled)
		if !found {
			panic(fmt.Sprintf("undefined variable: '%s'", e.Name))
		}
		return v
	case *hir.FieldAccess:
		return b.lowerFieldAccess(e)
	case *hir.IndexExpr:
		return b.lowerIndexExpr(e)
	case *hir.DerefExpr:
		addr := b.lowerDerefExpr(e)
		return &Mem{Base: addr}
	default:
		panic("ensureAddr: expr is not addressable: " + fmt.Sprintf("%T", expr))
	}
	return nil
}

func (b *IRBuilder) CaseStmt(stmt *hir.CaseStmt) {
	endLabel := b.NewLabel("case_end")
	elseLabel := endLabel
	if stmt.Else != nil {
		elseLabel = b.NewLabel("case_else")
	}

	type test struct {
		testLabel  string
		bodyLabel  string
		labelRange *hir.LabelRange
		body       *hir.CompoundStmt
	}

	var tests []test
	testCount := 0
	for bodyCount, c := range stmt.Cases {
		for i := 0; i < len(c.Labels); i++ {
			tests = append(tests, test{
				testLabel:  b.NewLabel(fmt.Sprintf("case_test_%d", testCount)),
				bodyLabel:  b.NewLabel(fmt.Sprintf("case_body_%d", bodyCount)),
				labelRange: c.Labels[i],
				body:       c.Body,
			})
			testCount++
		}
	}

	tSel := b.ensureValue(stmt.Expr)

	jmp := &JumpInst{Target: tests[0].testLabel}
	b.SetTerm(jmp)
	b.Emit(jmp)

	// Emit all test blocks
	for i, t := range tests {
		block := b.NewBlock(t.testLabel)
		b.SetBlock(block)

		nextTestLabel := elseLabel
		if i+1 < len(tests) {
			nextTestLabel = tests[i+1].testLabel
		}

		b.lowerCaseTest(t.labelRange, tSel, t.bodyLabel, nextTestLabel)
	}

	// Emit all body blocks
	for _, t := range tests {
		block := b.NewBlock(t.bodyLabel)
		b.SetBlock(block)

		b.CompoundStmt(t.body)

		if !b.BlockTermIsSet() {
			j := &JumpInst{Target: endLabel}
			b.SetTerm(j)
			b.Emit(j)
		}
	}

	// ELSE block
	if stmt.Else != nil {
		elseBlk := b.NewBlock(elseLabel)
		b.SetBlock(elseBlk)

		b.CompoundStmt(stmt.Else)

		if !b.BlockTermIsSet() {
			j := &JumpInst{Target: endLabel}
			b.SetTerm(j)
			b.Emit(j)
		}
	}

	// Final end block
	endBlk := b.NewBlock(endLabel)
	b.SetBlock(endBlk)
}
func (b *IRBuilder) lowerCaseTest(r *hir.LabelRange, tSel Value, bodyLabel, nextTestLabel string) {
	tLow := b.ensureValue(r.Low)

	if r.High == nil || r.Low == r.High {
		// Singleton case label
		tmp := b.NewTemp(Int1Type)
		b.Emit(&CmpInst{Target: tmp, Op: EQ, Left: tSel, Right: tLow})

		br := &CondBrInst{Cond: tmp, TrueLabel: bodyLabel, FalseLabel: nextTestLabel}
		b.SetTerm(br)
		b.Emit(br)
		return
	}

	// Range: Low <= x <= High
	tHigh := b.ensureValue(r.High)

	cmpLo := b.NewTemp(Int1Type)
	cmpHi := b.NewTemp(Int1Type)
	both := b.NewTemp(Int1Type)

	b.Emit(&CmpInst{Target: cmpLo, Op: GE, Left: tSel, Right: tLow})
	b.Emit(&CmpInst{Target: cmpHi, Op: LE, Left: tSel, Right: tHigh})
	b.Emit(&BinaryInst{Target: both, Op: AND, Left: cmpLo, Right: cmpHi})

	br := &CondBrInst{Cond: both, TrueLabel: bodyLabel, FalseLabel: nextTestLabel}
	b.SetTerm(br)
	b.Emit(br)
}

func (b *IRBuilder) WithStmt(stmt *hir.WithStmt) {

}

func (b *IRBuilder) ExitStmt() {
	if b.curExit == "" {
		panic("EXIT used outside loop")
	}

	jmp := &JumpInst{Target: b.curExit}
	b.SetTerm(jmp)
	b.Emit(jmp)
}

func (b *IRBuilder) IfStmt(stmt *hir.IfStmt) {
	endLabel := b.NewLabel("if_end")

	// Track all conditional branches (initial + elsif)
	allConds := append([]*hir.ElseIfBranch{
		{Cond: stmt.Cond, Body: stmt.Then},
	}, stmt.ElseIfs...)

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

		b.CompoundStmt(branch.Body)

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
	if stmt.Else != nil {
		b.CompoundStmt(stmt.Else)
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

func (b *IRBuilder) LoopStmt(stmt *hir.LoopStmt) {
	loopLabel := b.NewLabel(stmt.Label)
	exitLabel := b.NewLabel(stmt.Label + "_exit")

	// Register exit label for EXIT lowering
	prevExit := b.curExit
	b.curExit = exitLabel
	defer func() { b.curExit = prevExit }()

	// Emit loop label
	loopBlk := b.NewBlock(loopLabel)
	b.SetBlock(loopBlk)

	// Emit loop body
	b.CompoundStmt(stmt.Body)

	// Jump back to loop
	jmp := &JumpInst{Target: loopLabel}
	b.SetTerm(jmp)
	b.Emit(jmp)

	// Emit loop exit label
	exitBlk := b.NewBlock(exitLabel)
	b.SetBlock(exitBlk)
}

func (b *IRBuilder) lowerArgs(fn *Function, Args []hir.Expr, startIdx, endIdx int, args *[]Value) {
	for idx := startIdx; idx < endIdx; idx++ {
		param := fn.Params[idx].(*Param)
		arg := Args[idx]

		var v Value

		switch param.Kind {
		case "VAR", "IN":
			v = b.ensureAddr(arg)
		default:
			v = b.ensureValue(arg)
		}

		b.Emit(&Arg{Index: idx, Value: v})
		*args = append(*args, v)
	}
}

func (b *IRBuilder) lowerVarArgs(Args []hir.Expr, startIdx, endIdx int, args *[]Value) {
	for idx := startIdx; idx < endIdx; idx++ {
		arg := Args[idx]

		v := b.ensureValue(arg)

		b.Emit(&Arg{Index: idx, Value: v})
		*args = append(*args, v)
	}
}

func (b *IRBuilder) FuncCall(call *hir.FuncCall) Value {
	fxn, ok := b.Func.Env.Lookup(strings.ToLower(call.Func.Name))
	if !ok {
		panic("undefined function: " + call.Func.Name)
	}

	fn, ok := fxn.(*Function)
	if !ok {
		panic("symbol is not a function: " + call.Func.Name)
	}

	// the currently active function make a function call. The callee function exists
	// and is a valid, hence the active function is not a leaf
	b.Func.IsLeaf = false

	if fn.IsBuiltin {
		f := builtinLowering[fn.FnName]
		return f(b, fn, call)
	}

	var args []Value
	b.lowerArgs(fn, call.Args, 0, len(fn.Params), &args)
	if fn.Variadic {
		b.lowerVarArgs(call.Args, len(fn.Params), len(call.Args), &args)
	}

	var t Value
	if call.RetType != nil {
		t = b.NewTemp(call.RetType)
	} else {
		t = b.NewTemp(Void)
	}

	name := call.Func.Mangled
	if name == "" {
		name = call.Func.Name
	}

	b.Emit(&CallInst{
		Target: t,
		Callee: name,
		Args:   args,
	})

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

func (b *IRBuilder) lowerUnaryExpr(u *hir.UnaryExpr) Value {
	op := b.lowerOp(u.Op)
	operand := b.ensureValue(u.Operand)

	return b.CreateUnary(op, operand, u.SemaType)
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
	case token.SET:

	default:
		panic("unhandled literal kind: " + c.Kind.String())
	}

	t := b.NewTemp(v.Type())
	b.Emit(&MoveInst{Target: t, Value: v})

	return t
}

func (b *IRBuilder) lowerOpenArrayIndex(arr Value, indices []Value, elemSize uint64, wordSize uint64) Value {

	n := len(indices)

	// -------------------------------------------------------------------------
	// 1. Load dimension lengths from the header: arr[0], arr[1], ..., arr[n-1]
	// -------------------------------------------------------------------------
	lengths := make([]Value, n)
	for k := 0; k < n; k++ {
		// compute address = arr + k*wordSize
		addr := b.NewTemp(Int64Type)
		b.Emit(&BinaryInst{
			Target: addr,
			Op:     ADD,
			Left:   arr,
			Right:  Int64Lit(uint64(k) * wordSize),
		})

		// load len(k)
		LVal := b.NewTemp(Int64Type)
		b.Emit(&LoadInst{
			Target: LVal,
			Addr:   &Mem{Base: addr},
		})
		lengths[k] = LVal
	}

	// -------------------------------------------------------------------------
	// 2. Compute dynamic strides
	//
	// stride[n-1] = 1
	// stride[k] = product(len[k+1] .. len[n-1])
	// -------------------------------------------------------------------------
	strides := make([]Value, n)
	one := UInt64Lit(1)

	// last stride = 1
	strides[n-1] = one

	// fill backwards
	for k := n - 2; k >= 0; k-- {
		acc := b.NewTemp(UInt64Type)
		b.Emit(&MoveInst{Target: acc, Value: one})
		for j := k + 1; j < n; j++ {
			tmp := b.NewTemp(UInt64Type)
			b.Emit(&BinaryInst{
				Target: tmp,
				Op:     MUL,
				Left:   acc,
				Right:  lengths[j],
			})
			acc = tmp
		}
		strides[k] = acc
	}

	// -------------------------------------------------------------------------
	// 3. Compute linear index = Σ (indices[k] * strides[k])
	// -------------------------------------------------------------------------
	linear := b.NewTemp(UInt64Type)
	b.Emit(&MoveInst{Target: linear, Value: UInt64Lit(0)})

	for k := 0; k < n; k++ {
		mul := b.NewTemp(UInt64Type)
		b.Emit(&BinaryInst{
			Target: mul,
			Op:     MUL,
			Left:   indices[k],
			Right:  strides[k],
		})

		sum := b.NewTemp(UInt64Type)
		b.Emit(&BinaryInst{
			Target: sum,
			Op:     ADD,
			Left:   linear,
			Right:  mul,
		})
		linear = sum
	}

	// -------------------------------------------------------------------------
	// 4. Multiply by element size: byteOffset = linear * elemSize
	// -------------------------------------------------------------------------
	byteOffset := b.NewTemp(UInt64Type)
	b.Emit(&BinaryInst{
		Target: byteOffset,
		Op:     MUL,
		Left:   linear,
		Right:  UInt64Lit(elemSize),
	})

	// -------------------------------------------------------------------------
	// 5. Add header size = n * wordSize
	// -------------------------------------------------------------------------
	withHeader := b.NewTemp(UInt64Type)
	b.Emit(&BinaryInst{
		Target: withHeader,
		Op:     ADD,
		Left:   byteOffset,
		Right:  UInt64Lit(uint64(n) * wordSize),
	})

	// -------------------------------------------------------------------------
	// 6. Add array base pointer → final address
	// -------------------------------------------------------------------------
	addr := b.NewTemp(UInt64Type)
	b.Emit(&BinaryInst{
		Target: addr,
		Op:     ADD,
		Left:   arr,
		Right:  withHeader,
	})

	return &Mem{Base: addr}
}

func (b *IRBuilder) lowerIndexExpr(e *hir.IndexExpr) Value {
	arr := b.ensureAddr(e.Array)

	var arrayType *ArrayType
	switch at := arr.Type().(type) {
	case *PointerType:
		t := b.NewTemp(UInt64Type)
		b.Emit(&LoadInst{Target: t, Addr: arr})
		arr = t
		arrayType = at.Ref.(*ArrayType)
	case *ArrayType:
		arrayType = at
	default:
		panic("lowerIndexExpr: base is not array or pointer type")
	}

	indices := make([]Value, len(e.Index))
	for i, idx := range e.Index {
		indices[i] = b.ensureValue(idx)
	}

	if arrayType.IsOpen() {
		wordSize := b.ctx.TargetMachineWordSize
		width := uint64(arrayType.Elem.Width())
		return b.lowerOpenArrayIndex(arr, indices, width, wordSize)
	}

	strides := arrayType.Strides()
	if len(strides) != len(indices) {
		panic("lowerIndexExpr: index count does not match array rank")
	}

	acc := b.NewTemp(Int64Type)
	b.emitAssign(acc, Int64Lit(0))

	for k, idx := range indices {
		stride := uint64(strides[k])
		tMul := b.CreateBinary(MUL, idx, Int64Lit(stride), Int64Type)
		acc = b.CreateBinary(ADD, acc, tMul, Int64Type)
	}

	addr := b.CreateBinary(ADD, arr, acc, Int64Type)
	return &Mem{Base: addr}
}

func (b *IRBuilder) lowerFieldAccess(e *hir.FieldAccess) Value {
	// Get the base record address
	recordAddr := b.ensureAddr(e.Record)

	// Look up the field offset from the record’s type
	var recordType *RecordType
	switch ty := recordAddr.Type().(type) {
	case *RecordType:
		recordType = ty
	case *PointerType:
		rec := ty.Ref.(*RecordType)
		tmp := b.NewTemp(UInt64Type)
		b.Emit(&LoadInst{Target: tmp, Addr: recordAddr})
		recordAddr = tmp
		recordType = rec
	default:
		panic("lowerFieldAccess: base is not a record type")
	}

	field, exists := recordType.Field(e.Field)
	if !exists {
		panic("lowerFieldAccess: field does not exist in record type")
	}

	// Compute the field address by adding the offset to the base address
	offset := UInt64Lit(uint64(field.Offset))
	tmp := b.NewTemp(Int64Type)
	b.Emit(&MoveInst{Target: tmp, Value: offset})
	fieldAddr := b.CreateBinary(ADD, recordAddr, tmp, Int64Type)

	return &Mem{Base: fieldAddr}
}

func (b *IRBuilder) lowerDerefExpr(e *hir.DerefExpr) Value {
	return b.ensureValue(e.Pointer)
}

func (b *IRBuilder) CreateBinary(op InstrOp, left, right Value, ty Type) *Temp {
	t := b.NewTemp(ty)

	if op.IsCmpCondCode() {
		b.Emit(&CmpInst{Target: t, Op: op, Left: left, Right: right})
	} else {
		b.Emit(&BinaryInst{Target: t, Op: op, Left: left, Right: right})
	}

	return t
}

func (b *IRBuilder) CreateUnary(op InstrOp, operand Value, ty Type) Value {
	t := b.NewTemp(ty)
	b.Emit(&UnaryInst{
		Target:  t,
		Op:      op,
		Operand: operand,
	})

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
	return &Temp{Ident: name, OrigName: name, Typ: ty, Size: ty.Width()}
}

func (b *IRBuilder) NewLabel(prefix string) string {
	b.labelCount++
	return prefix + "_" + strconv.Itoa(b.labelCount)
}

// SetTerm updates the terminating instruction field of the currently active
// basic block with 'term'.
func (b *IRBuilder) SetTerm(term Instr) {
	b.Block.Term = term
}

// BlockTermIsSet returns true if the terminating instruction of the currently
// active basic block is set, i.e. not nil
func (b *IRBuilder) BlockTermIsSet() bool {
	return b.Block.Term != nil
}

func (b *IRBuilder) NewBlock(label string) *Block {
	blk := NewBlock(label)
	b.Func.Blocks[blk.ID] = blk

	return blk
}

// SetBlock make 'block' the currently active basic block, ensuring that
// subsequent instructions created will be added to this block.
func (b *IRBuilder) SetBlock(block *Block) {
	b.Block = block
}
