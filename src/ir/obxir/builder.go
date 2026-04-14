package obxir

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// IRBuilder lowers a desugared HIR program into obxir.
type IRBuilder struct {
	wordSize uint64 // target machine word / pointer size in bytes

	Func  *Function // currently active function
	Block *Block    // currently active basic block

	TempGen    int
	labelCount int
	curExit    string
}

func NewIRBuilder(wordSize uint64) *IRBuilder {
	return &IRBuilder{wordSize: wordSize}
}

// Build lowers a complete HIR program to an obxir Program.
func (b *IRBuilder) Build(hirProgram *desugar.Program) *Program {
	program := &Program{}
	for _, m := range hirProgram.Modules {
		program.Modules = append(program.Modules, b.lowerModule(m))
	}
	return program
}

func (b *IRBuilder) lowerModule(hirMod *desugar.Module) *Module {
	env := NewSymbolTable(GlobalEnv)
	functions := make([]*Function, 0)
	globals := make(map[string]*GlobalVariable)
	constants := make(map[string]Constant)

	for _, decl := range hirMod.Decls {
		switch d := decl.(type) {
		case *desugar.Function:
			fn := b.lowerFunction(d, NewSymbolTable(env))
			functions = append(functions, fn)
			env.Define(d.Name, fn)
		case *desugar.Constant:
			nc := &NamedConst{
				OrigName:   d.Name,
				Ident:      d.Mangled,
				Typ:        b.lowerType(d.Type),
				ConstValue: b.ensureValue(d.Value),
				Size:       d.Size,
			}
			constants[nc.Ident] = nc
			env.Define(nc.Ident, nc)
		case *desugar.Variable:
			gv := &GlobalVariable{
				Ident:    d.Mangled,
				OrigName: d.Name,
				Typ:      b.lowerType(d.Type),
				Size:     d.Size,
			}
			globals[gv.Ident] = gv
			env.Define(gv.Ident, gv)
		}
	}

	functions = append(functions, b.lowerFunction(hirMod.Init, NewSymbolTable(env)))

	return &Module{
		Name:    hirMod.Name,
		IsEntry: hirMod.IsEntry,
		Globals: globals,
		Consts:  constants,
		Funcs:   functions,
		Env:     env,
	}
}

func (b *IRBuilder) lowerFunction(hirFn *desugar.Function, env *SymbolTable) *Function {
	fn := NewFunction(hirFn.FnName(), hirFn.IsExport, b.lowerType(hirFn.Result), env)

	prevFn := b.Func
	defer func() { b.Func = prevFn }()
	b.Func = fn

	for _, param := range hirFn.Params {
		var kind string
		switch param.Kind {
		case desugar.ValueParam:
			kind = "VALUE"
		case desugar.InParam:
			kind = "IN"
		case desugar.VarParam:
			kind = "VAR"
		}
		p := &Param{Ident: param.Name, OrigName: param.Name, Typ: b.lowerType(param.Typ), Kind: kind}
		fn.Params = append(fn.Params, p)
		fn.Env.Define(p.Ident, p)
	}

	for _, local := range hirFn.Locals {
		switch d := local.(type) {
		case *desugar.Variable:
			lcl := &Local{Ident: d.Mangled, OrigName: d.Name, Typ: b.lowerType(d.Type), Size: d.Size}
			fn.Locals = append(fn.Locals, lcl)
			fn.Env.Define(lcl.Ident, lcl)
		case *desugar.Constant:
			c := &NamedConst{Ident: d.Name, ConstValue: b.ensureValue(d.Value), Typ: b.lowerType(d.Type), Size: d.Size}
			fn.Constants[d.Mangled] = c
			fn.Env.Define(d.Mangled, c)
		case *desugar.Function:
			// nested procedures: handled at usage
		}
	}

	entry := NewBlock("entry")
	fn.Blocks[entry.ID] = entry
	fn.Entry = entry

	fn.Env.Define(hirFn.Name, fn)
	if key := hirFn.FnName(); key != hirFn.Name {
		fn.Env.Define(key, fn)
	}

	b.SetBlock(entry)
	b.CompoundStmt(hirFn.Body)

	exit := NewBlock(fn.FnName + "_exit")
	fn.Blocks[exit.ID] = exit
	fn.Exit = exit

	return fn
}

// ─── Statement lowering ───────────────────────────────────────────────────

func (b *IRBuilder) CompoundStmt(stmt *desugar.CompoundStmt) {
	for _, st := range stmt.Stmts {
		switch s := st.(type) {
		case *desugar.AssignStmt:
			b.AssignStmt(s)
		case *desugar.ReturnStmt:
			b.ReturnStmt(s)
		case *desugar.IfStmt:
			b.IfStmt(s)
		case *desugar.LoopStmt:
			b.LoopStmt(s)
		case *desugar.ExitStmt:
			b.ExitStmt()
		case *desugar.CompoundStmt:
			b.CompoundStmt(s)
		case *desugar.FuncCall:
			b.FuncCall(s)
		case *desugar.CaseStmt:
			b.CaseStmt(s)
		case *desugar.WithStmt:
			b.WithStmt(s)
		}
	}
}

func (b *IRBuilder) AssignStmt(assign *desugar.AssignStmt) {
	addr := b.ensureAddr(assign.Left)
	value := b.ensureValue(assign.Right)
	b.emitAssign(addr, value)
}

func (b *IRBuilder) ReturnStmt(s *desugar.ReturnStmt) {
	var result Value
	if s.Result != nil {
		result = b.ensureValue(s.Result)
	}
	b.CreateReturn(result)
}

func (b *IRBuilder) ExitStmt() {
	if b.curExit == "" {
		panic("EXIT used outside loop")
	}
	b.SetTerm(&JumpInst{Target: b.curExit})
}

func (b *IRBuilder) IfStmt(stmt *desugar.IfStmt) {
	endLabel := b.NewLabel("if_end")

	allConds := append([]*desugar.ElseIfBranch{{Cond: stmt.Cond, Body: stmt.Then}}, stmt.ElseIfs...)

	for i, branch := range allConds {
		cond := b.ensureValue(branch.Cond)
		falseLabel := b.NewLabel(fmt.Sprintf("if_next_%d", i))
		trueLabel := b.NewLabel(fmt.Sprintf("if_true_%d", i))

		b.SetTerm(&CondBrInst{Cond: cond, TrueLabel: trueLabel, FalseLabel: falseLabel})

		nextBlk := b.NewBlock(trueLabel)
		b.SetBlock(nextBlk)
		b.CompoundStmt(branch.Body)
		if !b.BlockTermIsSet() {
			b.SetTerm(&JumpInst{Target: endLabel})
		}

		nextBlk = b.NewBlock(falseLabel)
		b.SetBlock(nextBlk)
	}

	if stmt.Else != nil {
		b.CompoundStmt(stmt.Else)
	}
	if !b.BlockTermIsSet() {
		b.SetTerm(&JumpInst{Target: endLabel})
	}

	endBlk := b.NewBlock(endLabel)
	b.SetBlock(endBlk)
}

func (b *IRBuilder) LoopStmt(stmt *desugar.LoopStmt) {
	loopLabel := b.NewLabel(stmt.Label)
	exitLabel := b.NewLabel(stmt.Label + "_exit")

	prevExit := b.curExit
	b.curExit = exitLabel
	defer func() { b.curExit = prevExit }()

	loopBlk := b.NewBlock(loopLabel)
	b.SetBlock(loopBlk)
	b.CompoundStmt(stmt.Body)
	b.SetTerm(&JumpInst{Target: loopLabel})

	exitBlk := b.NewBlock(exitLabel)
	b.SetBlock(exitBlk)
}

func (b *IRBuilder) CaseStmt(stmt *desugar.CaseStmt) {
	endLabel := b.NewLabel("case_end")
	elseLabel := endLabel
	if stmt.Else != nil {
		elseLabel = b.NewLabel("case_else")
	}

	type test struct {
		testLabel  string
		bodyLabel  string
		labelRange *desugar.LabelRange
		body       *desugar.CompoundStmt
	}

	var tests []test
	testCount := 0
	for bodyCount, c := range stmt.Cases {
		for i := range c.Labels {
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
	b.SetTerm(&JumpInst{Target: tests[0].testLabel})

	for i, t := range tests {
		block := b.NewBlock(t.testLabel)
		b.SetBlock(block)

		nextTestLabel := elseLabel
		if i+1 < len(tests) {
			nextTestLabel = tests[i+1].testLabel
		}
		b.lowerCaseTest(t.labelRange, tSel, t.bodyLabel, nextTestLabel)
	}

	for _, t := range tests {
		block := b.NewBlock(t.bodyLabel)
		b.SetBlock(block)
		b.CompoundStmt(t.body)
		if !b.BlockTermIsSet() {
			b.SetTerm(&JumpInst{Target: endLabel})
		}
	}

	if stmt.Else != nil {
		elseBlk := b.NewBlock(elseLabel)
		b.SetBlock(elseBlk)
		b.CompoundStmt(stmt.Else)
		if !b.BlockTermIsSet() {
			b.SetTerm(&JumpInst{Target: endLabel})
		}
	}

	endBlk := b.NewBlock(endLabel)
	b.SetBlock(endBlk)
}

func (b *IRBuilder) lowerCaseTest(r *desugar.LabelRange, tSel Value, bodyLabel, nextLabel string) {
	tLow := b.ensureValue(r.Low)

	if r.High == nil || r.Low == r.High {
		tmp := b.NewTemp(Int1Type)
		b.Emit(&ICmpInst{Target: tmp, Op: EQ, Left: tSel, Right: tLow})
		b.SetTerm(&CondBrInst{Cond: tmp, TrueLabel: bodyLabel, FalseLabel: nextLabel})
		return
	}

	tHigh := b.ensureValue(r.High)
	cmpLo := b.NewTemp(Int1Type)
	cmpHi := b.NewTemp(Int1Type)
	both := b.NewTemp(Int1Type)

	b.Emit(&ICmpInst{Target: cmpLo, Op: GE, Left: tSel, Right: tLow})
	b.Emit(&ICmpInst{Target: cmpHi, Op: LE, Left: tSel, Right: tHigh})
	b.Emit(&BinaryInst{Target: both, Op: AND, Left: cmpLo, Right: cmpHi})
	b.SetTerm(&CondBrInst{Cond: both, TrueLabel: bodyLabel, FalseLabel: nextLabel})
}

func (b *IRBuilder) WithStmt(stmt *desugar.WithStmt) {
	endLabel := b.NewLabel("with_end")
	elseLabel := endLabel
	if stmt.Else != nil && len(stmt.Else.Stmts) > 0 {
		elseLabel = b.NewLabel("with_else")
	}

	n := len(stmt.Guards)
	bodyLabels := make([]string, n)
	nextLabels := make([]string, n)
	for i := range stmt.Guards {
		bodyLabels[i] = b.NewLabel(fmt.Sprintf("with_body_%d", i))
		if i+1 < n {
			nextLabels[i] = b.NewLabel(fmt.Sprintf("with_check_%d", i+1))
		} else {
			nextLabels[i] = elseLabel
		}
	}

	for i, guard := range stmt.Guards {
		exprVal := b.ensureValue(guard.Expr)
		var typeVal Type
		if tr, ok := guard.Type.(*desugar.TypeRef); ok {
			typeVal = b.lowerType(tr.UnderType)
		} else {
			typeVal = b.lowerType(guard.Type.Type())
		}

		cond := b.NewTemp(Int1Type)
		b.Emit(&ICmpInst{Target: cond, Op: IS, Left: exprVal, Right: &TypeValue{Ty: typeVal}})
		b.SetTerm(&CondBrInst{Cond: cond, TrueLabel: bodyLabels[i], FalseLabel: nextLabels[i]})

		bodyBlk := b.NewBlock(bodyLabels[i])
		b.SetBlock(bodyBlk)
		b.CompoundStmt(guard.Body)
		if !b.BlockTermIsSet() {
			b.SetTerm(&JumpInst{Target: endLabel})
		}

		if i+1 < n {
			nextBlk := b.NewBlock(nextLabels[i])
			b.SetBlock(nextBlk)
		}
	}

	if stmt.Else != nil && len(stmt.Else.Stmts) > 0 {
		elseBlk := b.NewBlock(elseLabel)
		b.SetBlock(elseBlk)
		b.CompoundStmt(stmt.Else)
		if !b.BlockTermIsSet() {
			b.SetTerm(&JumpInst{Target: endLabel})
		}
	}

	endBlk := b.NewBlock(endLabel)
	b.SetBlock(endBlk)
}

// ─── Expression lowering ─────────────────────────────────────────────────

func (b *IRBuilder) ensureValue(expr desugar.Expr) Value {
	if ty, ok := expr.(types.Type); ok {
		return &TypeValue{Ty: b.lowerType(ty)}
	}

	switch e := expr.(type) {
	case *desugar.Literal:
		return b.lowerConst(e)
	case *desugar.FuncCall:
		return b.FuncCall(e)
	case *desugar.BinaryExpr:
		return b.lowerBinary(e)
	case *desugar.UnaryExpr:
		return b.lowerUnaryExpr(e)
	case *desugar.VariableRef:
		v, found := b.Func.Env.Lookup(e.Mangled)
		if !found {
			panic(fmt.Sprintf("undefined variable: '%s'", e.Name))
		}
		t := b.NewTemp(v.Type())
		b.Emit(&LoadInst{Addr: v, Target: t})
		return t
	case *desugar.ConstantRef:
		return b.Func.Constants[e.Mangled]
	case *desugar.Param:
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
	case *desugar.SetExpr:
		return b.lowerSetExpr(e)
	case *desugar.RangeExpr:
		return b.lowerRangeExpr(e)
	case *desugar.FieldAccess:
		addr := b.lowerFieldAccess(e)
		t := b.NewTemp(addr.Type())
		b.Emit(&LoadInst{Addr: addr, Target: t})
		return t
	case *desugar.IndexExpr:
		addr := b.lowerIndexExpr(e)
		t := b.NewTemp(addr.Type())
		b.Emit(&LoadInst{Addr: addr, Target: t})
		return t
	case *desugar.DerefExpr:
		addr := b.lowerDerefExpr(e)
		t := b.NewTemp(addr.Type())
		b.Emit(&LoadInst{Addr: &Mem{Base: addr}, Target: t})
		return t
	case *desugar.TypeGuardExpr:
	case *desugar.TypeRef:
		return &TypeValue{Ty: b.lowerType(e.UnderType)}
	default:
		panic("unhandled expr: " + fmt.Sprintf("%T", e))
	}
	return nil
}

func (b *IRBuilder) ensureAddr(expr desugar.Expr) Value {
	switch e := expr.(type) {
	case *desugar.Param:
		v, found := b.Func.Env.Lookup(e.Name)
		if !found {
			panic(fmt.Sprintf("undefined parameter: '%s'", e.Name))
		}
		param, ok := v.(*Param)
		if !ok {
			panic(fmt.Sprintf("symbol '%s' is not a parameter", e.Name))
		}
		return param
	case *desugar.VariableRef:
		v, found := b.Func.Env.Lookup(e.Mangled)
		if !found {
			panic(fmt.Sprintf("undefined variable: '%s'", e.Name))
		}
		return v
	case *desugar.FieldAccess:
		return b.lowerFieldAccess(e)
	case *desugar.IndexExpr:
		return b.lowerIndexExpr(e)
	case *desugar.DerefExpr:
		addr := b.lowerDerefExpr(e)
		return &Mem{Base: addr}
	default:
		panic("ensureAddr: expr is not addressable: " + fmt.Sprintf("%T", expr))
	}
}

func (b *IRBuilder) emitAssign(dst Value, value Value) {
	if dst.IsMem() {
		b.Emit(&StoreInst{Addr: dst, Val: value})
	} else {
		b.Emit(&MoveInst{Target: dst, Value: value})
	}
}

func (b *IRBuilder) lowerBinary(expr *desugar.BinaryExpr) Value {
	left := b.ensureValue(expr.Left)
	right := b.ensureValue(expr.Right)
	op := b.lowerOp(expr.Op)
	return b.CreateBinary(op, left, right, b.lowerType(expr.SemaType))
}

func (b *IRBuilder) lowerUnaryExpr(u *desugar.UnaryExpr) Value {
	op := b.lowerOp(u.Op)
	// Unary minus maps to NEG, not binary SUB.
	if op == SUB {
		op = NEG
	}
	operand := b.ensureValue(u.Operand)
	return b.CreateUnary(op, operand, b.lowerType(u.SemaType))
}

func (b *IRBuilder) lowerConst(c *desugar.Literal) Value {
	var v Value

	switch c.Kind {
	case token.BYTE_LIT:
		val, _ := strconv.ParseUint(c.Value, 10, 8)
		v = &IntegerLit{LitValue: val, Bits: 8, Signed: false, Typ: b.lowerType(c.SemaType)}
	case token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
		val, _ := strconv.ParseUint(c.Value, 10, 64)
		var bits uint
		switch c.Kind {
		case token.INT8_LIT:
			bits = 8
		case token.INT16_LIT:
			bits = 16
		case token.INT32_LIT:
			bits = 32
		default:
			bits = 64
		}
		v = &IntegerLit{LitValue: val, Bits: bits, Signed: true, Typ: b.lowerType(c.SemaType)}
	case token.REAL_LIT, token.LONGREAL_LIT:
		val, _ := strconv.ParseFloat(c.Value, 64)
		var bits uint
		if c.Kind == token.REAL_LIT {
			bits = 32
		} else {
			bits = 64
		}
		v = &FloatLit{LitValue: val, Bits: bits, Typ: b.lowerType(c.SemaType)}
	case token.CHAR_LIT, token.WCHAR_LIT:
		v = &CharLit{LitValue: []rune(c.Value), Typ: b.lowerType(c.SemaType)}
	case token.HEX_STR_LIT, token.STR_LIT:
		name := "str_const_" + strconv.Itoa(len(b.Func.Constants))
		str := &StrLit{LitName: name, LitValue: c.Value, Typ: b.lowerType(c.SemaType)}
		b.Func.Constants[name] = str
		v = str
	case token.TRUE:
		v = True
	case token.FALSE:
		v = False
	case token.NIL:
		v = UInt64Lit(0)
	default:
		panic("unhandled literal kind: " + c.Kind.String())
	}

	t := b.NewTemp(v.Type())
	b.Emit(&MoveInst{Target: t, Value: v})
	return t
}

func (b *IRBuilder) lowerSetExpr(s *desugar.SetExpr) Value {
	setTemp := b.NewPrefixTemp("set", UInt32Type)
	b.Emit(&MoveInst{Target: setTemp, Value: UInt32Lit(0)})

	for _, elem := range s.Elems {
		var mask Value
		switch elem := elem.(type) {
		case *desugar.RangeExpr:
			mask = b.lowerRangeExpr(elem)
		default:
			idx := b.ensureValue(elem)
			mask = b.NewPrefixTemp("mask", UInt32Type)
			t := b.NewTemp(UInt32Type)
			b.Emit(&MoveInst{Target: t, Value: UInt32Lit(1)})
			b.Emit(&BinaryInst{Target: mask.(*Temp), Op: LSHL, Left: t, Right: idx})
		}

		tmp := b.NewTemp(UInt32Type)
		b.Emit(&BinaryInst{Target: tmp, Op: OR, Left: setTemp, Right: mask})
		b.Emit(&MoveInst{Target: setTemp, Value: tmp})
	}

	return setTemp
}

func (b *IRBuilder) lowerRangeExpr(e *desugar.RangeExpr) Value {
	low := b.ensureValue(e.Low)
	var high Value
	if e.High != nil {
		high = b.ensureValue(e.High)
	} else {
		high = low
	}

	length := b.NewPrefixTemp("len", UInt32Type)
	b.Emit(&BinaryInst{Target: length, Op: ADD, Left: high, Right: UInt32Lit(1)})
	b.Emit(&BinaryInst{Target: length, Op: SUB, Left: length, Right: low})

	ones := b.NewPrefixTemp("ones", UInt32Type)
	t := b.NewTemp(UInt32Type)
	b.Emit(&MoveInst{Target: t, Value: UInt32Lit(1)})
	b.Emit(&BinaryInst{Target: ones, Op: LSHL, Left: t, Right: length})
	b.Emit(&BinaryInst{Target: ones, Op: SUB, Left: ones, Right: t})

	mask := b.NewPrefixTemp("mask", UInt32Type)
	b.Emit(&BinaryInst{Target: mask, Op: LSHL, Left: ones, Right: low})

	return mask
}

func (b *IRBuilder) lowerSet(op InstrOp, left, right Value) *Temp {
	switch op {
	case IN:
		mask := b.NewPrefixTemp("mask", UInt32Type)
		t := b.NewTemp(UInt32Type)
		b.Emit(&MoveInst{Target: t, Value: UInt32Lit(1)})
		b.Emit(&BinaryInst{Target: mask, Op: LSHL, Left: t, Right: left})
		tmp := b.NewTemp(UInt32Type)
		b.Emit(&BinaryInst{Target: tmp, Op: AND, Left: right, Right: mask})
		cond := b.NewTemp(UInt32Type)
		t = b.NewTemp(UInt32Type)
		b.Emit(&MoveInst{Target: t, Value: UInt32Lit(0)})
		b.Emit(&ICmpInst{Target: cond, Op: NE, Left: tmp, Right: t})
		return cond
	case ADD:
		res := b.NewTemp(SetType)
		b.Emit(&BinaryInst{Target: res, Op: OR, Left: left, Right: right})
		return res
	case MUL:
		res := b.NewTemp(SetType)
		b.Emit(&BinaryInst{Target: res, Op: AND, Left: left, Right: right})
		return res
	case SUB:
		notSetB := b.NewTemp(SetType)
		b.Emit(&UnaryInst{Target: notSetB, Op: NOT, Operand: right})
		res := b.NewTemp(SetType)
		b.Emit(&BinaryInst{Target: res, Op: AND, Left: left, Right: notSetB})
		return res
	case RDIV:
		res := b.NewTemp(SetType)
		b.Emit(&BinaryInst{Target: res, Op: XOR, Left: left, Right: right})
		return res
	default:
		panic("lowerSet: unhandled set operation")
	}
}

func (b *IRBuilder) lowerOpenArrayIndex(arr Value, indices []Value, elemSize uint64, wordSize uint64) Value {
	n := len(indices)

	lengths := make([]Value, n)
	for k := 0; k < n; k++ {
		addr := b.NewTemp(Int64Type)
		b.Emit(&BinaryInst{Target: addr, Op: ADD, Left: arr, Right: Int64Lit(uint64(k) * wordSize)})
		lv := b.NewTemp(Int64Type)
		b.Emit(&LoadInst{Target: lv, Addr: &Mem{Base: addr}})
		lengths[k] = lv
	}

	strides := make([]Value, n)
	one := UInt64Lit(1)
	strides[n-1] = one

	for k := n - 2; k >= 0; k-- {
		acc := b.NewTemp(UInt64Type)
		b.Emit(&MoveInst{Target: acc, Value: one})
		for j := k + 1; j < n; j++ {
			tmp := b.NewTemp(UInt64Type)
			b.Emit(&BinaryInst{Target: tmp, Op: MUL, Left: acc, Right: lengths[j]})
			acc = tmp
		}
		strides[k] = acc
	}

	linear := b.NewTemp(UInt64Type)
	b.Emit(&MoveInst{Target: linear, Value: UInt64Lit(0)})

	for k := 0; k < n; k++ {
		mul := b.NewTemp(UInt64Type)
		b.Emit(&BinaryInst{Target: mul, Op: MUL, Left: indices[k], Right: strides[k]})
		sum := b.NewTemp(UInt64Type)
		b.Emit(&BinaryInst{Target: sum, Op: ADD, Left: linear, Right: mul})
		linear = sum
	}

	byteOffset := b.NewTemp(UInt64Type)
	b.Emit(&BinaryInst{Target: byteOffset, Op: MUL, Left: linear, Right: UInt64Lit(elemSize)})

	withHeader := b.NewTemp(UInt64Type)
	b.Emit(&BinaryInst{Target: withHeader, Op: ADD, Left: byteOffset, Right: UInt64Lit(uint64(n) * wordSize)})

	addr := b.NewTemp(UInt64Type)
	b.Emit(&BinaryInst{Target: addr, Op: ADD, Left: arr, Right: withHeader})

	return &Mem{Base: addr}
}

func (b *IRBuilder) lowerIndexExpr(e *desugar.IndexExpr) Value {
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
		wordSize := b.wordSize
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

func (b *IRBuilder) lowerFieldAccess(e *desugar.FieldAccess) Value {
	recordAddr := b.ensureAddr(e.Record)

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

	offset := UInt64Lit(uint64(field.Offset))
	tmp := b.NewTemp(Int64Type)
	b.Emit(&MoveInst{Target: tmp, Value: offset})
	fieldAddr := b.CreateBinary(ADD, recordAddr, tmp, Int64Type)

	return &Mem{Base: fieldAddr}
}

func (b *IRBuilder) lowerDerefExpr(e *desugar.DerefExpr) Value {
	return b.ensureValue(e.Pointer)
}

// ─── Type lowering ────────────────────────────────────────────────────────

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
			return SetType
		default:
			panic("unhandled basic type: " + fmt.Sprintf("%v", ty.Kind))
		}
	case *types.ArrayType:
		return &ArrayType{Len: ty.Length, Elem: b.lowerType(ty.Elem)}
	case *types.StringType:
		return &StringType{Length: ty.Length}
	case *types.EnumType:
	case *types.RecordType:
		fields := make(map[string]RecordField)
		offset := 0
		for _, f := range ty.Fields {
			ft := b.lowerType(f.Type)
			fields[f.Name] = RecordField{Name: f.Name, Type: ft, Offset: offset}
			offset += sema.AlignTo(f.Type.Width(), f.Type.Alignment())
		}
		return &RecordType{Fields: fields, Size: offset}
	case *types.ProcedureType:
	case *types.PointerType:
		return &PointerType{Ref: b.lowerType(ty.Base)}
	case *types.NamedType:
		return b.lowerType(ty.Def)
	default:
		panic("unhandled type: " + fmt.Sprintf("%T", ty))
	}

	return Void
}

// ─── Operator lowering ────────────────────────────────────────────────────

func (b *IRBuilder) lowerOp(op token.Kind) InstrOp {
	switch op {
	case token.PLUS:
		return ADD
	case token.MINUS:
		return SUB
	case token.STAR:
		return MUL
	case token.QUOT:
		return RDIV
	case token.DIV:
		return IDIV
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
	case token.IN:
		return IN
	case token.IS:
		return IS
	case token.AND:
		return AND
	default:
		panic("unknown operator " + op.String())
	}
}

// ─── Function call lowering ───────────────────────────────────────────────

func (b *IRBuilder) FuncCall(call *desugar.FuncCall) Value {
	var fxn Value
	var ok bool
	for _, n := range []string{call.Func.Mangled, call.Func.Name, strings.ToLower(call.Func.Name)} {
		if n == "" {
			continue
		}
		if fxn, ok = b.Func.Env.Lookup(n); ok {
			break
		}
	}
	if !ok {
		panic("undefined function: " + call.Func.Name)
	}

	fn, ok := fxn.(*Function)
	if !ok {
		panic("symbol is not a function: " + call.Func.Name)
	}

	b.Func.IsLeaf = false

	if fn.IsBuiltin {
		f := builtinLowering[fn.FnName]
		return f(b, call)
	}

	var args []Value
	b.lowerArgs(fn, call.Args, 0, len(fn.Params), &args)
	if fn.Variadic {
		b.lowerVarArgs(call.Args, len(fn.Params), len(call.Args), &args)
	}

	var t Value
	if call.RetType != nil {
		t = b.NewTemp(b.lowerType(call.RetType))
	} else {
		t = b.NewTemp(Void)
	}

	name := call.Func.Mangled
	if name == "" {
		name = call.Func.Name
	}

	b.Emit(&CallInst{Target: t, Callee: name, Args: args})
	return t
}

func (b *IRBuilder) lowerArgs(fn *Function, args []desugar.Expr, start, end int, out *[]Value) {
	for idx := start; idx < end; idx++ {
		param := fn.Params[idx].(*Param)
		var v Value
		switch param.Kind {
		case "VAR", "IN":
			v = b.ensureAddr(args[idx])
		default:
			v = b.ensureValue(args[idx])
		}
		b.Emit(&Arg{Index: idx, Value: v})
		*out = append(*out, v)
	}
}

// callee resolves the *Function for a FuncCall by looking up the callee name
// in the current function's environment.  It is used by builtin handlers that
// need the callee's parameter or variadic metadata (e.g. lowerPrintfBuiltin).
func (b *IRBuilder) callee(call *desugar.FuncCall) *Function {
	for _, n := range []string{call.Func.Mangled, call.Func.Name, strings.ToLower(call.Func.Name)} {
		if n == "" {
			continue
		}
		if v, ok := b.Func.Env.Lookup(n); ok {
			if fn, ok := v.(*Function); ok {
				return fn
			}
		}
	}
	panic("callee: function not found in env: " + call.Func.Name)
}

func (b *IRBuilder) lowerVarArgs(args []desugar.Expr, start, end int, out *[]Value) {	for idx := start; idx < end; idx++ {
		v := b.ensureValue(args[idx])
		b.Emit(&Arg{Index: idx, Value: v})
		*out = append(*out, v)
	}
}

// ─── Builder helpers ──────────────────────────────────────────────────────

// CreateBinary emits a binary instruction and returns its result temp.
func (b *IRBuilder) CreateBinary(op InstrOp, left, right Value, ty Type) *Temp {
	if right.Type() == SetType || left.Type() == SetType {
		return b.lowerSet(op, left, right)
	}
	t := b.NewTemp(ty)
	if op.IsCmpCondCode() {
		b.Emit(&ICmpInst{Target: t, Op: op, Left: left, Right: right})
	} else {
		b.Emit(&BinaryInst{Target: t, Op: op, Left: left, Right: right})
	}
	return t
}

// CreateUnary emits a unary instruction and returns its result temp.
func (b *IRBuilder) CreateUnary(op InstrOp, operand Value, ty Type) Value {
	t := b.NewTemp(ty)
	b.Emit(&UnaryInst{Target: t, Op: op, Operand: operand})
	return t
}

// CreateAddrOf emits an address-of instruction.
func (b *IRBuilder) CreateAddrOf(addr Value) Value {
	t := b.NewTemp(addr.Type())
	b.Emit(&AddrOf{Target: t, Addr: addr})
	return t
}

// CreateReturn emits a return instruction.
func (b *IRBuilder) CreateReturn(value Value) {
	b.SetTerm(&ReturnInst{Result: value})
}

// Emit appends instr to the current block's instruction list.
func (b *IRBuilder) Emit(instr Instr) {
	b.Block.Instrs = append(b.Block.Instrs, instr)
}

// NewTemp creates a fresh numbered temporary.
func (b *IRBuilder) NewTemp(ty Type) *Temp {
	b.TempGen++
	name := fmt.Sprintf("t%d", b.TempGen)
	return &Temp{Ident: name, OrigName: name, Typ: ty, Size: ty.Width()}
}

// NewPrefixTemp creates a fresh temporary with a named prefix.
func (b *IRBuilder) NewPrefixTemp(prefix string, ty Type) *Temp {
	b.TempGen++
	name := fmt.Sprintf("%s.t%d", prefix, b.TempGen)
	return &Temp{Ident: name, OrigName: name, Typ: ty, Size: ty.Width()}
}

// NewLabel creates a unique label with the given prefix.
func (b *IRBuilder) NewLabel(prefix string) string {
	b.labelCount++
	return prefix + "_" + strconv.Itoa(b.labelCount)
}

// SetTerm appends term to the current block and marks it as the terminator.
func (b *IRBuilder) SetTerm(term Instr) {
	b.Emit(term)
	b.Block.Term = term
}

// BlockTermIsSet returns true when the current block already has a terminator.
func (b *IRBuilder) BlockTermIsSet() bool {
	return b.Block.Term != nil
}

// NewBlock creates a new basic block and registers it with the current function.
func (b *IRBuilder) NewBlock(label string) *Block {
	blk := NewBlock(label)
	b.Func.Blocks[blk.ID] = blk
	return blk
}

// SetBlock makes block the currently active basic block.
func (b *IRBuilder) SetBlock(block *Block) {
	b.Block = block
}

// ─── NEW open-array helpers ───────────────────────────────────────────────

func (b *IRBuilder) lowerNEWPtrToRec(allocSize uint64) Value {
	sizeTemp := b.NewTemp(Int64Type)
	b.Emit(&MoveInst{Target: sizeTemp, Value: Int64Lit(allocSize)})
	ptrTemp := b.NewTemp(PointerTo(UInt8Type))
	b.Emit(&CallInst{Target: ptrTemp, Callee: "malloc", Args: []Value{sizeTemp}})
	return ptrTemp
}

func (b *IRBuilder) lowerNEWFixedArray(allocSize uint64) Value {
	sizeImm := UInt64Lit(allocSize)
	t := b.NewTemp(Int64Type)
	b.Emit(&MoveInst{Target: t, Value: sizeImm})
	b.Emit(&Arg{Index: 0, Value: t})
	ptr := b.NewTemp(PointerTo(UInt8Type))
	b.Emit(&CallInst{Target: ptr, Callee: "malloc", Args: []Value{t}})
	return ptr
}

func (b *IRBuilder) lowerNEWOpen(dims []Value, elemSize uint64, wordSize uint64) Value {
	n := len(dims)

	prod := b.NewTemp(Int64Type)
	b.Emit(&MoveInst{Target: prod, Value: Int64Lit(1)})
	for _, d := range dims {
		tmp := b.NewTemp(Int64Type)
		b.Emit(&BinaryInst{Target: tmp, Op: MUL, Left: prod, Right: d})
		prod = tmp
	}

	dataBytes := b.NewTemp(UInt64Type)
	b.Emit(&BinaryInst{Target: dataBytes, Op: MUL, Left: prod, Right: Int64Lit(elemSize)})

	totalSize := b.NewTemp(Int64Type)
	b.Emit(&BinaryInst{Target: totalSize, Op: ADD, Left: dataBytes, Right: Int64Lit(uint64(n) * wordSize)})

	b.Emit(&Arg{Index: 0, Value: totalSize})

	ptr := b.NewTemp(Int64Type)
	b.Emit(&CallInst{Target: ptr, Callee: "malloc", Args: []Value{totalSize}})

	for i, d := range dims {
		offsetImm := int64(i) * int64(wordSize)
		mem := &Mem{Base: ptr, Offs: offsetImm}
		b.Emit(&StoreInst{Addr: mem, Val: d})
	}

	return ptr
}

