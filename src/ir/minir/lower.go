package minir

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// ── public API ────────────────────────────────────────────────────────────────

// Lowerer translates desugared HIR functions into minir Functions.
type Lowerer struct {
	// per-function mutable state, reset by lowerFunction
	blockSeq int               // monotone block-ID counter
	labelSeq int               // label-suffix counter
	fn       *Function         // function currently being lowered
	curBlock *Block            // active basic block
	varEnv   map[string]*Temp  // variable name → alloca address temp (IsAddr=true)
	constEnv map[string]Value  // constant name → pre-computed Value
	loopExit map[string]string // loop label → exit-block label; "" = innermost loop
}

// Lower converts every non-external desugar.Function in prog into a
// minir.Function. External (FFI) declarations are skipped.
func Lower(prog *desugar.Program) []*Function {
	l := &Lowerer{}
	var out []*Function
	for _, mod := range prog.Modules {
		for _, decl := range mod.Decls {
			if fn, ok := decl.(*desugar.Function); ok && !fn.IsExternal {
				out = append(out, l.lowerFunction(fn))
			}
		}
		if mod.Init != nil && mod.Init.Body != nil {
			out = append(out, l.lowerFunction(mod.Init))
		}
	}
	return out
}

// ── type mapping ──────────────────────────────────────────────────────────────

// LowerType maps a sema/types.Type to a minir.Type.
// Exported so callers can use it without constructing a Lowerer.
func LowerType(ty types.Type) Type {
	if ty == nil {
		return nil
	}
	switch t := ty.(type) {
	case *types.BasicType:
		switch t.Kind {
		case types.BOOLEAN:
			return primI1
		case types.BYTE, types.CHAR, types.INT8, types.INT16, types.SHORTINT,
			types.INT32, types.INTEGER, types.WCHAR, types.SET:
			return primI32
		case types.INT64, types.LONGINT:
			return primI64
		case types.REAL:
			return primF32
		case types.LONGREAL:
			return primF64
		case types.VOID, types.NIL, types.UNKNOWN:
			return nil
		default:
			return primI32
		}
	case *types.PointerType:
		base := LowerType(t.Base)
		if base == nil {
			return Ptr(primI32) // void* → ptr.i32
		}
		return Ptr(base)
	case *types.CPointerType:
		base := LowerType(t.Base)
		if base == nil {
			return Ptr(primI32)
		}
		return Ptr(base)
	case *types.ArrayType:
		length := t.Length
		if length < 0 {
			length = 0 // open array → [0 x elem]
		}
		return NewArrayType(length, LowerType(t.Elem))
	case *types.RecordType:
		var fields []RecordField
		offset := 0
		for _, f := range t.Fields {
			ft := LowerType(f.Type)
			fields = append(fields, RecordField{Name: f.Name, Type: ft, Offset: offset})
			if w := f.Type.Width(); w > 0 {
				offset += w
			} else {
				offset += 4
			}
		}
		return NewRecordType("", fields)
	case *types.NamedType:
		inner := LowerType(t.Def)
		if rec, ok := inner.(*RecordType); ok && rec.TypeName == "" {
			return NewRecordType(t.Name, rec.Fields)
		}
		return inner
	case *types.ProcedureType:
		var params []Type
		for _, p := range t.Params {
			params = append(params, LowerType(p.Type))
		}
		return &FunctionType{Params: params, Result: LowerType(t.Result)}
	case *types.StringType:
		return NewArrayType(t.Length+1, primI32)
	case *types.EnumType:
		return primI32
	default:
		return primI32
	}
}

// ── function lowering ─────────────────────────────────────────────────────────

func (l *Lowerer) lowerFunction(hirFn *desugar.Function) *Function {
	// reset per-function state
	l.blockSeq = 0
	l.labelSeq = 0
	l.varEnv = make(map[string]*Temp)
	l.constEnv = make(map[string]Value)
	l.loopExit = make(map[string]string)

	fn := &Function{
		FnName: hirFn.FnName(),
		Result: LowerType(hirFn.Result),
		Blocks: make(map[int]*Block),
	}
	l.fn = fn

	entry := l.newBlock("entry")
	fn.Entry = entry
	fn.Blocks[entry.ID] = entry
	l.switchTo(entry)

	// parameters: create alloca + store the incoming value
	for _, p := range hirFn.Params {
		pt := LowerType(p.Typ)
		if pt == nil {
			pt = primI32
		}
		param := NewTemp(p.Name, pt)
		fn.Params = append(fn.Params, param)
		addr := l.newAddrTemp(p.Name, pt)
		l.emit(&AllocaInst{Dst: addr, AllocType: pt})
		l.varEnv[p.Name] = addr
		l.emit(&StoreInst{Val: param, Addr: addr})
	}

	// locals: alloca for variables, inline for constants
	for _, local := range hirFn.Locals {
		switch d := local.(type) {
		case *desugar.Variable:
			vt := LowerType(d.Type)
			if vt == nil {
				vt = primI32
			}
			addr := l.newAddrTemp(d.Name, vt)
			l.emit(&AllocaInst{Dst: addr, AllocType: vt})
			l.varEnv[d.Name] = addr
			if d.Mangled != "" && d.Mangled != d.Name {
				l.varEnv[d.Mangled] = addr
			}
		case *desugar.Constant:
			cv := l.lowerLiteralExpr(d.Value)
			l.constEnv[d.Name] = cv
			if d.Mangled != "" {
				l.constEnv[d.Mangled] = cv
			}
		}
	}

	// body
	if hirFn.Body != nil {
		l.lowerStmts(hirFn.Body)
	}

	// ensure the current block is terminated
	if l.curBlock != nil && l.curBlock.Term == nil {
		ret := &ReturnInst{}
		l.emit(ret)
		l.curBlock.Term = ret
	}

	// exit placeholder block (required by verifier; acts as a sentinel)
	exit := l.newBlock(fn.FnName + "_exit")
	fn.Exit = exit
	fn.Blocks[exit.ID] = exit
	retExit := &ReturnInst{}
	exit.Instrs = []Instr{retExit}
	exit.Term = retExit

	// wire CFG edges from terminators
	linkCFG(fn)

	return fn
}

// ── statement lowering ────────────────────────────────────────────────────────

func (l *Lowerer) lowerStmts(cs *desugar.CompoundStmt) {
	if cs == nil {
		return
	}
	for _, s := range cs.Stmts {
		l.lowerStmt(s)
	}
}

func (l *Lowerer) lowerStmt(s desugar.Stmt) {
	switch st := s.(type) {
	case *desugar.AssignStmt:
		l.lowerAssign(st)
	case *desugar.ReturnStmt:
		l.lowerReturn(st)
	case *desugar.IfStmt:
		l.lowerIf(st)
	case *desugar.LoopStmt:
		l.lowerLoop(st)
	case *desugar.ExitStmt:
		l.lowerExit(st)
	case *desugar.CompoundStmt:
		l.lowerStmts(st)
	case *desugar.FuncCall:
		l.lowerCallStmt(st)
	case *desugar.CaseStmt:
		l.lowerCase(st)
	case *desugar.WithStmt:
		// TODO: runtime type dispatch (requires vtable support)
	}
}

func (l *Lowerer) lowerAssign(st *desugar.AssignStmt) {
	addr := l.lowerAddr(st.Left)
	val := l.lowerValue(st.Right)
	l.emit(&StoreInst{Val: val, Addr: addr})
}

func (l *Lowerer) lowerReturn(st *desugar.ReturnStmt) {
	var result *Temp
	if st.Result != nil {
		v := l.lowerValue(st.Result)
		result = l.ensureTemp(v, LowerType(st.Result.Type()))
	}
	ret := &ReturnInst{Result: result}
	l.emit(ret)
	l.curBlock.Term = ret
	// Switch to an orphan block so unreachable code after RETURN emits into
	// a block that is NOT in fn.Blocks, and is therefore ignored by the verifier.
	dead := l.newBlock(l.newLabel("dead"))
	l.switchTo(dead)
}

func (l *Lowerer) lowerIf(st *desugar.IfStmt) {
	endLabel := l.newLabel("if_end")

	type branch struct {
		cond desugar.Expr
		body *desugar.CompoundStmt
	}
	branches := []branch{{cond: st.Cond, body: st.Then}}
	for _, elif := range st.ElseIfs {
		branches = append(branches, branch{cond: elif.Cond, body: elif.Body})
	}

	for i, br := range branches {
		condVal := l.lowerValue(br.cond)
		condTemp := l.ensureTemp(condVal, primI1)

		trueLabel := l.newLabel(fmt.Sprintf("if_then_%d", i))
		var falseLabel string
		if i+1 < len(branches) {
			falseLabel = l.newLabel(fmt.Sprintf("if_elif_%d", i+1))
		} else if st.Else != nil {
			falseLabel = l.newLabel("if_else")
		} else {
			falseLabel = endLabel
		}

		cbr := &CondBrInst{Cond: condTemp, TrueLabel: trueLabel, FalseLabel: falseLabel}
		l.emit(cbr)
		l.curBlock.Term = cbr

		thenBlk := l.newBlock(trueLabel)
		l.fn.Blocks[thenBlk.ID] = thenBlk
		l.switchTo(thenBlk)
		l.lowerStmts(br.body)
		if l.curBlock.Term == nil {
			j := &JumpInst{Target: endLabel}
			l.emit(j)
			l.curBlock.Term = j
		}

		if falseLabel != endLabel {
			nextBlk := l.newBlock(falseLabel)
			l.fn.Blocks[nextBlk.ID] = nextBlk
			l.switchTo(nextBlk)
		}
	}

	if st.Else != nil {
		l.lowerStmts(st.Else)
		if l.curBlock.Term == nil {
			j := &JumpInst{Target: endLabel}
			l.emit(j)
			l.curBlock.Term = j
		}
	}

	endBlk := l.newBlock(endLabel)
	l.fn.Blocks[endBlk.ID] = endBlk
	l.switchTo(endBlk)
}

func (l *Lowerer) lowerLoop(st *desugar.LoopStmt) {
	loopLabel := l.newLabel("loop")
	exitLabel := l.newLabel("loop_exit")
	if st.Label != "" {
		loopLabel = "loop." + st.Label
		exitLabel = "loop." + st.Label + ".exit"
	}

	// unconditional jump into loop header
	j := &JumpInst{Target: loopLabel}
	l.emit(j)
	l.curBlock.Term = j

	loopBlk := l.newBlock(loopLabel)
	l.fn.Blocks[loopBlk.ID] = loopBlk
	l.switchTo(loopBlk)

	// push exit label
	prevUnlabelled := l.loopExit[""]
	if st.Label != "" {
		l.loopExit[st.Label] = exitLabel
	}
	l.loopExit[""] = exitLabel

	l.lowerStmts(st.Body)

	// back-edge (only when the current block is not already terminated)
	if l.curBlock.Term == nil {
		back := &JumpInst{Target: loopLabel}
		l.emit(back)
		l.curBlock.Term = back
	}

	// pop exit label
	l.loopExit[""] = prevUnlabelled
	if st.Label != "" {
		delete(l.loopExit, st.Label)
	}

	exitBlk := l.newBlock(exitLabel)
	l.fn.Blocks[exitBlk.ID] = exitBlk
	l.switchTo(exitBlk)
}

func (l *Lowerer) lowerExit(st *desugar.ExitStmt) {
	target := l.loopExit[st.LoopLabel]
	if target == "" {
		target = l.loopExit[""]
	}
	j := &JumpInst{Target: target}
	l.emit(j)
	l.curBlock.Term = j
	// orphan block – not added to fn.Blocks
	dead := l.newBlock(l.newLabel("dead"))
	l.switchTo(dead)
}

func (l *Lowerer) lowerCase(st *desugar.CaseStmt) {
	endLabel := l.newLabel("case_end")
	defaultLabel := endLabel
	if st.Else != nil && len(st.Else.Stmts) > 0 {
		defaultLabel = l.newLabel("case_else")
	}

	// pre-generate one body label per case
	bodyLabels := make([]string, len(st.Cases))
	for i := range bodyLabels {
		bodyLabels[i] = l.newLabel(fmt.Sprintf("case_body_%d", i))
	}

	keyVal := l.lowerValue(st.Expr)
	keyTemp := l.ensureTemp(keyVal, primI32)

	// emit per-case check chains; each case may have multiple label ranges
	for caseIdx, c := range st.Cases {
		var caseNext string
		if caseIdx+1 < len(st.Cases) {
			caseNext = l.newLabel(fmt.Sprintf("case_chk_%d", caseIdx+1))
		} else {
			caseNext = defaultLabel
		}

		for labIdx, lr := range c.Labels {
			var fallLabel string
			if labIdx+1 < len(c.Labels) {
				fallLabel = l.newLabel(fmt.Sprintf("case_%d_sub_%d", caseIdx, labIdx+1))
			} else {
				fallLabel = caseNext
			}
			l.emitCaseTest(keyTemp, lr, bodyLabels[caseIdx], fallLabel)
			if labIdx+1 < len(c.Labels) {
				blk := l.newBlock(fallLabel)
				l.fn.Blocks[blk.ID] = blk
				l.switchTo(blk)
			}
		}

		if caseIdx+1 < len(st.Cases) {
			blk := l.newBlock(caseNext)
			l.fn.Blocks[blk.ID] = blk
			l.switchTo(blk)
		}
	}

	// emit body blocks (after all check chains)
	for i, c := range st.Cases {
		bodyBlk := l.newBlock(bodyLabels[i])
		l.fn.Blocks[bodyBlk.ID] = bodyBlk
		l.switchTo(bodyBlk)
		l.lowerStmts(c.Body)
		if l.curBlock.Term == nil {
			j := &JumpInst{Target: endLabel}
			l.emit(j)
			l.curBlock.Term = j
		}
	}

	// else block
	if st.Else != nil && len(st.Else.Stmts) > 0 {
		elseBlk := l.newBlock(defaultLabel)
		l.fn.Blocks[elseBlk.ID] = elseBlk
		l.switchTo(elseBlk)
		l.lowerStmts(st.Else)
		if l.curBlock.Term == nil {
			j := &JumpInst{Target: endLabel}
			l.emit(j)
			l.curBlock.Term = j
		}
	}

	endBlk := l.newBlock(endLabel)
	l.fn.Blocks[endBlk.ID] = endBlk
	l.switchTo(endBlk)
}

func (l *Lowerer) emitCaseTest(key *Temp, lr *desugar.LabelRange, bodyLabel, fallLabel string) {
	loVal := l.lowerValue(lr.Low)
	singleton := lr.Low == lr.High
	if !singleton {
		if la, ok := lr.Low.(*desugar.Literal); ok {
			if lh, ok2 := lr.High.(*desugar.Literal); ok2 {
				singleton = la.Value == lh.Value
			}
		}
	}
	if singleton {
		cmp := NewAnonTemp(primI1)
		l.emit(&ICmpInst{Dst: cmp, Pred: "eq", Left: key, Right: loVal})
		br := &CondBrInst{Cond: cmp, TrueLabel: bodyLabel, FalseLabel: fallLabel}
		l.emit(br)
		l.curBlock.Term = br
		return
	}
	hiVal := l.lowerValue(lr.High)
	loOk := NewAnonTemp(primI1)
	hiOk := NewAnonTemp(primI1)
	both := NewAnonTemp(primI1)
	l.emit(&ICmpInst{Dst: loOk, Pred: "sge", Left: key, Right: loVal})
	l.emit(&ICmpInst{Dst: hiOk, Pred: "sle", Left: key, Right: hiVal})
	l.emit(&BinaryInst{Dst: both, Op: "and", Left: loOk, Right: hiOk})
	br := &CondBrInst{Cond: both, TrueLabel: bodyLabel, FalseLabel: fallLabel}
	l.emit(br)
	l.curBlock.Term = br
}

func (l *Lowerer) lowerCallStmt(call *desugar.FuncCall) {
	var args []Value
	for _, a := range call.Args {
		args = append(args, l.lowerValue(a))
	}
	callee := call.Func.Mangled
	if callee == "" {
		callee = call.Func.Name
	}
	l.emit(&CallInst{Callee: callee, Args: args})
}

// ── expression lowering ───────────────────────────────────────────────────────

func (l *Lowerer) lowerValue(expr desugar.Expr) Value {
	switch e := expr.(type) {
	case *desugar.Literal:
		return l.lowerLiteralExpr(e)
	case *desugar.VariableRef:
		addr := l.resolveVar(e.Mangled, e.Name)
		dst := NewTemp(e.Name, LowerType(e.SemaType))
		l.emit(&LoadInst{Dst: dst, Addr: addr})
		return dst
	case *desugar.ConstantRef:
		if cv, ok := l.constEnv[e.Name]; ok {
			return cv
		}
		if cv, ok := l.constEnv[e.Mangled]; ok {
			return cv
		}
		return l.lowerLiteralExpr(e.Value)
	case *desugar.Param:
		addr := l.resolveVar(e.Name, e.Name)
		dst := NewTemp(e.Name, LowerType(e.Typ))
		l.emit(&LoadInst{Dst: dst, Addr: addr})
		return dst
	case *desugar.BinaryExpr:
		return l.lowerBinary(e)
	case *desugar.UnaryExpr:
		return l.lowerUnary(e)
	case *desugar.FuncCall:
		return l.lowerCallExpr(e)
	case *desugar.FieldAccess:
		addr := l.lowerFieldAddr(e)
		dst := NewAnonTemp(LowerType(e.SemaType))
		l.emit(&LoadInst{Dst: dst, Addr: addr})
		return dst
	case *desugar.IndexExpr:
		addr := l.lowerIndexAddr(e)
		dst := NewAnonTemp(LowerType(e.SemaType))
		l.emit(&LoadInst{Dst: dst, Addr: addr})
		return dst
	case *desugar.DerefExpr:
		ptrVal := l.lowerValue(e.Pointer)
		ptrTemp := l.ensureTemp(ptrVal, Ptr(LowerType(e.SemaType)))
		ptrTemp.IsAddr = true
		dst := NewAnonTemp(LowerType(e.SemaType))
		l.emit(&LoadInst{Dst: dst, Addr: ptrTemp})
		return dst
	default:
		return NewConst("0", int64(0), primI32)
	}
}

// lowerAddr returns an IsAddr=true *Temp representing the address of an lvalue.
func (l *Lowerer) lowerAddr(expr desugar.Expr) *Temp {
	switch e := expr.(type) {
	case *desugar.VariableRef:
		return l.resolveVar(e.Mangled, e.Name)
	case *desugar.Param:
		return l.resolveVar(e.Name, e.Name)
	case *desugar.FieldAccess:
		return l.lowerFieldAddr(e)
	case *desugar.IndexExpr:
		return l.lowerIndexAddr(e)
	case *desugar.DerefExpr:
		ptrVal := l.lowerValue(e.Pointer)
		t := l.ensureTemp(ptrVal, Ptr(LowerType(e.SemaType)))
		t.IsAddr = true
		return t
	default:
		panic(fmt.Sprintf("lowerAddr: non-addressable expr %T", expr))
	}
}

func (l *Lowerer) lowerFieldAddr(e *desugar.FieldAccess) *Temp {
	base := l.lowerAddr(e.Record)
	ft := LowerType(e.SemaType)
	dst := l.newAddrTemp(e.Field, ft)

	var recTy *RecordType
	if pt, ok := base.Ty.(*PointerType); ok {
		recTy, _ = pt.Elem.(*RecordType)
	}
	var elemType Type = ft
	var offsets []int
	if recTy != nil {
		if idx := recTy.FieldIndex(e.Field); idx >= 0 {
			offsets = []int{idx}
		}
		elemType = recTy
	}
	l.emit(&GEPInst{Dst: dst, Base: base, ElemType: elemType, Offsets: offsets})
	return dst
}

func (l *Lowerer) lowerIndexAddr(e *desugar.IndexExpr) *Temp {
	base := l.lowerAddr(e.Array)
	et := LowerType(e.SemaType)
	dst := l.newAddrTemp("", et)

	var offset int
	if len(e.Index) > 0 {
		idxVal := l.lowerValue(e.Index[0])
		if c, ok := idxVal.(*Constant); ok {
			offset, _ = strconv.Atoi(fmt.Sprintf("%v", c.Val))
		}
	}
	var arrTy *ArrayType
	if pt, ok := base.Ty.(*PointerType); ok {
		arrTy, _ = pt.Elem.(*ArrayType)
	}
	var elemType Type = et
	if arrTy != nil {
		elemType = arrTy
	}
	l.emit(&GEPInst{Dst: dst, Base: base, ElemType: elemType, Offsets: []int{offset}})
	return dst
}

func (l *Lowerer) lowerBinary(e *desugar.BinaryExpr) Value {
	left := l.lowerValue(e.Left)
	right := l.lowerValue(e.Right)
	ty := LowerType(e.SemaType)
	if ty == nil {
		ty = primI32
	}
	switch e.Op {
	case token.EQUAL, token.NEQ, token.LESS, token.LEQ, token.GREAT, token.GEQ:
		pred := tokenToICmpPred(e.Op)
		dst := NewAnonTemp(primI1)
		if isFloatVal(left) || isFloatVal(right) {
			l.emit(&FCmpInst{ICmpInst: ICmpInst{Dst: dst, Pred: pred, Left: left, Right: right}})
		} else {
			l.emit(&ICmpInst{Dst: dst, Pred: pred, Left: left, Right: right})
		}
		return dst
	case token.AND:
		dst := NewAnonTemp(ty)
		l.emit(&BinaryInst{Dst: dst, Op: "and", Left: left, Right: right})
		return dst
	case token.OR:
		dst := NewAnonTemp(ty)
		l.emit(&BinaryInst{Dst: dst, Op: "or", Left: left, Right: right})
		return dst
	case token.IN:
		// set membership: ((1 << left) & right) != 0
		one := NewConst("1", int64(1), primI32)
		shifted := NewAnonTemp(primI32)
		l.emit(&BinaryInst{Dst: shifted, Op: "shl", Left: one, Right: left})
		andRes := NewAnonTemp(primI32)
		l.emit(&BinaryInst{Dst: andRes, Op: "and", Left: shifted, Right: right})
		zero := NewConst("0", int64(0), primI32)
		dst := NewAnonTemp(primI1)
		l.emit(&ICmpInst{Dst: dst, Pred: "ne", Left: andRes, Right: zero})
		return dst
	default:
		op := tokenToArithOp(e.Op)
		dst := NewAnonTemp(ty)
		l.emit(&BinaryInst{Dst: dst, Op: op, Left: left, Right: right})
		return dst
	}
}

func (l *Lowerer) lowerUnary(e *desugar.UnaryExpr) Value {
	operand := l.lowerValue(e.Operand)
	ty := LowerType(e.SemaType)
	if ty == nil {
		ty = primI32
	}
	switch e.Op {
	case token.NOT:
		one := NewConst("true", int64(1), primI1)
		dst := NewAnonTemp(primI1)
		l.emit(&BinaryInst{Dst: dst, Op: "xor", Left: operand, Right: one})
		return dst
	case token.MINUS:
		zero := NewConst("0", int64(0), ty)
		dst := NewAnonTemp(ty)
		l.emit(&BinaryInst{Dst: dst, Op: "sub", Left: zero, Right: operand})
		return dst
	default:
		return operand
	}
}

func (l *Lowerer) lowerCallExpr(call *desugar.FuncCall) Value {
	var args []Value
	for _, a := range call.Args {
		args = append(args, l.lowerValue(a))
	}
	callee := call.Func.Mangled
	if callee == "" {
		callee = call.Func.Name
	}
	rt := LowerType(call.RetType)
	var dst *Temp
	if rt != nil {
		dst = NewAnonTemp(rt)
	}
	l.emit(&CallInst{Dst: dst, Callee: callee, Args: args})
	if dst != nil {
		return dst
	}
	return NewConst("0", int64(0), primI32)
}

func (l *Lowerer) lowerLiteralExpr(expr desugar.Expr) Value {
	lit, ok := expr.(*desugar.Literal)
	if !ok {
		return l.lowerValue(expr)
	}
	ty := LowerType(lit.SemaType)
	if ty == nil {
		ty = primI32
	}
	switch lit.Kind {
	case token.BYTE_LIT, token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
		iv, _ := strconv.ParseInt(lit.Value, 10, 64)
		return NewConst(lit.Value, iv, ty)
	case token.REAL_LIT, token.LONGREAL_LIT:
		fv, _ := strconv.ParseFloat(lit.Value, 64)
		return NewConst(lit.Value, fv, ty)
	case token.TRUE:
		return NewConst("true", int64(1), primI1)
	case token.FALSE:
		return NewConst("false", int64(0), primI1)
	case token.CHAR_LIT, token.WCHAR_LIT:
		return NewConst(lit.Value, lit.Value, primI32)
	case token.STR_LIT, token.HEX_STR_LIT:
		return NewConst(lit.Value, lit.Value, NewArrayType(len(lit.Value)+1, primI32))
	case token.NIL:
		return NewConst("nil", int64(0), Ptr(primI32))
	default:
		iv, err := strconv.ParseInt(lit.Value, 10, 64)
		if err == nil {
			return NewConst(lit.Value, iv, ty)
		}
		return NewConst(lit.Value, lit.Value, ty)
	}
}

// ── CFG wiring ────────────────────────────────────────────────────────────────

// linkCFG inspects all block terminators and populates Preds/Succs maps.
func linkCFG(fn *Function) {
	labelMap := make(map[string]*Block, len(fn.Blocks))
	for _, b := range fn.Blocks {
		labelMap[b.Label] = b
	}
	for _, b := range fn.Blocks {
		if b.Term == nil {
			continue
		}
		var targets []string
		switch t := b.Term.(type) {
		case *JumpInst:
			targets = []string{t.Target}
		case *CondBrInst:
			targets = []string{t.TrueLabel, t.FalseLabel}
		case *SwitchInst:
			targets = []string{t.Default}
			for _, a := range t.Arms {
				targets = append(targets, a.Label)
			}
		}
		for _, lbl := range targets {
			if succ, ok := labelMap[lbl]; ok {
				b.AddSucc(succ)
				succ.AddPred(b)
			}
		}
	}
}

// ── helpers ───────────────────────────────────────────────────────────────────

func (l *Lowerer) newBlock(label string) *Block {
	id := l.blockSeq
	l.blockSeq++
	return &Block{
		ID:    id,
		Label: label,
		Preds: make(map[int]*Block),
		Succs: make(map[int]*Block),
	}
}

func (l *Lowerer) switchTo(b *Block) { l.curBlock = b }

func (l *Lowerer) emit(i Instr) { l.curBlock.Instrs = append(l.curBlock.Instrs, i) }

func (l *Lowerer) newLabel(prefix string) string {
	s := fmt.Sprintf("%s.%d", prefix, l.labelSeq)
	l.labelSeq++
	return s
}

func (l *Lowerer) newAddrTemp(name string, ty Type) *Temp {
	if ty == nil {
		ty = primI32
	}
	t := NewTemp(name, Ptr(ty))
	t.IsAddr = true
	return t
}

func (l *Lowerer) resolveVar(mangled, bare string) *Temp {
	if mangled != "" {
		if t, ok := l.varEnv[mangled]; ok {
			return t
		}
	}
	if t, ok := l.varEnv[bare]; ok {
		return t
	}
	panic(fmt.Sprintf("lowerer: undefined variable %q / %q", mangled, bare))
}

// ensureTemp coerces v to a *Temp. Constants are materialized via a trivial
// identity (add 0 / xor false) so the result is a proper SSA def.
func (l *Lowerer) ensureTemp(v Value, ty Type) *Temp {
	if t, ok := v.(*Temp); ok {
		return t
	}
	if ty == nil {
		ty = primI32
	}
	dst := NewAnonTemp(ty)
	if ty == primI1 {
		zero := NewConst("false", int64(0), primI1)
		l.emit(&BinaryInst{Dst: dst, Op: "xor", Left: v, Right: zero})
	} else {
		zero := NewConst("0", int64(0), ty)
		l.emit(&BinaryInst{Dst: dst, Op: "add", Left: v, Right: zero})
	}
	return dst
}

// isFloatVal reports whether v's type is f32 or f64.
func isFloatVal(v Value) bool {
	if v == nil {
		return false
	}
	t := v.Type()
	return t == primF32 || t == primF64
}

// tokenToICmpPred maps a comparison token to an ICmpInst predicate string.
func tokenToICmpPred(op token.Kind) string {
	switch op {
	case token.EQUAL:
		return "eq"
	case token.NEQ:
		return "ne"
	case token.LESS:
		return "slt"
	case token.LEQ:
		return "sle"
	case token.GREAT:
		return "sgt"
	case token.GEQ:
		return "sge"
	default:
		return "eq"
	}
}

// tokenToArithOp maps an arithmetic token to a BinaryInst op string.
func tokenToArithOp(op token.Kind) string {
	switch op {
	case token.PLUS:
		return "add"
	case token.MINUS:
		return "sub"
	case token.STAR:
		return "mul"
	case token.QUOT:
		return "fdiv"
	case token.DIV:
		return "div"
	case token.MOD:
		return "mod"
	default:
		return "add"
	}
}
