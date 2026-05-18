package lower

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/minir"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// ── public API ────────────────────────────────────────────────────────────────

// Lowerer translates desugared HIR functions into minir Functions.
type Lowerer struct {
	// module-level state (shared across all functions in the program)
	mod *minir.Module // the module being built; set by Lower()

	// per-function mutable state, reset by lowerFunction
	blockSeq int                    // monotone block-ID counter
	labelSeq int                    // label-suffix counter
	fn       *minir.Function        // function currently being lowered
	curBlock *minir.Block           // active basic block
	varEnv   map[string]*minir.Temp // variable name → alloca address temp (IsAddr=true)
	constEnv map[string]minir.Value // constant name → pre-computed Value
	loopExit map[string]string      // loop label → exit-block label; "" = innermost loop
	// diagnostics/context
	dctx     *compiler.Context
	reported map[string]bool // dedupe map keyed by rttiName@module
}

func New(dctx *compiler.Context) *Lowerer {
	return &Lowerer{
		dctx:     dctx,
		varEnv:   make(map[string]*minir.Temp),
		constEnv: make(map[string]minir.Value),
		loopExit: make(map[string]string),
		reported: make(map[string]bool),
	}
}

// Lower is a convenience package-level entry point that lowers a HIR program
// using a default (nil-context) Lowerer. It is equivalent to
//
//	New(nil).Lower(prog)
//
// and is provided primarily for use in tests and tooling where a full compiler
// context is not available.
func Lower(prog *desugar.Program) *minir.Program {
	return New(nil).Lower(prog)
}

// Lower lowers a desugared program using a compiler Context for diagnostic
// emission translates a desugar.Program into a *Program, producing one
// Module per desugar.Module.
//
// Within each module, lowering runs in two passes:
//
//  1. Module-scope declarations:
//     - *desugar.Variable → *GlobalVar (GlobalRef in Module.SymTab)
//     - *desugar.Constant → *GlobalConst (*Constant in Module.SymTab so
//     ConstantRef resolution returns the immediate value without a load)
//     - *desugar.Function{IsExternal} → *ExternalFunc (declaration only)
//
//  2. Function bodies: each non-external function is lowered; global variables
//     resolve directly to their *GlobalRef address.
func (l *Lowerer) Lower(prog *desugar.Program) *minir.Program {
	outProg := &minir.Program{}

	for _, hirMod := range prog.Modules {
		mod := &minir.Module{Name: hirMod.Name}
		l.mod = mod
		// expose module to LowerType so it can emit module-level constants
		currentModule = mod

		// ── pass 1: module-scope declarations ──────────────────────────
		for _, decl := range hirMod.Decls {
			switch d := decl.(type) {
			case *desugar.Variable:
				l.lowerGlobalVar(d)
			case *desugar.Constant:
				l.lowerGlobalConst(d)
			case *desugar.Function:
				if d.IsExternal {
					l.lowerExternalFunc(d)
				}
			}
		}
		// ── pass 2: function bodies ─────────────────────────────────────
		for _, decl := range hirMod.Decls {
			if fn, ok := decl.(*desugar.Function); ok && !fn.IsExternal {
				mod.Functions = append(mod.Functions, l.lowerFunction(fn))
			}
		}
		if hirMod.Init != nil && hirMod.Init.Body != nil {
			mod.Functions = append(mod.Functions, l.lowerFunction(hirMod.Init))
		}

		outProg.Modules = append(outProg.Modules, mod)
		// finished lowering this module
		currentModule = nil
	}

	return outProg
}

// ── function lowering ─────────────────────────────────────────────────────────

func (l *Lowerer) lowerFunction(hirFn *desugar.Function) *minir.Function {
	// reset per-function state
	l.blockSeq = 0
	l.labelSeq = 0
	l.varEnv = make(map[string]*minir.Temp)
	l.constEnv = make(map[string]minir.Value)
	l.loopExit = make(map[string]string)

	fn := &minir.Function{
		FnName:     hirFn.FnName(),
		Result:     LowerType(hirFn.Result),
		Blocks:     make(map[int]*minir.Block),
		ParamKinds: make([]desugar.ParamKind, 0),
	}
	l.fn = fn

	entry := l.newBlock("entry")
	fn.Entry = entry
	fn.Blocks[entry.ID] = entry
	l.switchTo(entry)

	// parameters
	l.lowerParams(hirFn.Params)

	// locals: alloca for variables, inline for constants
	l.lowerLocals(hirFn.Locals)

	// Create function exit block and result-storage temp before lowering the body
	// so return sites can jump to the canonical exit and wire Succs/Preds now.
	exit := l.newBlock(fn.FnName + "_exit")
	fn.Exit = exit
	fn.Blocks[exit.ID] = exit

	// body
	if hirFn.Body != nil {
		l.lowerStmts(hirFn.Body)
	}

	// ensure the current block is terminated: jump to the canonical exit.
	// Only wire the CFG edge when curBlock is actually registered in fn.Blocks
	// (i.e. it is a live, reachable block). Dead orphan blocks (e.g. the
	// continuation after an all-branches-return if-else) must not create a
	// spurious edge to fn.Exit.
	if l.curBlock != nil && l.curBlock.Term == nil {
		j := &minir.JumpInst{Target: fn.Exit.Label}
		l.emit(j)
		l.curBlock.Term = j
		if _, live := fn.Blocks[l.curBlock.ID]; live {
			l.curBlock.AddSucc(fn.Exit)
			fn.Exit.AddPred(l.curBlock)
		}
	}

	// wire CFG edges from other terminators (linkCFG is idempotent with our
	// per-site wiring because AddSucc/AddPred are no-ops for existing links).
	linkCFG(fn)

	// Keep the exit as a simple sentinel return; actual return values and
	// halting semantics remain in their original blocks and are handled
	// by later phases.
	l.switchTo(fn.Exit)

	ret := &minir.ReturnInst{}
	l.emit(ret)
	fn.Exit.Term = ret

	return fn
}

// lowerParams lowers parameters to SSA temps and bindings in varEnv;
// VAR/IN params get pointer types and are passed by reference.
func (l *Lowerer) lowerParams(params []*desugar.Param) {
	// parameters: ValueParam -> incoming SSA temp (element type).
	// VarParam/InParam -> incoming pointer param (address of the actual).
	for _, p := range params {
		et := LowerType(p.Typ)
		if et == nil {
			et = minir.I32()
		}

		var param *minir.Temp
		if p.Kind == desugar.VarParam || p.Kind == desugar.InParam {
			// Expect a pointer parameter for VAR/IN semantics.
			param = l.newAddrTemp(p.Name, et)
		} else {
			// Value parameter: incoming SSA temp of element type.
			param = NewTemp(p.Name, et)
		}

		l.fn.Params = append(l.fn.Params, param)

		// record original HIR param kind for downstream passes
		l.fn.ParamKinds = append(l.fn.ParamKinds, p.Kind)

		// Bind name to either the value temp or address temp accordingly.
		l.varEnv[p.Name] = param
	}
}

// lowerLocals lowers local variable declarations to stack allocas and constant declarations
// to pre-computed Values, and binds their names in varEnv and constEnv respectively.  Local
// functions and types are not handled here; they are currently unsupported in minir but may
// be added in the future.
func (l *Lowerer) lowerLocals(locals []desugar.Decl) {
	for _, local := range locals {
		switch d := local.(type) {
		case *desugar.Variable:
			vt := LowerType(d.Type)
			if vt == nil {
				vt = minir.I32()
			}

			addr := l.newAddrTemp(d.Name, vt)
			l.emit(&minir.AllocaInst{Dst: addr, AllocType: vt})

			key := d.Mangled
			if key == "" {
				key = d.Name
			}

			l.varEnv[key] = addr
			if d.Name != "" && d.Name != key {
				l.varEnv[d.Name] = addr
			}
		case *desugar.Constant:
			cv := l.lowerLiteralExpr(d.Value)
			key := d.Mangled
			if key == "" {
				key = d.Name
			}

			l.constEnv[key] = cv
			if d.Name != "" {
				l.constEnv[d.Name] = cv
			}
			//case *desugar.Function:
			//case *desugar.Type:
		}
	}
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
		l.lowerWith(st)
	}
}

func (l *Lowerer) lowerAssign(st *desugar.AssignStmt) {
	addr := l.lowerAddr(st.Left)
	val := l.lowerValue(st.Right)
	l.emit(&minir.StoreInst{Val: val, Addr: addr})
}

func (l *Lowerer) lowerReturn(st *desugar.ReturnStmt) {
	// Emit a ReturnInst in the current block. The return value (if any)
	// is materialized into a temp so the ReturnInst holds a proper SSA def.
	var result *minir.Temp
	if st.Result != nil {
		v := l.lowerValue(st.Result)
		result = l.ensureTemp(v, LowerType(st.Result.Type()))
	}
	ret := &minir.ReturnInst{Result: result}
	l.emit(ret)
	l.curBlock.Term = ret
	// Wire a CFG edge to the canonical exit block so all return paths are
	// visible in the CFG — mirrors the pattern used in builtinHalt.
	if l.fn != nil && l.fn.Exit != nil {
		l.curBlock.AddSucc(l.fn.Exit)
		l.fn.Exit.AddPred(l.curBlock)
	}
	// Switch to an orphan block so unreachable code after RETURN emits into
	// a block that is NOT in fn.Blocks, and is therefore ignored by the verifier.
	dead := l.newBlock(l.newLabel("dead"))
	l.switchTo(dead)
}

func (l *Lowerer) lowerIf(st *desugar.IfStmt) {
	endLabel := l.newLabel("if_end")
	// hasLiveJumpToEnd tracks whether any live (in-fn.Blocks) block provides
	// a path to endLabel.  When false, endBlk is unreachable and need not be
	// added to fn.Blocks.
	hasLiveJumpToEnd := false

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
		condTemp := l.ensureTemp(condVal, minir.I1())

		trueLabel := l.newLabel(fmt.Sprintf("if_then_%d", i))
		var falseLabel string
		if i+1 < len(branches) {
			falseLabel = l.newLabel(fmt.Sprintf("if_elif_%d", i+1))
		} else if st.Else != nil {
			falseLabel = l.newLabel("if_else")
		} else {
			falseLabel = endLabel
			// The CondBr's false path leads directly to endLabel from a live block.
			hasLiveJumpToEnd = true
		}

		cbr := &minir.CondBrInst{Cond: condTemp, TrueLabel: trueLabel, FalseLabel: falseLabel}
		l.emit(cbr)
		l.curBlock.Term = cbr

		thenBlk := l.newBlock(trueLabel)
		l.fn.Blocks[thenBlk.ID] = thenBlk
		l.switchTo(thenBlk)
		l.lowerStmts(br.body)
		if l.curBlock.Term == nil {
			j := &minir.JumpInst{Target: endLabel}
			l.emit(j)
			l.curBlock.Term = j
			// Count as live only when curBlock is a registered live block.
			if _, live := l.fn.Blocks[l.curBlock.ID]; live {
				hasLiveJumpToEnd = true
			}
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
			j := &minir.JumpInst{Target: endLabel}
			l.emit(j)
			l.curBlock.Term = j
			if _, live := l.fn.Blocks[l.curBlock.ID]; live {
				hasLiveJumpToEnd = true
			}
		}
	}

	endBlk := l.newBlock(endLabel)
	// Only register endBlk when at least one live predecessor jumps to it;
	// otherwise every branch terminated (e.g. all paths return) and endBlk
	// would be an orphaned, unreachable block in fn.Blocks.
	if hasLiveJumpToEnd {
		l.fn.Blocks[endBlk.ID] = endBlk
	}
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
	j := &minir.JumpInst{Target: loopLabel}
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
		back := &minir.JumpInst{Target: loopLabel}
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
	j := &minir.JumpInst{Target: target}
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
	keyTemp := l.ensureTemp(keyVal, minir.I32())

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
			j := &minir.JumpInst{Target: endLabel}
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
			j := &minir.JumpInst{Target: endLabel}
			l.emit(j)
			l.curBlock.Term = j
		}
	}

	endBlk := l.newBlock(endLabel)
	l.fn.Blocks[endBlk.ID] = endBlk
	l.switchTo(endBlk)
}

func (l *Lowerer) emitCaseTest(key *minir.Temp, lr *desugar.LabelRange, bodyLabel, fallLabel string) {
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
		cmp := NewAnonTemp(minir.I1())
		l.emit(&minir.ICmpInst{Dst: cmp, Pred: "eq", Left: key, Right: loVal})
		br := &minir.CondBrInst{Cond: cmp, TrueLabel: bodyLabel, FalseLabel: fallLabel}
		l.emit(br)
		l.curBlock.Term = br
		return
	}
	hiVal := l.lowerValue(lr.High)
	loOk := NewAnonTemp(minir.I1())
	hiOk := NewAnonTemp(minir.I1())
	both := NewAnonTemp(minir.I1())
	l.emit(&minir.ICmpInst{Dst: loOk, Pred: "sge", Left: key, Right: loVal})
	l.emit(&minir.ICmpInst{Dst: hiOk, Pred: "sle", Left: key, Right: hiVal})
	l.emit(&minir.BinaryInst{Dst: both, Op: "and", Left: loOk, Right: hiOk})
	br := &minir.CondBrInst{Cond: both, TrueLabel: bodyLabel, FalseLabel: fallLabel}
	l.emit(br)
	l.curBlock.Term = br
}

func (l *Lowerer) lowerCallStmt(call *desugar.FuncCall) {
	// Dispatch to inline builtin lowering first.
	if fn, ok := builtinLowering[strings.ToLower(call.Func.Name)]; ok {
		fn(l, call)
		return
	}

	callee := call.Func.Mangled
	if callee == "" {
		callee = call.Func.Name
	}

	// Determine which formals expect addresses (VAR/IN) when possible.
	formalAddr := l.formalAddrForCall(call)

	var args []minir.Value
	for i, a := range call.Args {
		needAddr := false
		if formalAddr != nil && i < len(formalAddr) {
			needAddr = formalAddr[i]
		}
		if needAddr {
			args = append(args, l.lowerAddr(a))
		} else {
			args = append(args, l.lowerValue(a))
		}
	}

	l.emit(&minir.CallInst{Callee: callee, Args: args})
}

// ── expression lowering ───────────────────────────────────────────────────────

func (l *Lowerer) lowerValue(expr desugar.Expr) minir.Value {
	switch e := expr.(type) {
	case *desugar.Literal:
		return l.lowerLiteralExpr(e)
	case *desugar.VariableRef:
		addr := l.resolveVar(e.Mangled, e.Name)
		dst := NewTemp(e.Name, LowerType(e.SemaType))
		l.emit(&minir.LoadInst{Dst: dst, Addr: addr})
		return dst
	case *desugar.ConstantRef:
		if cv, ok := l.constEnv[e.Name]; ok {
			return cv
		}
		if cv, ok := l.constEnv[e.Mangled]; ok {
			return cv
		}
		// Fall back to module-scope constants (stored as *Constant in SymTab).
		if l.mod != nil {
			if v, ok := l.mod.SymTab.Lookup(e.Name); ok {
				return v
			}
			if e.Mangled != "" {
				if v, ok := l.mod.SymTab.Lookup(e.Mangled); ok {
					return v
				}
			}
		}
		return l.lowerLiteralExpr(e.Value)
	case *desugar.Param:
		v := l.resolveVar(e.Name, e.Name)
		// If v is an address-like value (alloca temp or global), load from it.
		if minir.IsAddrValue(v) {
			dst := NewTemp(e.Name, LowerType(e.Typ))
			l.emit(&minir.LoadInst{Dst: dst, Addr: v})
			return dst
		}
		// Otherwise v is a value temp (ValueParam) — return it directly.
		if t, ok := v.(*minir.Temp); ok {
			return t
		}
		// Fallback: materialize via a load into a temp
		dst := NewTemp(e.Name, LowerType(e.Typ))
		l.emit(&minir.BinaryInst{Dst: dst, Op: "add", Left: v, Right: minir.NewConst("0", int64(0), dst.Type())})
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
		l.emit(&minir.LoadInst{Dst: dst, Addr: addr})
		return dst
	case *desugar.IndexExpr:
		addr := l.lowerIndexAddr(e)
		dst := NewAnonTemp(LowerType(e.SemaType))
		l.emit(&minir.LoadInst{Dst: dst, Addr: addr})
		return dst
	case *desugar.DerefExpr:
		ptrVal := l.lowerValue(e.Pointer)
		ptrTemp := l.ensureTemp(ptrVal, minir.Ptr(LowerType(e.SemaType)))
		ptrTemp.IsAddr = true
		dst := NewAnonTemp(LowerType(e.SemaType))
		l.emit(&minir.LoadInst{Dst: dst, Addr: ptrTemp})
		return dst
	case *desugar.TypeRef:
		// A type denotation used as a value (e.g. SIZE(T), DEFAULT(T)).
		// Return a zero constant of the underlying type; builtins intercept
		// before this path for type-query forms.
		ty := LowerType(e.UnderType)
		if ty == nil {
			ty = minir.I32()
		}
		return minir.NewConst("0", int64(0), ty)
	case *desugar.TypeGuardExpr:
		// Lower the subject expression to a pointer/value temp we can pass to
		// a runtime helper. We keep the helper responsible for the subtype
		// walk; on failure we emit a HaltInst to abort as required.
		subj := l.lowerValue(e.Expr)
		// ensure a pointer-like temp for passing to the runtime helper
		obj := l.ensureTemp(subj, minir.Ptr(minir.I32()))

		// Resolve target RTTI symbol name using the shared helper.
		rttiName := rttiNameForType(e.Typ)
		if rttiName == "" || l.mod == nil {
			// no runtime layout available — conservatively continue (no-op)
			return obj
		}
		// ensure the RTTI global was emitted into the module constants
		if _, ok := l.mod.SymTab.Lookup(rttiName); !ok {
			return obj
		}

		// Inline numeric-ID based subtype walk.
		// Load instance RTTI pointer from object header (vptr at offset 0).
		instRTTIPtr := NewAnonTemp(minir.Ptr(minir.I32()))
		// Treat obj as an address (pointer-to-object) for the load.
		obj.IsAddr = true
		l.emit(&minir.LoadInst{Dst: instRTTIPtr, Addr: obj})
		instRTTIPtr.IsAddr = true // it points into RTTI memory; mark addressable for GEP

		// Use the shared RTTI POD record type descriptor.
		rttiRec := rttiPODRecordType()
		// GEP to field 0 (ID)
		idAddr := l.newAddrTemp("rtti.id", rttiRec)
		l.emit(&minir.GEPInst{Dst: idAddr, Base: instRTTIPtr, ElemType: rttiRec, Offsets: []int{0}})
		instID := NewAnonTemp(minir.I64())
		l.emit(&minir.LoadInst{Dst: instID, Addr: idAddr})

		// Resolve target ID from rttiID map (assigned during LowerType emission).
		tid := rttiID[rttiName]
		if tid == 0 {
			// no id assigned: conservatively continue
			return obj
		}
		targetConst := minir.NewConst(fmt.Sprintf("%d", tid), int64(tid), minir.I64())

		// Prepare basic blocks: check-loop, mismatch, load-next-id, pass, fail.
		startLabel := l.newLabel("tg.check")
		passLabel := l.newLabel("tg.pass")
		failLabel := l.newLabel("tg.fail")
		mismatchLabel := l.newLabel("tg.mismatch")
		loadNextLabel := l.newLabel("tg.loadnext")

		// Jump into the check block
		j := &minir.JumpInst{Target: startLabel}
		l.emit(j)
		l.curBlock.Term = j

		// Allocate all blocks up-front
		startBlk := l.newBlock(startLabel)
		l.fn.Blocks[startBlk.ID] = startBlk
		mismatchBlk := l.newBlock(mismatchLabel)
		l.fn.Blocks[mismatchBlk.ID] = mismatchBlk
		loadNextBlk := l.newBlock(loadNextLabel)
		l.fn.Blocks[loadNextBlk.ID] = loadNextBlk
		failBlk := l.newBlock(failLabel)
		l.fn.Blocks[failBlk.ID] = failBlk

		// Capture predecessor label before switching away
		prevLabel := l.curBlock.Label

		// Pre-declare temps defined in later blocks (for phi referencing)
		nextRTTIPtr := NewAnonTemp(minir.Ptr(minir.I32())) // defined in mismatchBlk
		nextID := NewAnonTemp(minir.I64())                 // defined in loadNextBlk

		// ── start block: phis + compare ─────────────────────────────────────
		l.switchTo(startBlk)
		phiID := NewAnonTemp(minir.I64())
		phiPtr := NewAnonTemp(minir.Ptr(minir.I32()))
		phiPtr.IsAddr = true // mark addressable so it can be used as GEP base in mismatch
		l.emit(&minir.PhiInst{Dst: phiPtr, Args: []minir.PhiArm{
			{BlockLabel: prevLabel, Val: instRTTIPtr},
			{BlockLabel: loadNextLabel, Val: nextRTTIPtr},
		}})
		l.emit(&minir.PhiInst{Dst: phiID, Args: []minir.PhiArm{
			{BlockLabel: prevLabel, Val: instID},
			{BlockLabel: loadNextLabel, Val: nextID},
		}})
		cmp := NewAnonTemp(minir.I1())
		l.emit(&minir.ICmpInst{Dst: cmp, Pred: "eq", Left: phiID, Right: targetConst})
		cbr := &minir.CondBrInst{Cond: cmp, TrueLabel: passLabel, FalseLabel: mismatchLabel}
		l.emit(cbr)
		l.curBlock.Term = cbr

		// ── mismatch block: load base ptr, check null ────────────────────────
		l.switchTo(mismatchBlk)
		baseAddr := l.newAddrTemp("rtti.base", rttiRec)
		l.emit(&minir.GEPInst{Dst: baseAddr, Base: phiPtr, ElemType: rttiRec, Offsets: []int{1}})
		l.emit(&minir.LoadInst{Dst: nextRTTIPtr, Addr: baseAddr})
		zeroPtr := minir.NewConst("0", int64(0), minir.Ptr(minir.I32()))
		isNull := NewAnonTemp(minir.I1())
		l.emit(&minir.ICmpInst{Dst: isNull, Pred: "eq", Left: nextRTTIPtr, Right: zeroPtr})
		brNull := &minir.CondBrInst{Cond: isNull, TrueLabel: failLabel, FalseLabel: loadNextLabel}
		l.emit(brNull)
		l.curBlock.Term = brNull

		// ── loadNext block: load base ID, jump back to check ────────────────
		l.switchTo(loadNextBlk)
		nextRTTIPtr.IsAddr = true // mark addressable for GEP in this block
		baseIDAddr := l.newAddrTemp("rtti.id2", rttiRec)
		l.emit(&minir.GEPInst{Dst: baseIDAddr, Base: nextRTTIPtr, ElemType: rttiRec, Offsets: []int{0}})
		l.emit(&minir.LoadInst{Dst: nextID, Addr: baseIDAddr})
		jBack := &minir.JumpInst{Target: startLabel}
		l.emit(jBack)
		l.curBlock.Term = jBack

		// ── fail block: emit Halt ────────────────────────────────────────────
		l.switchTo(failBlk)
		halt := &minir.HaltInst{Code: minir.NewConst("1", int64(1), minir.I32())}
		l.emit(halt)
		failBlk.Term = halt
		if l.fn != nil && l.fn.Exit != nil {
			failBlk.AddSucc(l.fn.Exit)
			l.fn.Exit.AddPred(failBlk)
		}

		// ── pass block: continue ─────────────────────────────────────────────
		passBlk := l.newBlock(passLabel)
		l.fn.Blocks[passBlk.ID] = passBlk
		l.switchTo(passBlk)
		// continue, return original object
		return obj
	case *desugar.ModuleRef:
		// Module handle: represent as an opaque pointer-sized constant carrying
		// the module name (used by LDMOD/LDCMD).
		return minir.NewConst(e.Name, e.Name, minir.Ptr(minir.I32()))
	case *desugar.SetExpr:
		return l.lowerSetExpr(e)
	case *desugar.RangeExpr:
		return l.lowerRangeExpr(e)
	default:
		// Unknown/unsupported expression node — emit a warning diagnostic and
		// continue with a conservative zero value so lowering can progress.
		msg := fmt.Sprintf("lowerValue: unsupported expr type %T", expr)
		var start, end = e.Pos(), e.End()
		l.reportLoweringDiagnostic(msg, start, end)

		// Conservative fallback: return a zero constant of the expression's type
		ty := LowerType(expr.Type())
		if ty == nil {
			ty = minir.I32()
		}

		if ty == minir.I1() {
			return minir.NewConst("false", int64(0), minir.I1())
		}

		return minir.NewConst("0", int64(0), ty)
	}
}

// lowerAddr returns a Value representing the address of an lvalue.
// The result is either an IsAddr=true *Temp (stack alloca) or a *GlobalRef
// (module-scope variable / constant) — both satisfy isAddrValue.
func (l *Lowerer) lowerAddr(expr desugar.Expr) minir.Value {
	switch e := expr.(type) {
	case *desugar.VariableRef:
		return l.resolveVar(e.Mangled, e.Name)
	case *desugar.Param:
		v := l.resolveVar(e.Name, e.Name)
		// Forbid taking the address of a ValueParam (non-address temp).
		if !minir.IsAddrValue(v) {
			// Attempt to take address of a non-addressable value parameter.
			// Emit a warning and create an addressable stack slot, storing the
			// parameter value into it so lowering can continue.
			msg := fmt.Sprintf("lowerAddr: attempt to take address of value parameter '%s'", e.Name)
			l.reportLoweringDiagnostic(msg, e.StartOffset, e.EndOffset)
			pt := LowerType(e.Typ)
			if pt == nil {
				pt = minir.I32()
			}
			addr := l.newAddrTemp(e.Name, pt)
			l.emit(&minir.AllocaInst{Dst: addr, AllocType: pt})
			l.emit(&minir.StoreInst{Val: v, Addr: addr})
			return addr
		}
		return v
	case *desugar.FieldAccess:
		return l.lowerFieldAddr(e)
	case *desugar.IndexExpr:
		return l.lowerIndexAddr(e)
	case *desugar.DerefExpr:
		ptrVal := l.lowerValue(e.Pointer)
		t := l.ensureTemp(ptrVal, minir.Ptr(LowerType(e.SemaType)))
		t.IsAddr = true
		return t
	default:
		// Non-addressable lvalue expression — emit warning and materialize an
		// addressable temporary by lowering the value and storing it to a new
		// alloca so clients expecting an address can continue.
		msg := fmt.Sprintf("lowerAddr: non-addressable expr %T", expr)

		// Try to extract Start/End when expr is a desugar node
		var start, end = e.Pos(), e.End()
		l.reportLoweringDiagnostic(msg, start, end)

		// Materialize fallback address
		val := l.lowerValue(expr)
		ty := LowerType(expr.Type())
		if ty == nil {
			ty = minir.I32()
		}

		addr := l.newAddrTemp("", ty)
		l.emit(&minir.AllocaInst{Dst: addr, AllocType: ty})
		l.emit(&minir.StoreInst{Val: val, Addr: addr})

		return addr
	}
}

func (l *Lowerer) lowerFieldAddr(e *desugar.FieldAccess) *minir.Temp {
	base := l.lowerAddr(e.Record)
	ft := LowerType(e.SemaType)
	dst := l.newAddrTemp(e.Field, ft)

	var recTy *minir.RecordType
	if pt, ok := base.Type().(*minir.PointerType); ok {
		recTy, _ = pt.Elem.(*minir.RecordType)
	}

	var elemType = ft
	var offsets []int
	if recTy != nil {
		if idx := recTy.FieldIndex(e.Field); idx >= 0 {
			offsets = []int{idx}
		}
		elemType = recTy
	}

	l.emit(&minir.GEPInst{Dst: dst, Base: base, ElemType: elemType, Offsets: offsets, Indices: nil})

	return dst
}

func (l *Lowerer) lowerIndexAddr(e *desugar.IndexExpr) *minir.Temp {
	base := l.lowerAddr(e.Array)
	et := LowerType(e.SemaType)
	dst := l.newAddrTemp("", et)

	var offsets []int
	var indices []minir.Value
	if len(e.Index) == 0 {
		offsets = []int{0}
		indices = nil
	} else {
		offsets = make([]int, 0, len(e.Index))
		indices = make([]minir.Value, 0)
		for _, idxExpr := range e.Index {
			idxVal := l.lowerValue(idxExpr)
			// If the index lowered to a constant, fold it into compile-time offsets.
			if c, ok := idxVal.(minir.Constant); ok {
				// If the index lowered to a constant, fold it into compile-time offsets.
				if n, ok2 := minir.AsInt64(c); ok2 {
					offsets = append(offsets, int(n))
				} else {
					// Emit warning and conservatively treat as zero index so
					// lowering can continue.
					msg := fmt.Sprintf("lowerIndexAddr: cannot fold non-integer constant index %v", c)
					// Attempt to use idxExpr range when available.
					var start, end int
					if lit, ok := idxExpr.(*desugar.Literal); ok {
						start, end = lit.StartOffset, lit.EndOffset
					}
					l.reportLoweringDiagnostic(msg, start, end)
					offsets = append(offsets, 0)
				}
			} else {
				// Dynamic runtime index: record a zero placeholder in Offsets and
				// append the runtime Value to Indices (preserve left-to-right order).
				offsets = append(offsets, 0)
				indices = append(indices, idxVal)
			}
		}
	}

	var elemType = et
	var arrTy *minir.ArrayType
	if pt, ok := base.Type().(*minir.PointerType); ok {
		arrTy, _ = pt.Elem.(*minir.ArrayType)
	}

	if arrTy != nil {
		elemType = arrTy
	}

	l.emit(&minir.GEPInst{Dst: dst, Base: base, ElemType: elemType, Offsets: offsets, Indices: indices})

	return dst
}

func (l *Lowerer) lowerBinary(e *desugar.BinaryExpr) minir.Value {
	// IS must be handled before lowering both operands as values because the
	// right-hand side is a type denotation (TypeRef), not a runtime value.
	if e.Op == token.IS {
		left := l.lowerValue(e.Left)
		rttiName := rttiNameForType(e.Right.Type())
		// pass source offsets for diagnostic Range attachment when available
		return l.lowerISCheck(left, rttiName, e.StartOffset, e.EndOffset)
	}

	left := l.lowerValue(e.Left)
	right := l.lowerValue(e.Right)
	ty := LowerType(e.SemaType)
	if ty == nil {
		ty = minir.I32()
	}

	switch e.Op {
	case token.EQUAL, token.NEQ, token.LESS, token.LEQ, token.GREAT, token.GEQ:
		left, right = l.alignOperands(left, right)
		pred := tokenToICmpPred(e.Op)
		dst := NewAnonTemp(minir.I1())

		if isFloatVal(left) || isFloatVal(right) {
			l.emit(&minir.FCmpInst{ICmpInst: minir.ICmpInst{Dst: dst, Pred: pred, Left: left, Right: right}})
		} else {
			l.emit(&minir.ICmpInst{Dst: dst, Pred: pred, Left: left, Right: right})
		}

		return dst
	case token.AND:
		left, right = l.sameWidthOperands(left, right, ty)
		dst := NewAnonTemp(ty)
		l.emit(&minir.BinaryInst{Dst: dst, Op: "and", Left: left, Right: right})
		return dst
	case token.OR:
		left, right = l.sameWidthOperands(left, right, ty)
		dst := NewAnonTemp(ty)
		l.emit(&minir.BinaryInst{Dst: dst, Op: "or", Left: left, Right: right})
		return dst
	case token.IN:
		left, right = l.sameWidthOperands(left, right, minir.U32())
		// set membership: ((1 << left) & right) != 0
		one := minir.NewConst("1", int64(1), minir.U32())
		shifted := NewAnonTemp(minir.U32())
		l.emit(&minir.BinaryInst{Dst: shifted, Op: "shl", Left: one, Right: left})

		andRes := NewAnonTemp(minir.U32())
		l.emit(&minir.BinaryInst{Dst: andRes, Op: "and", Left: shifted, Right: right})

		zero := minir.NewConst("0", int64(0), minir.U32())
		dst := NewAnonTemp(minir.I1())
		l.emit(&minir.ICmpInst{Dst: dst, Pred: "ne", Left: andRes, Right: zero})

		return dst
	default:
		left, right = l.sameWidthOperands(left, right, ty)
		op := tokenToArithOp(e.Op)
		dst := NewAnonTemp(ty)
		l.emit(&minir.BinaryInst{Dst: dst, Op: op, Left: left, Right: right})

		return dst
	}
}

func (l *Lowerer) lowerUnary(e *desugar.UnaryExpr) minir.Value {
	operand := l.lowerValue(e.Operand)
	ty := LowerType(e.SemaType)
	if ty == nil {
		ty = minir.I32()
	}

	switch e.Op {
	case token.NOT:
		operand = l.sameType(operand, minir.I1())
		one := minir.NewConst("true", int64(1), minir.I1())
		dst := NewAnonTemp(minir.I1())
		l.emit(&minir.BinaryInst{Dst: dst, Op: "xor", Left: operand, Right: one})
		return dst
	case token.MINUS:
		operand = coerceToType(operand, ty)
		zero := minir.NewConst("0", int64(0), ty)
		dst := NewAnonTemp(ty)
		l.emit(&minir.BinaryInst{Dst: dst, Op: "sub", Left: zero, Right: operand})
		return dst
	default:
		return operand
	}
}

// sameWidthOperands coerces both operands to the same type, preferring the
// requested type when it is available. This keeps the verifier happy when one
// side is an untyped literal that defaulted to i64 during lowering.
//
// For integer target types the coercion is annotation-only (coerceToType —
// no CastInst emitted).  Float and pointer targets still go through sameType
// so that the appropriate fpext/fptrunc/bitcast instruction is emitted.
func (l *Lowerer) sameWidthOperands(left, right minir.Value, ty minir.Type) (minir.Value, minir.Value) {
	if ty != nil {
		if minir.IsIntType(ty) {
			// Implicit integer promotion: annotate without emitting a cast.
			left = coerceToType(left, ty)
			right = coerceToType(right, ty)
			return left, right
		}
		// Float / pointer: explicit cast instruction required.
		if left != nil && (left.Type() == nil || !left.Type().Equal(ty)) {
			left = l.sameType(left, ty)
		}
		if right != nil && (right.Type() == nil || !right.Type().Equal(ty)) {
			right = l.sameType(right, ty)
		}
		return left, right
	}
	return l.alignOperands(left, right)
}

// alignOperands coerces one operand to the other operand's type, preferring to
// cast constants rather than values when the widths differ.
//
// For integer-integer pairs the dominant type is chosen via dominantIntType and
// both sides are annotation-coerced (coerceToType — no CastInst emitted).
// All other combinations (float-float, int-float, pointer) fall back to the
// explicit sameType path.
func (l *Lowerer) alignOperands(left, right minir.Value) (minir.Value, minir.Value) {
	if left == nil || right == nil {
		return left, right
	}

	lt, rt := left.Type(), right.Type()
	if lt == nil || rt == nil || lt.Equal(rt) {
		return left, right
	}

	// Integer-integer: use the dominant type; annotate without emitting casts.
	if dom := dominantIntType(lt, rt); dom != nil {
		left = coerceToType(left, dom)
		right = coerceToType(right, dom)
		return left, right
	}

	// Float or mixed: fall back to explicit-cast path (prefer casting constants).
	if left.IsConst() && !right.IsConst() {
		if rt != nil {
			left = l.sameType(left, rt)
		}
		return left, right
	}

	if right.IsConst() && !left.IsConst() {
		if lt != nil {
			right = l.sameType(right, lt)
		}
		return left, right
	}

	if lt != nil {
		right = l.sameType(right, lt)
		return left, right
	}

	if rt != nil {
		left = l.sameType(left, rt)
	}

	return left, right
}

// lowerSetExpr materializes a SET literal `{e1, e2, e3..n}` as a unsigned 32-bit
// bitmask.  Each singleton element contributes bit (1 << elem); each
// RangeExpr element contributes a contiguous run of bits.  The result is
// a u32 *Temp (or a zero *Constant when the set is empty).
func (l *Lowerer) lowerSetExpr(s *desugar.SetExpr) minir.Value {
	if len(s.Elems) == 0 {
		return minir.NewConst("0", int64(0), minir.U32())
	}

	var acc minir.Value = minir.NewConst("0", int64(0), minir.U32())
	for _, elem := range s.Elems {
		var mask minir.Value
		if re, ok := elem.(*desugar.RangeExpr); ok {
			mask = l.lowerRangeExpr(re)
		} else {
			// singleton: mask = 1 << elem
			idx := l.lowerValue(elem)
			one := minir.NewConst("1", int64(1), minir.U32())
			shifted := NewAnonTemp(minir.U32())
			l.emit(&minir.BinaryInst{Dst: shifted, Op: "shl", Left: one, Right: idx})
			mask = shifted
		}
		newAcc := NewAnonTemp(minir.U32())
		l.emit(&minir.BinaryInst{Dst: newAcc, Op: "or", Left: acc, Right: mask})
		acc = newAcc
	}
	return acc
}

// lowerRangeExpr computes the bitmask for a range [low..high] in a set
// literal so that bits low, low+1, …, high are all set.
//
// Algorithm (matching the obxir lowering):
//
//	length = (high + 1) - low          // number of bits to set
//	ones   = (1 << length) - 1         // a run of `length` consecutive 1-bits
//	mask   = ones << low               // shift the run to start at bit `low`
func (l *Lowerer) lowerRangeExpr(e *desugar.RangeExpr) minir.Value {
	low := l.lowerValue(e.Low)
	var high minir.Value
	if e.High != nil {
		high = l.lowerValue(e.High)
	} else {
		high = low
	}
	one := minir.NewConst("1", int64(1), minir.U32())

	// length = (high + 1) - low
	highPlusOne := NewAnonTemp(minir.U32())
	l.emit(&minir.BinaryInst{Dst: highPlusOne, Op: "add", Left: high, Right: one})
	length := NewAnonTemp(minir.U32())
	l.emit(&minir.BinaryInst{Dst: length, Op: "sub", Left: highPlusOne, Right: low})

	// ones = (1 << length) - 1
	shifted := NewAnonTemp(minir.U32())
	l.emit(&minir.BinaryInst{Dst: shifted, Op: "shl", Left: one, Right: length})
	ones := NewAnonTemp(minir.U32())
	l.emit(&minir.BinaryInst{Dst: ones, Op: "sub", Left: shifted, Right: one})

	// mask = ones << low
	mask := NewAnonTemp(minir.U32())
	l.emit(&minir.BinaryInst{Dst: mask, Op: "shl", Left: ones, Right: low})
	return mask
}

// lowerISCheck emits an inline RTTI-based subtype-check for `obj IS T`, where
// rttiName is the RTTI symbol of the target type T.  It returns a *Temp of
// type i1 (true = obj's dynamic type IS T or an extension of T, false = not).
// After returning, l.curBlock is the merge/continuation block that the caller
// can continue emitting into.  Returns a conservative i1 false constant temp
// when RTTI metadata is unavailable.
func (l *Lowerer) lowerISCheck(obj minir.Value, rttiName string, start, end int) *minir.Temp {
	falseTemp := func() *minir.Temp {
		dst := NewAnonTemp(minir.I1())
		l.emit(&minir.BinaryInst{Dst: dst, Op: "xor",
			Left:  minir.NewConst("false", int64(0), minir.I1()),
			Right: minir.NewConst("false", int64(0), minir.I1()),
		})
		return dst
	}

	if rttiName == "" || l.mod == nil || l.fn == nil {
		return falseTemp()
	}

	if _, ok := l.mod.SymTab.Lookup(rttiName); !ok {
		// RTTI symbol has not been emitted into this module — report once and return
		l.reportMissingRTTI(rttiName, start, end)
		return falseTemp()
	}

	tid := rttiID[rttiName]
	if tid == 0 {
		// no numeric RTTI id assigned — report once and return
		l.reportMissingRTTI(rttiName, start, end)
		return falseTemp()
	}
	targetConst := minir.NewConst(fmt.Sprintf("%d", tid), int64(tid), minir.I64())

	rttiRec := rttiPODRecordType()

	// Load the RTTI pointer from the object header (vptr at offset 0).
	objTemp := l.ensureTemp(obj, minir.Ptr(minir.I32()))
	objTemp.IsAddr = true
	instRTTIPtr := NewAnonTemp(minir.Ptr(minir.I32()))
	l.emit(&minir.LoadInst{Dst: instRTTIPtr, Addr: objTemp})
	instRTTIPtr.IsAddr = true // points into RTTI memory

	// Load initial ID field.
	idAddr0 := l.newAddrTemp("is.id", rttiRec)
	l.emit(&minir.GEPInst{Dst: idAddr0, Base: instRTTIPtr, ElemType: rttiRec, Offsets: []int{0}})
	instID := NewAnonTemp(minir.I64())
	l.emit(&minir.LoadInst{Dst: instID, Addr: idAddr0})

	// Labels.
	preludeLabel := l.curBlock.Label
	startLabel := l.newLabel("is.check")
	mismatchLabel := l.newLabel("is.mismatch")
	loadNextLabel := l.newLabel("is.loadnext")
	foundLabel := l.newLabel("is.found")
	notFoundLabel := l.newLabel("is.nofound")
	mergeLabel := l.newLabel("is.merge")

	// Jump from prelude into the loop header.
	jPre := &minir.JumpInst{Target: startLabel}
	l.emit(jPre)
	l.curBlock.Term = jPre

	// Allocate all blocks up-front.
	startBlk := l.newBlock(startLabel)
	l.fn.Blocks[startBlk.ID] = startBlk
	mismatchBlk := l.newBlock(mismatchLabel)
	l.fn.Blocks[mismatchBlk.ID] = mismatchBlk
	loadNextBlk := l.newBlock(loadNextLabel)
	l.fn.Blocks[loadNextBlk.ID] = loadNextBlk
	foundBlk := l.newBlock(foundLabel)
	l.fn.Blocks[foundBlk.ID] = foundBlk
	notFoundBlk := l.newBlock(notFoundLabel)
	l.fn.Blocks[notFoundBlk.ID] = notFoundBlk
	mergeBlk := l.newBlock(mergeLabel)
	l.fn.Blocks[mergeBlk.ID] = mergeBlk

	// Pre-declare temps that will be defined in later blocks so phi arms can
	// reference them before their defining blocks are emitted.
	nextRTTIPtr := NewAnonTemp(minir.Ptr(minir.I32())) // defined in mismatchBlk
	nextID := NewAnonTemp(minir.I64())                 // defined in loadNextBlk

	// ── start block: phis + compare ─────────────────────────────────────────
	l.switchTo(startBlk)
	phiPtr := NewAnonTemp(minir.Ptr(minir.I32()))
	phiPtr.IsAddr = true // will be used as GEP base in mismatch
	phiID := NewAnonTemp(minir.I64())
	l.emit(&minir.PhiInst{Dst: phiPtr, Args: []minir.PhiArm{
		{BlockLabel: preludeLabel, Val: instRTTIPtr},
		{BlockLabel: loadNextLabel, Val: nextRTTIPtr},
	}})
	l.emit(&minir.PhiInst{Dst: phiID, Args: []minir.PhiArm{
		{BlockLabel: preludeLabel, Val: instID},
		{BlockLabel: loadNextLabel, Val: nextID},
	}})
	cmp := NewAnonTemp(minir.I1())
	l.emit(&minir.ICmpInst{Dst: cmp, Pred: "eq", Left: phiID, Right: targetConst})
	cbrStart := &minir.CondBrInst{Cond: cmp, TrueLabel: foundLabel, FalseLabel: mismatchLabel}
	l.emit(cbrStart)
	l.curBlock.Term = cbrStart

	// ── mismatch block: load base ptr, null-check ───────────────────────────
	l.switchTo(mismatchBlk)
	baseAddr := l.newAddrTemp("is.base", rttiRec)
	l.emit(&minir.GEPInst{Dst: baseAddr, Base: phiPtr, ElemType: rttiRec, Offsets: []int{1}})
	l.emit(&minir.LoadInst{Dst: nextRTTIPtr, Addr: baseAddr})
	zeroPtr := minir.NewConst("0", int64(0), minir.Ptr(minir.I32()))
	isNull := NewAnonTemp(minir.I1())
	l.emit(&minir.ICmpInst{Dst: isNull, Pred: "eq", Left: nextRTTIPtr, Right: zeroPtr})
	cbrMismatch := &minir.CondBrInst{Cond: isNull, TrueLabel: notFoundLabel, FalseLabel: loadNextLabel}
	l.emit(cbrMismatch)
	l.curBlock.Term = cbrMismatch

	// ── loadNext block: load next ID, jump back to check ───────────────────
	l.switchTo(loadNextBlk)
	nextRTTIPtr.IsAddr = true // mark addressable for GEP
	baseIDAddr := l.newAddrTemp("is.id2", rttiRec)
	l.emit(&minir.GEPInst{Dst: baseIDAddr, Base: nextRTTIPtr, ElemType: rttiRec, Offsets: []int{0}})
	l.emit(&minir.LoadInst{Dst: nextID, Addr: baseIDAddr})
	jBack := &minir.JumpInst{Target: startLabel}
	l.emit(jBack)
	l.curBlock.Term = jBack

	// ── found block: jump to merge ──────────────────────────────────────────
	l.switchTo(foundBlk)
	jFound := &minir.JumpInst{Target: mergeLabel}
	l.emit(jFound)
	foundBlk.Term = jFound

	// ── notFound block: jump to merge ───────────────────────────────────────
	l.switchTo(notFoundBlk)
	jNotFound := &minir.JumpInst{Target: mergeLabel}
	l.emit(jNotFound)
	notFoundBlk.Term = jNotFound

	// ── merge block: phi boolean result ─────────────────────────────────────
	l.switchTo(mergeBlk)
	result := NewAnonTemp(minir.I1())
	l.emit(&minir.PhiInst{Dst: result, Args: []minir.PhiArm{
		{BlockLabel: foundLabel, Val: minir.NewConst("true", int64(1), minir.I1())},
		{BlockLabel: notFoundLabel, Val: minir.NewConst("false", int64(0), minir.I1())},
	}})
	// l.curBlock is now mergeBlk; caller continues here.
	return result
}

// reportMissingRTTI emits a single warning diagnostic per missing RTTI symbol
// per-module so users are informed about absent runtime type metadata without
// spamming repeated messages.
func (l *Lowerer) reportMissingRTTI(rttiName string, start, end int) {
	if l.dctx == nil || rttiName == "" {
		return
	}
	module := ""
	if l.mod != nil {
		module = l.mod.Name
	}
	key := fmt.Sprintf("%s@%s", rttiName, module)
	if l.reported == nil {
		l.reported = make(map[string]bool)
	}
	if l.reported[key] {
		return
	}
	l.reported[key] = true

	var rng *source.Range
	if l.dctx != nil && l.dctx.Source != nil {
		rng = l.dctx.Source.Span(l.dctx.FileName, start, end)
	}
	d := diag.Diagnostic{
		Severity: diag.Warning,
		Message:  fmt.Sprintf("RTTI metadata missing for type %q", rttiName),
		Range:    rng,
	}
	if l.dctx.Reporter != nil {
		l.dctx.Reporter.Report(d)
	}
}

// reportLoweringDiagnostic emits a one-time warning diagnostic (per-module)
// for general lowering issues. Messages are deduped by exact message + module
// to avoid spamming the reporter when lowering repeats the same problem.
func (l *Lowerer) reportLoweringDiagnostic(msg string, start, end int) {
	if l.dctx == nil || msg == "" {
		return
	}
	module := ""
	if l.mod != nil {
		module = l.mod.Name
	}
	key := fmt.Sprintf("lower:%s@%s", msg, module)
	if l.reported == nil {
		l.reported = make(map[string]bool)
	}
	if l.reported[key] {
		return
	}
	l.reported[key] = true

	var rng *source.Range
	if l.dctx != nil && l.dctx.Source != nil && start != 0 && end != 0 {
		rng = l.dctx.Source.Span(l.dctx.FileName, start, end)
	}
	d := diag.Diagnostic{
		Severity: diag.Warning,
		Message:  msg,
		Range:    rng,
	}
	if l.dctx.Reporter != nil {
		l.dctx.Reporter.Report(d)
	}
}

// lowerWith lowers a WITH statement performing runtime type dispatch:
//
//	WITH v IS T1 DO body1
//	   | v IS T2 DO body2
//	   ...
//	   ELSE elseBody
//	END
//
// Each guard emits an inline IS check (RTTI chain walk).  A matched guard
// executes its body and then jumps to the common end block; an unmatched guard
// continues to the next check.  The optional ELSE clause handles all failures.
func (l *Lowerer) lowerWith(st *desugar.WithStmt) {
	if len(st.Guards) == 0 {
		// No guards: lower the else body (if any) inline and return.
		if st.Else != nil {
			l.lowerStmts(st.Else)
		}
		return
	}

	endLabel := l.newLabel("with.end")
	endBlk := l.newBlock(endLabel)
	l.fn.Blocks[endBlk.ID] = endBlk

	// Allocate body-block labels up-front so the outer check chain can reference them.
	bodyLabels := make([]string, len(st.Guards))
	for i := range st.Guards {
		bodyLabels[i] = l.newLabel(fmt.Sprintf("with.body%d", i))
	}

	for i, guard := range st.Guards {
		// ── IS check ──────────────────────────────────────────────────────
		obj := l.lowerValue(guard.Expr)
		rttiName := rttiNameForType(guard.Type.Type())

		// Determine the label for the *next* check (or else/end) when IS fails.
		var nextLabel string
		if i+1 < len(st.Guards) {
			nextLabel = l.newLabel(fmt.Sprintf("with.check%d", i+1))
		} else if st.Else != nil && len(st.Else.Stmts) > 0 {
			nextLabel = l.newLabel("with.else")
		} else {
			nextLabel = endLabel
		}

		// Emit the inline boolean IS check; we land in the merge block.
		// Pass guard Start/End so diagnostics can point to the guard span.
		cond := l.lowerISCheck(obj, rttiName, guard.StartOffset, guard.EndOffset)
		// If RTTI unavailable, cond is a false const-materialized temp; the
		// body will be unreachable.  That is the conservative safe choice.

		// Branch: IS matched → body, IS failed → next check / else / end.
		cbr := &minir.CondBrInst{Cond: cond, TrueLabel: bodyLabels[i], FalseLabel: nextLabel}
		l.emit(cbr)
		l.curBlock.Term = cbr

		// ── body block ────────────────────────────────────────────────────
		bodyBlk := l.newBlock(bodyLabels[i])
		l.fn.Blocks[bodyBlk.ID] = bodyBlk
		l.switchTo(bodyBlk)
		l.lowerStmts(guard.Body)
		// Fall through to end unless already terminated.
		if l.curBlock != nil && l.curBlock.Term == nil {
			jEnd := &minir.JumpInst{Target: endLabel}
			l.emit(jEnd)
			l.curBlock.Term = jEnd
		}

		// ── next check block (if needed) ──────────────────────────────────
		if i+1 < len(st.Guards) {
			nextBlk := l.newBlock(nextLabel)
			l.fn.Blocks[nextBlk.ID] = nextBlk
			l.switchTo(nextBlk)
		} else if st.Else != nil && len(st.Else.Stmts) > 0 {
			elseBlk := l.newBlock(nextLabel)
			l.fn.Blocks[elseBlk.ID] = elseBlk
			l.switchTo(elseBlk)
			l.lowerStmts(st.Else)
			if l.curBlock != nil && l.curBlock.Term == nil {
				jEnd := &minir.JumpInst{Target: endLabel}
				l.emit(jEnd)
				l.curBlock.Term = jEnd
			}
		}
		// If nextLabel == endLabel we fall out of the loop and land in endBlk.
	}

	// Switch to the common end block; caller continues here.
	l.switchTo(endBlk)
}

func (l *Lowerer) lowerCallExpr(call *desugar.FuncCall) minir.Value {
	// Dispatch to inline builtin lowering first.
	if fn, ok := builtinLowering[strings.ToLower(call.Func.Name)]; ok {
		v := fn(l, call)
		if v != nil {
			return v
		}

		// Builtin failed to produce a value in expression context: warn and
		// return a conservative zero value of the expected return type.
		msg := fmt.Sprintf("lowerCallExpr: builtin %q returned nil in expression context", call.Func.Name)
		l.reportLoweringDiagnostic(msg, call.StartOffset, call.EndOffset)
		rt := LowerType(call.RetType)
		if rt == nil {
			rt = minir.I32()
		}

		if rt == minir.I1() {
			return minir.NewConst("false", int64(0), minir.I1())
		}

		return minir.NewConst("0", int64(0), rt)
	}

	callee := call.Func.Mangled
	if callee == "" {
		callee = call.Func.Name
	}

	// Determine which formals expect addresses (VAR/IN) when possible.
	formalAddr := l.formalAddrForCall(call)

	var args []minir.Value
	for i, a := range call.Args {
		needAddr := false
		if formalAddr != nil && i < len(formalAddr) {
			needAddr = formalAddr[i]
		}
		if needAddr {
			args = append(args, l.lowerAddr(a))
		} else {
			args = append(args, l.lowerValue(a))
		}
	}

	var dst *minir.Temp
	rt := LowerType(call.RetType)
	if rt != nil {
		dst = NewAnonTemp(rt)
	}

	l.emit(&minir.CallInst{Dst: dst, Callee: callee, Args: args})
	if dst != nil {
		return dst
	}

	return minir.NewConst("0", int64(0), minir.I32())
}

func (l *Lowerer) lowerLiteralExpr(expr desugar.Expr) minir.Value {
	lit, ok := expr.(*desugar.Literal)
	if !ok {
		l.reportLoweringDiagnostic(fmt.Sprintf("lowerLiteralExpr: expected *desugar.Literal, got %T", expr),
			expr.Pos(), expr.End())
		return minir.NewConst("0", int64(0), minir.I32())
	}

	ty := LowerType(lit.SemaType)
	if ty == nil {
		ty = minir.I32()
	}

	switch lit.Kind {
	case token.BYTE_LIT:
		iv, err := strconv.ParseUint(lit.Value, 10, 8)
		if err != nil {
			l.reportLoweringDiagnostic(fmt.Sprintf("lowerLiteralExpr: cannot parse byte literal %q: %v",
				lit.Value, err), lit.StartOffset, lit.EndOffset)
			return minir.NewConst(lit.Value, lit.Value, ty)
		}
		return minir.NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
	case token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
		v := lit.Value
		var iv int64
		var err error
		if strings.HasSuffix(v, "h") || strings.HasSuffix(v, "H") {
			v2 := v[:len(v)-1]
			iv, err = strconv.ParseInt(v2, 16, 64)
		} else {
			iv, err = strconv.ParseInt(v, 10, 64)
		}
		if err != nil {
			return minir.NewConst(lit.Value, lit.Value, ty)
		}

		return minir.NewConst(fmt.Sprintf("%d", iv), iv, ty)
	case token.REAL_LIT, token.LONGREAL_LIT:
		v := lit.Value
		norm := strings.ReplaceAll(v, "D", "E")
		norm = strings.ReplaceAll(norm, "d", "E")
		norm = strings.ReplaceAll(norm, "S", "E")
		norm = strings.ReplaceAll(norm, "s", "E")
		fv, _ := strconv.ParseFloat(norm, 64)
		name := strconv.FormatFloat(fv, 'g', -1, 64)
		return minir.NewConst(name, fv, ty)
	case token.TRUE:
		return minir.NewConst("true", int64(1), minir.I1())
	case token.FALSE:
		return minir.NewConst("false", int64(0), minir.I1())
	case token.CHAR_LIT:
		v := lit.Value
		var iv uint64
		var err error
		if strings.HasSuffix(v, "x") || strings.HasSuffix(v, "X") {
			v2 := v[:len(v)-1]
			iv, err = strconv.ParseUint(v2, 16, 8)
		} else {
			iv, err = strconv.ParseUint(v, 16, 8)
		}

		if err != nil {
			l.reportLoweringDiagnostic(fmt.Sprintf("lowerLiteralExpr: cannot parse char literal %q: %v",
				lit.Value, err), lit.StartOffset, lit.EndOffset)
			return minir.NewConst(lit.Value, lit.Value, ty)
		}

		return minir.NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
	case token.WCHAR_LIT:
		v := lit.Value
		var iv uint64
		var err error
		if strings.HasSuffix(v, "x") || strings.HasSuffix(v, "X") {
			v2 := v[:len(v)-1]
			iv, err = strconv.ParseUint(v2, 16, 16)
		} else {
			iv, err = strconv.ParseUint(v, 16, 16)
		}

		if err != nil {
			l.reportLoweringDiagnostic(fmt.Sprintf("lowerLiteralExpr: cannot parse char literal %q: %v",
				lit.Value, err), lit.StartOffset, lit.EndOffset)
			return minir.NewConst(lit.Value, lit.Value, ty)
		}

		return minir.NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
	case token.STR_LIT, token.HEX_STR_LIT:
		return minir.NewConst(lit.Value, lit.Value, minir.NewArrayType(len(lit.Value)+1, minir.U16()))
	case token.NIL:
		return minir.NewConst("nil", int64(0), minir.Ptr(minir.Void()))
	default:
		iv, err := strconv.ParseInt(lit.Value, 10, 64)
		if err == nil {
			return minir.NewConst(lit.Value, iv, ty)
		}
		return minir.NewConst(lit.Value, lit.Value, ty)
	}
}

// ── CFG wiring ────────────────────────────────────────────────────────────────

// linkCFG inspects all block terminators and populates Preds/Succs maps.
func linkCFG(fn *minir.Function) {
	labelMap := make(map[string]*minir.Block, len(fn.Blocks))
	for _, b := range fn.Blocks {
		labelMap[b.Label] = b
	}
	for _, b := range fn.Blocks {
		if b.Term == nil {
			continue
		}
		var targets []string
		switch t := b.Term.(type) {
		case *minir.JumpInst:
			targets = []string{t.Target}
		case *minir.CondBrInst:
			targets = []string{t.TrueLabel, t.FalseLabel}
		case *minir.SwitchInst:
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

// formalAddrForCall attempts to infer which formals of a call expect
// addresses (VAR/IN semantics). It consults, in order:
//  1. lowered Function entries in the current module with ParamKinds set,
//  2. ExternalFunc signatures (pointer param → address),
//  3. HIR ProcedureType info attached to the call (fp.Kind == "VAR"/"IN").
//
// Returns nil when no information is available.
func (l *Lowerer) formalAddrForCall(call *desugar.FuncCall) []bool {
	var formalAddr []bool
	callee := call.Func.Mangled
	if callee == "" {
		callee = call.Func.Name
	}
	if l.mod != nil {
		for _, fn := range l.mod.Functions {
			if fn.FnName == callee && len(fn.ParamKinds) > 0 {
				for _, k := range fn.ParamKinds {
					formalAddr = append(formalAddr, k == desugar.VarParam || k == desugar.InParam)
				}
				return formalAddr
			}
		}
		for _, ef := range l.mod.Externals {
			if ef.Name == callee && ef.Sig != nil {
				for _, pt := range ef.Sig.Params {
					if _, ok := pt.(*minir.PointerType); ok {
						formalAddr = append(formalAddr, true)
					} else {
						formalAddr = append(formalAddr, false)
					}
				}
				return formalAddr
			}
		}
	}
	if pt, ok := call.Func.SemaType.(*types.ProcedureType); ok && pt != nil {
		for _, fp := range pt.Params {
			kind := strings.ToUpper(fp.Kind)
			if kind == "VAR" || kind == "IN" {
				formalAddr = append(formalAddr, true)
			} else {
				formalAddr = append(formalAddr, false)
			}
		}
	}
	return formalAddr
}

func (l *Lowerer) newBlock(label string) *minir.Block {
	id := l.blockSeq
	l.blockSeq++
	return &minir.Block{
		ID:     id,
		Label:  label,
		Preds:  make(map[int]*minir.Block),
		Succs:  make(map[int]*minir.Block),
		Parent: l.fn,
	}
}

func (l *Lowerer) switchTo(b *minir.Block) { l.curBlock = b }

func (l *Lowerer) emit(i minir.Instr) { l.curBlock.Instrs = append(l.curBlock.Instrs, i) }

func (l *Lowerer) newLabel(prefix string) string {
	s := fmt.Sprintf("%s.%d", prefix, l.labelSeq)
	l.labelSeq++
	return s
}

func (l *Lowerer) newAddrTemp(name string, ty minir.Type) *minir.Temp {
	if ty == nil {
		ty = minir.I32()
	}
	t := NewTemp(name, minir.Ptr(ty))
	t.IsAddr = true
	return t
}

// resolveVar resolves a variable name to its address Value.
// Lookup order:
//  1. local varEnv (alloca *Temp for function parameters and locals)
//  2. module SymTab (returns the *GlobalRef for module-scope variables, or the
//     *Constant value for module-scope constants)
//
// Panics when the name is not found in either scope.
func (l *Lowerer) resolveVar(mangled, bare string) minir.Value {
	if mangled != "" {
		if t, ok := l.varEnv[mangled]; ok {
			return t
		}
	}
	if t, ok := l.varEnv[bare]; ok {
		return t
	}
	// Fall back to module globals.
	if l.mod != nil {
		if mangled != "" {
			if v, ok := l.mod.SymTab.Lookup(mangled); ok {
				return v
			}
		}
		if v, ok := l.mod.SymTab.Lookup(bare); ok {
			return v
		}
	}
	// Undefined variable: emit a diagnostic and return a synthetic addressable
	// temporary so lowering can continue. The reporter (when present) will
	// include no source Range for this site.
	msg := fmt.Sprintf("lowerer: undefined variable %q / %q", mangled, bare)
	l.reportLoweringDiagnostic(msg, 0, 0)
	// Create an addressable alloca as a fallback.
	addr := l.newAddrTemp(bare, minir.I32())
	l.emit(&minir.AllocaInst{Dst: addr, AllocType: minir.I32()})
	return addr
}

// ensureTemp coerces v to a *Temp. Constants are materialized via a trivial
// identity (add 0 / xor false) so the result is a proper SSA def.
func (l *Lowerer) ensureTemp(v minir.Value, ty minir.Type) *minir.Temp {
	if t, ok := v.(*minir.Temp); ok {
		return t
	}
	if ty == nil {
		ty = minir.I32()
	}
	dst := NewAnonTemp(ty)
	if ty == minir.I1() {
		zero := minir.NewConst("false", int64(0), minir.I1())
		l.emit(&minir.BinaryInst{Dst: dst, Op: "xor", Left: v, Right: zero})
	} else {
		zero := minir.NewConst("0", int64(0), ty)
		l.emit(&minir.BinaryInst{Dst: dst, Op: "add", Left: v, Right: zero})
	}
	return dst
}

// isFloatVal reports whether v's type is f32 or f64.
func isFloatVal(v minir.Value) bool {
	if v == nil {
		return false
	}
	t := v.Type()
	return t == minir.F32() || t == minir.F64()
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
