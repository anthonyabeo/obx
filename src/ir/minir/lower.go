package minir

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// ── public API ────────────────────────────────────────────────────────────────

// Lowerer translates desugared HIR functions into minir Functions.
type Lowerer struct {
	// module-level state (shared across all functions in the program)
	mod *Module // the module being built; set by Lower()
	// interned string literal content -> synthesized global const
	stringGlobals map[string]*GlobalConst
	stringSeq     int

	// per-function mutable state, reset by lowerFunction
	blockSeq int               // monotone block-ID counter
	labelSeq int               // label-suffix counter
	fn       *Function         // function currently being lowered
	curBlock *Block            // active basic block
	varEnv   map[string]*Temp  // variable name → alloca address temp (IsAddr=true)
	constEnv map[string]Value  // constant name → pre-computed Value
	loopExit map[string]string // loop label → exit-block label; "" = innermost loop

	exitPreds  []string
	exitValues []Value

	// diagnostics/context
	dctx     *compiler.Context
	reported map[string]bool // dedupe map keyed by rttiName@module

	// noInitModules is an externally provided set of module names that must
	// NOT have an __init_X() call synthesised (e.g., FFI DEFINITION modules
	// compiled in a separate phase whose desugar.Module never appears in
	// prog.Modules). Merged with the locally derived defModules set inside
	// Lower(). Populated via SetNoInitModules before calling Lower.
	noInitModules map[string]bool
}

func New(dctx *compiler.Context) *Lowerer {
	return &Lowerer{
		dctx:          dctx,
		varEnv:        make(map[string]*Temp),
		constEnv:      make(map[string]Value),
		loopExit:      make(map[string]string),
		reported:      make(map[string]bool),
		stringGlobals: make(map[string]*GlobalConst),
		noInitModules: make(map[string]bool),
	}
}

// SetNoInitModules registers module names for which no __init_X() call should
// be generated. Call this before Lower() when some imported modules are known
// to be DEFINITION-only (no body) but are not visible in prog.Modules because
// they were compiled in a separate phase (e.g. FFI bindings from precompile
// Phase 1).
func (l *Lowerer) SetNoInitModules(names map[string]bool) {
	for name := range names {
		if name != "" {
			l.noInitModules[name] = true
		}
	}
}

// Lower is a convenience package-level entry point that lowers a HIR program
// using a default (nil-context) Lowerer. It is equivalent to
//
//	New(nil).Lower(prog)
//
// and is provided primarily for use in tests and tooling where a full compiler
// context is not available.
func Lower(prog *desugar.Program) *Program {
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
func (l *Lowerer) Lower(prog *desugar.Program) *Program {
	outProg := &Program{}

	// Build the combined no-init set:
	//   1. Modules in this program with Init==nil AND DLLName=="" (pure DEFINITION,
	//      no body and no DLL binding — truly has no __init_X).
	//      FFI DEFINITION modules (DLLName != "") have a backend-synthesised
	//      __init_X (from emitARM64StdioInit etc.) and must NOT be skipped.
	//   2. Externally registered names via SetNoInitModules (pure DEFINITION
	//      modules compiled in a prior phase whose desugar.Module is not in
	//      prog.Modules).
	allNoInit := make(map[string]bool)
	for k := range l.noInitModules {
		allNoInit[k] = true
	}
	for _, m := range prog.Modules {
		if m.Init == nil && m.DLLName == "" {
			allNoInit[m.Name] = true
		}
	}

	for _, hirMod := range prog.Modules {
		mod := &Module{Name: hirMod.Name, IsEntry: hirMod.IsEntry, DLLName: hirMod.DLLName}
		l.mod = mod
		l.stringGlobals = make(map[string]*GlobalConst)
		l.stringSeq = 0
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
			initFn := *hirMod.Init
			initFn.Body = prependModuleInitCalls(hirMod.Name, hirMod.Imports, hirMod.Init.Body, allNoInit)
			mod.Functions = append(mod.Functions, l.lowerFunction(&initFn))
		}

		outProg.Modules = append(outProg.Modules, mod)
		// finished lowering this module
		currentModule = nil
	}

	return outProg
}

func prependModuleInitCalls(moduleName string, imports []*ast.Import, body *desugar.CompoundStmt, defModules map[string]bool) *desugar.CompoundStmt {
	if body == nil || len(imports) == 0 {
		return body
	}

	calls := make([]desugar.Stmt, 0, len(imports))
	seen := make(map[string]bool)
	for _, imp := range imports {
		if imp == nil {
			continue
		}
		// Use the bare module name (last segment of the import path).
		// For "import S := posix.Stdio", imp.Name is "posix.Stdio" but
		// the module's own declared name is "Stdio" (last path segment).
		bareName := imp.Name
		if len(imp.ImportPath) > 0 {
			bareName = imp.ImportPath[len(imp.ImportPath)-1]
		}
		if bareName == "" || bareName == moduleName || seen[bareName] {
			continue
		}
		// DEFINITION modules (no BEGIN..END body) have no __init_ function;
		// skip generating a call for them.
		if defModules[bareName] {
			continue
		}
		seen[bareName] = true
		calls = append(calls, &desugar.FuncCall{
			Func: &desugar.FunctionRef{
				Name:    "__init_" + bareName,
				Mangled: "__init_" + bareName,
				Module:  bareName,
			},
		})
	}
	if len(calls) == 0 {
		return body
	}

	stmts := make([]desugar.Stmt, 0, len(calls)+len(body.Stmts))
	stmts = append(stmts, calls...)
	stmts = append(stmts, body.Stmts...)
	return &desugar.CompoundStmt{Stmts: stmts}
}

// ── function lowering ─────────────────────────────────────────────────────────

func (l *Lowerer) lowerFunction(hirFn *desugar.Function) *Function {
	// reset per-function state
	l.blockSeq = 0
	l.labelSeq = 0
	l.varEnv = make(map[string]*Temp)
	l.constEnv = make(map[string]Value)
	l.loopExit = make(map[string]string)
	l.exitPreds = l.exitPreds[:0]
	l.exitValues = l.exitValues[:0]

	fn := &Function{
		FnName:     hirFn.FnName(),
		Result:     LowerType(hirFn.Result),
		Blocks:     make(map[int]*Block),
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
		j := &JumpInst{Target: fn.Exit.Label}
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

	// The exit block is the canonical place where all functions return.
	// All return paths jump here via JumpInst; no direct ReturnInstr elsewhere.
	l.switchTo(fn.Exit)

	var retVal Value
	if len(l.exitValues) == 1 {
		retVal = l.exitValues[0]
	} else if len(l.exitValues) > 1 {
		// Multiple return paths with different values require a phi in the exit block.
		t := NewAnonTemp(fn.Result)
		retVal = t

		phi := &PhiInst{Dst: t}
		for i, v := range l.exitValues {
			phi.Args = append(phi.Args, PhiArm{
				BlockLabel: l.exitPreds[i],
				Val:        v,
			})
		}

		l.emit(phi)
	}

	ret := &ReturnInst{Result: retVal}
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
			msg := fmt.Sprintf("lowerParams: invalid type %s", p.Typ)
			l.reportLoweringDiagnostic(msg, p.StartOffset, p.EndOffset)

			et = I32()
		}

		var param *Temp
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
				vt = I32()
			}

			addr := l.newAddrTemp(d.Name, vt)
			l.emit(&AllocaInst{Dst: addr, AllocType: vt})

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
		case *desugar.Function:
			// TODO implement local functions lowering
		case *desugar.Type:
			// TODO implement local types lowering
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
	val = l.coerceValue(val, addr.Type().(*PointerType).Elem) // coerce RHS to match LHS pointee type
	l.emit(&StoreInst{Val: val, Addr: addr})
}

func (l *Lowerer) lowerReturn(st *desugar.ReturnStmt) {
	var result Value
	if st.Result != nil {
		result = l.lowerValue(st.Result)
		// Coerce the return value to match function return type
		if l.fn.Result != nil {
			result = l.coerceValue(result, l.fn.Result)
		}
	}

	// Record the return value and predecessor block for phi construction in the exit block.
	if result != nil && l.fn.Result != nil {
		l.exitValues = append(l.exitValues, result)
		l.exitPreds = append(l.exitPreds, l.curBlock.Label)
	}

	// Jump to the canonical exit block (the only place to exit)
	if l.fn != nil && l.fn.Exit != nil {
		jmp := &JumpInst{Target: l.fn.Exit.Label}
		l.emit(jmp)
		l.curBlock.Term = jmp

		l.curBlock.AddSucc(l.fn.Exit)
		l.fn.Exit.AddPred(l.curBlock)
	}
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

		trueLabel := l.newLabel(fmt.Sprintf("if_then_%d", i))
		var falseLabel string
		if i+1 < len(branches) {
			falseLabel = l.newLabel(fmt.Sprintf("if_elif_%d", i+1))
		} else if st.Else != nil {
			falseLabel = l.newLabel("if_else")
		} else {
			falseLabel = endLabel
		}

		cbr := &CondBrInst{Cond: condVal, TrueLabel: trueLabel, FalseLabel: falseLabel}
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
	keyTemp := l.ensureTemp(keyVal, I32())

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
	// Coerce the label constant to the same integer type as the key.
	// BYTE_LIT labels produce u8 constants, but the key may be i32, causing
	// ICmpInst type mismatches.
	if key.Type() != nil && loVal != nil && loVal.Type() != nil && !loVal.Type().Equal(key.Type()) {
		loVal = l.coerceValue(loVal, key.Type())
	}
	singleton := lr.Low == lr.High
	if !singleton {
		if la, ok := lr.Low.(*desugar.Literal); ok {
			if lh, ok2 := lr.High.(*desugar.Literal); ok2 {
				singleton = la.Value == lh.Value
			}
		}
	}
	if singleton {
		cmp := NewAnonTemp(I1())
		l.emit(&ICmpInst{Dst: cmp, Pred: "eq", Left: key, Right: loVal})
		br := &CondBrInst{Cond: cmp, TrueLabel: bodyLabel, FalseLabel: fallLabel}
		l.emit(br)
		l.curBlock.Term = br
		return
	}
	hiVal := l.lowerValue(lr.High)
	if key.Type() != nil && hiVal != nil && hiVal.Type() != nil && !hiVal.Type().Equal(key.Type()) {
		hiVal = l.coerceValue(hiVal, key.Type())
	}
	loOk := NewAnonTemp(I1())
	hiOk := NewAnonTemp(I1())
	both := NewAnonTemp(I1())
	l.emit(&ICmpInst{Dst: loOk, Pred: "sge", Left: key, Right: loVal})
	l.emit(&ICmpInst{Dst: hiOk, Pred: "sle", Left: key, Right: hiVal})
	l.emit(&BinaryInst{Dst: both, Op: "and", Left: loOk, Right: hiOk})
	br := &CondBrInst{Cond: both, TrueLabel: bodyLabel, FalseLabel: fallLabel}
	l.emit(br)
	l.curBlock.Term = br
}

func (l *Lowerer) lowerCallStmt(call *desugar.FuncCall) {
	// Dispatch to inline builtin lowering first.
	// Only dispatch predeclared (unqualified, non-external) functions.
	// Module-qualified calls (e.g. Math.Floor) and FFI calls (e.g. M.floor)
	// share names with builtins but must not be intercepted here.
	if !call.Func.IsExternal && call.Func.Module == "" {
		if fn, ok := builtinLowering[strings.ToLower(call.Func.Name)]; ok {
			fn(l, call)
			return
		}
	}

	callee := call.Func.Mangled
	if callee == "" {
		callee = call.Func.Name
	}

	// Determine which formals expect addresses (VAR/IN) when possible.
	formalAddr := l.formalAddrForCall(call)

	var args []Value
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
		if IsAddrValue(v) {
			dst := NewTemp(e.Name, LowerType(e.Typ))
			l.emit(&LoadInst{Dst: dst, Addr: v})
			return dst
		}
		// Otherwise v is a value temp (ValueParam) — return it directly.
		if t, ok := v.(*Temp); ok {
			return t
		}
		// Fallback: materialize via a load into a temp
		dst := NewTemp(e.Name, LowerType(e.Typ))
		l.emit(&BinaryInst{Dst: dst, Op: "add", Left: v, Right: NewConst("0", int64(0), dst.Type())})
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
	case *desugar.TypeRef:
		// A type denotation used as a value (e.g. SIZE(T), DEFAULT(T)).
		// Return a zero constant of the underlying type; builtins intercept
		// before this path for type-query forms.
		ty := LowerType(e.UnderType)
		if ty == nil {
			ty = I32()
		}
		return NewConst("0", int64(0), ty)
	case *desugar.TypeGuardExpr:
		// Lower the subject expression to a pointer/value temp we can pass to
		// a runtime helper. We keep the helper responsible for the subtype
		// walk; on failure we emit a HaltInst to abort as required.
		subj := l.lowerValue(e.Expr)
		// ensure a pointer-like temp for passing to the runtime helper
		obj := l.ensureTemp(subj, Ptr(I32()))

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
		instRTTIPtr := NewAnonTemp(Ptr(I32()))
		// Treat obj as an address (pointer-to-object) for the load.
		obj.IsAddr = true
		l.emit(&LoadInst{Dst: instRTTIPtr, Addr: obj})
		instRTTIPtr.IsAddr = true // it points into RTTI memory; mark addressable for GEP

		// Use the shared RTTI POD record type descriptor.
		rttiRec := rttiPODRecordType()
		// GEP to field 0 (ID)
		idAddr := l.newAddrTemp("rtti.id", rttiRec)
		l.emit(&GEPInst{Dst: idAddr, Base: instRTTIPtr, ElemType: rttiRec, Offsets: []int{0}})
		instID := NewAnonTemp(I64())
		l.emit(&LoadInst{Dst: instID, Addr: idAddr})

		// Resolve target ID from rttiID map (assigned during LowerType emission).
		tid := rttiID[rttiName]
		if tid == 0 {
			// no id assigned: conservatively continue
			return obj
		}
		targetConst := NewConst(fmt.Sprintf("%d", tid), int64(tid), I64())

		// Prepare basic blocks: check-loop, mismatch, load-next-id, pass, fail.
		startLabel := l.newLabel("tg.check")
		passLabel := l.newLabel("tg.pass")
		failLabel := l.newLabel("tg.fail")
		mismatchLabel := l.newLabel("tg.mismatch")
		loadNextLabel := l.newLabel("tg.loadnext")

		// Jump into the check block
		j := &JumpInst{Target: startLabel}
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
		nextRTTIPtr := NewAnonTemp(Ptr(I32())) // defined in mismatchBlk
		nextID := NewAnonTemp(I64())           // defined in loadNextBlk

		// ── start block: phis + compare ─────────────────────────────────────
		l.switchTo(startBlk)
		phiID := NewAnonTemp(I64())
		phiPtr := NewAnonTemp(Ptr(I32()))
		phiPtr.IsAddr = true // mark addressable so it can be used as GEP base in mismatch
		l.emit(&PhiInst{Dst: phiPtr, Args: []PhiArm{
			{BlockLabel: prevLabel, Val: instRTTIPtr},
			{BlockLabel: loadNextLabel, Val: nextRTTIPtr},
		}})
		l.emit(&PhiInst{Dst: phiID, Args: []PhiArm{
			{BlockLabel: prevLabel, Val: instID},
			{BlockLabel: loadNextLabel, Val: nextID},
		}})
		cmp := NewAnonTemp(I1())
		l.emit(&ICmpInst{Dst: cmp, Pred: "eq", Left: phiID, Right: targetConst})
		cbr := &CondBrInst{Cond: cmp, TrueLabel: passLabel, FalseLabel: mismatchLabel}
		l.emit(cbr)
		l.curBlock.Term = cbr

		// ── mismatch block: load base ptr, check null ────────────────────────
		l.switchTo(mismatchBlk)
		baseAddr := l.newAddrTemp("rtti.base", rttiRec)
		l.emit(&GEPInst{Dst: baseAddr, Base: phiPtr, ElemType: rttiRec, Offsets: []int{1}})
		l.emit(&LoadInst{Dst: nextRTTIPtr, Addr: baseAddr})
		zeroPtr := NewConst("0", int64(0), Ptr(I32()))
		isNull := NewAnonTemp(I1())
		l.emit(&ICmpInst{Dst: isNull, Pred: "eq", Left: nextRTTIPtr, Right: zeroPtr})
		brNull := &CondBrInst{Cond: isNull, TrueLabel: failLabel, FalseLabel: loadNextLabel}
		l.emit(brNull)
		l.curBlock.Term = brNull

		// ── loadNext block: load base ID, jump back to check ────────────────
		l.switchTo(loadNextBlk)
		nextRTTIPtr.IsAddr = true // mark addressable for GEP in this block
		baseIDAddr := l.newAddrTemp("rtti.id2", rttiRec)
		l.emit(&GEPInst{Dst: baseIDAddr, Base: nextRTTIPtr, ElemType: rttiRec, Offsets: []int{0}})
		l.emit(&LoadInst{Dst: nextID, Addr: baseIDAddr})
		jBack := &JumpInst{Target: startLabel}
		l.emit(jBack)
		l.curBlock.Term = jBack

		// ── fail block: emit Halt ────────────────────────────────────────────
		l.switchTo(failBlk)
		halt := &HaltInst{Code: NewConst("1", int64(1), I32())}
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
		return NewConst(e.Name, e.Name, Ptr(I32()))
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
			ty = I32()
		}

		if ty == I1() {
			return NewConst("false", int64(0), I1())
		}

		return NewConst("0", int64(0), ty)
	}
}

// lowerAddr returns a Value representing the address of an lvalue.
// The result is either an IsAddr=true *Temp (stack alloca) or a *GlobalRef
// (module-scope variable / constant) — both satisfy isAddrValue.
func (l *Lowerer) lowerAddr(expr desugar.Expr) Value {
	switch e := expr.(type) {
	case *desugar.VariableRef:
		return l.resolveVar(e.Mangled, e.Name)
	case *desugar.Param:
		v := l.resolveVar(e.Name, e.Name)
		// Forbid taking the address of a ValueParam (non-address temp).
		if !IsAddrValue(v) {
			// Attempt to take the address of a non-addressable value parameter.
			// Emit a warning and create an addressable stack slot, storing the
			// parameter value into it so lowering can continue.
			msg := fmt.Sprintf("lowerAddr: attempt to take address of value parameter '%s'", e.Name)
			l.reportLoweringDiagnostic(msg, e.StartOffset, e.EndOffset)

			pt := LowerType(e.Typ)
			if pt == nil {
				pt = I32()
			}
			addr := l.newAddrTemp(e.Name, pt)
			l.emit(&AllocaInst{Dst: addr, AllocType: pt})
			l.emit(&StoreInst{Val: v, Addr: addr})
			return addr
		}
		return v
	case *desugar.FieldAccess:
		return l.lowerFieldAddr(e)
	case *desugar.IndexExpr:
		return l.lowerIndexAddr(e)
	case *desugar.DerefExpr:
		ptrVal := l.lowerValue(e.Pointer)
		t := l.ensureTemp(ptrVal, Ptr(LowerType(e.SemaType)))
		t.IsAddr = true
		return t
	case *desugar.TypeGuardExpr:
		// TODO implement support for type guard address expressions
		panic("lowerAddr: type guard address expressions not implemented")
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
			ty = I32()
		}

		addr := l.newAddrTemp("", ty)
		l.emit(&AllocaInst{Dst: addr, AllocType: ty})
		l.emit(&StoreInst{Val: val, Addr: addr})

		return addr
	}
}

func (l *Lowerer) lowerFieldAddr(e *desugar.FieldAccess) *Temp {
	base := l.lowerAddr(e.Record)
	ft := LowerType(e.SemaType)
	dst := l.newAddrTemp(e.Field, ft)

	// Resolve the RecordType that the base pointer points at.
	// base.Type() is always a PointerType because lowerAddr always yields an
	// address.  Two cases arise:
	//
	//   1. Direct record variable: var tm: Rec → base = ptr.RecordType
	//      pt.Elem is a RecordType → straightforward GEP.
	//
	//   2. Pointer-to-record variable (CPOINTER / POINTER TO Rec):
	//      var p: Ptr → alloca gives base of type ptr.(ptr.RecordType).
	//      We must emit a LoadInst to dereference the pointer and get a
	//      base of type ptr.RecordType before doing the GEP.
	//
	var recTy *RecordType
	if pt, ok := base.Type().(*PointerType); ok {
		switch inner := pt.Elem.(type) {
		case *RecordType:
			// Case 1: base is directly ptr.RecordType.
			recTy = inner
		case *PointerType:
			// Case 2: base is ptr.(ptr.RecordType) — auto-deref needed.
			if inner.Elem != nil {
				if rec, ok2 := inner.Elem.(*RecordType); ok2 {
					// Load the pointer to get ptr.RecordType.
					loaded := NewAnonTemp(inner)
					loaded.IsAddr = true
					l.emit(&LoadInst{Dst: loaded, Addr: base})
					base = loaded
					recTy = rec
				}
			}
		}
	}

	var elemType Type = ft
	var offsets []int
	if recTy != nil {
		if idx := recTy.FieldIndex(e.Field); idx >= 0 {
			offsets = []int{idx}
		}
		elemType = recTy
	}

	l.emit(&GEPInst{Dst: dst, Base: base, ElemType: elemType, Offsets: offsets, Indices: nil})

	return dst
}

func (l *Lowerer) lowerIndexAddr(e *desugar.IndexExpr) *Temp {
	base := l.lowerAddr(e.Array)
	et := LowerType(e.SemaType)
	dst := l.newAddrTemp("", et)

	// Detect an OpenArrayType base.  When present, we must skip past the
	// dope-vector header (NDims words of size wordSize) before indexing into
	// the data region.
	var headerNDims int
	var innerElemType Type = et
	if pt, ok := base.Type().(*PointerType); ok {
		if oa, ok := pt.Elem.(*OpenArrayType); ok {
			headerNDims = oa.NDims
			if oa.Elem != nil {
				innerElemType = oa.Elem
			}
		}
	}

	var offsets []int
	var indices []Value
	if len(e.Index) == 0 {
		offsets = []int{0}
		indices = nil
	} else {
		offsets = make([]int, 0, len(e.Index))
		indices = make([]Value, 0)
		for _, idxExpr := range e.Index {
			idxVal := l.lowerValue(idxExpr)
			// If the index lowered to a constant, fold it into compile-time offsets.
			if c, ok := idxVal.(Constant); ok {
				// If the index lowered to a constant, fold it into compile-time offsets.
				if n, ok2 := AsInt64(c); ok2 {
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

	// For open arrays, emit a preliminary GEP that skips past the
	// NDims-word dope-vector header, then use the resulting pointer as the
	// base for the actual element GEP.
	effectiveBase := base
	effectiveElemType := innerElemType
	if headerNDims > 0 {
		dt := l.dimType()
		dataStart := l.newAddrTemp("", innerElemType)
		l.emit(&GEPInst{
			Dst:      dataStart,
			Base:     base,
			ElemType: dt,
			Offsets:  []int{headerNDims},
		})
		effectiveBase = dataStart
	}

	// For fixed arrays keep the existing behaviour: wrap the element type in
	// the array type so the GEP uses element-stride arithmetic.
	if headerNDims == 0 {
		if pt, ok := base.Type().(*PointerType); ok {
			if arrTy, ok := pt.Elem.(*ArrayType); ok {
				effectiveElemType = arrTy
			}
		}
	}

	l.emit(&GEPInst{Dst: dst, Base: effectiveBase, ElemType: effectiveElemType, Offsets: offsets, Indices: indices})

	return dst
}

func (l *Lowerer) lowerBinary(e *desugar.BinaryExpr) Value {
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
		ty = I32()
	}

	switch e.Op {
	case token.EQUAL, token.NEQ, token.LESS, token.LEQ, token.GREAT, token.GEQ:
		left, right = l.alignOperands(left, right)
		pred := tokenToICmpPred(e.Op)
		dst := NewAnonTemp(I1())

		if isFloatVal(left) || isFloatVal(right) {
			l.emit(&FCmpInst{ICmpInst: ICmpInst{Dst: dst, Pred: pred, Left: left, Right: right}})
		} else {
			l.emit(&ICmpInst{Dst: dst, Pred: pred, Left: left, Right: right})
		}

		return dst
	case token.AND:
		left, right = l.sameWidthOperands(left, right, ty)
		dst := NewAnonTemp(ty)
		l.emit(&BinaryInst{Dst: dst, Op: "and", Left: left, Right: right})
		return dst
	case token.OR:
		left, right = l.sameWidthOperands(left, right, ty)
		dst := NewAnonTemp(ty)
		l.emit(&BinaryInst{Dst: dst, Op: "or", Left: left, Right: right})
		return dst
	case token.IN:
		left, right = l.sameWidthOperands(left, right, U32())
		// set membership: ((1 << left) & right) != 0
		one := NewConst("1", int64(1), U32())
		shifted := NewAnonTemp(U32())
		l.emit(&BinaryInst{Dst: shifted, Op: "shl", Left: one, Right: left})

		andRes := NewAnonTemp(U32())
		l.emit(&BinaryInst{Dst: andRes, Op: "and", Left: shifted, Right: right})

		zero := NewConst("0", int64(0), U32())
		dst := NewAnonTemp(I1())
		l.emit(&ICmpInst{Dst: dst, Pred: "ne", Left: andRes, Right: zero})

		return dst
	default:
		left, right = l.sameWidthOperands(left, right, ty)
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
		ty = I32()
	}

	switch e.Op {
	case token.NOT:
		operand = l.sameType(operand, I1())
		one := NewConst("true", int64(1), I1())
		dst := NewAnonTemp(I1())
		l.emit(&BinaryInst{Dst: dst, Op: "xor", Left: operand, Right: one})
		return dst
	case token.MINUS:
		operand = coerceToType(operand, ty)
		zero := NewConst("0", int64(0), ty)
		dst := NewAnonTemp(ty)
		l.emit(&BinaryInst{Dst: dst, Op: "sub", Left: zero, Right: operand})
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
func (l *Lowerer) sameWidthOperands(left, right Value, ty Type) (Value, Value) {
	if ty != nil {
		if IsIntType(ty) {
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
func (l *Lowerer) alignOperands(left, right Value) (Value, Value) {
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
		left = l.sameType(left, rt)
		return left, right
	}

	if right.IsConst() && !left.IsConst() {
		right = l.sameType(right, lt)
		return left, right
	}

	right = l.sameType(right, lt)

	return left, right
}

// coerceValue coerces a single value to a target type, following the same
// strategy as sameWidthOperands for single operands.
//
// For integer target types the coercion is annotation-only (coerceToType —
// no CastInst emitted).  Float and pointer targets still go through sameType
// so that the appropriate fpext/fptrunc/bitcast instruction is emitted.
//
// This is particularly useful for ensuring return values match the declared
// function return type, but may be used anywhere a single value must match
// a target type for type safety.
//
// Returns val unchanged if either val or ty is nil, or if val's type already
// matches ty.
func (l *Lowerer) coerceValue(val Value, ty Type) Value {
	if val == nil || ty == nil {
		return val
	}

	vty := val.Type()
	if vty != nil && vty.Equal(ty) {
		return val
	}

	// Integer target: implicit coercion via annotation only
	if IsIntType(ty) {
		return coerceToType(val, ty)
	}

	// Float / pointer: explicit cast instruction required
	return l.sameType(val, ty)
}

// lowerSetExpr materializes a SET literal `{e1, e2, e3..n}` as a unsigned 32-bit
// bitmask.  Each singleton element contributes bit (1 << elem); each
// RangeExpr element contributes a contiguous run of bits.  The result is
// a u32 *Temp (or a zero *Constant when the set is empty).
func (l *Lowerer) lowerSetExpr(s *desugar.SetExpr) Value {
	if len(s.Elems) == 0 {
		return NewConst("0", int64(0), U32())
	}

	var acc Value = NewConst("0", int64(0), U32())
	for _, elem := range s.Elems {
		var mask Value
		if re, ok := elem.(*desugar.RangeExpr); ok {
			mask = l.lowerRangeExpr(re)
		} else {
			// singleton: mask = 1 << elem
			idx := l.lowerValue(elem)
			one := NewConst("1", int64(1), U32())
			shifted := NewAnonTemp(U32())
			l.emit(&BinaryInst{Dst: shifted, Op: "shl", Left: one, Right: idx})
			mask = shifted
		}
		newAcc := NewAnonTemp(U32())
		l.emit(&BinaryInst{Dst: newAcc, Op: "or", Left: acc, Right: mask})
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
func (l *Lowerer) lowerRangeExpr(e *desugar.RangeExpr) Value {
	low := l.lowerValue(e.Low)
	var high Value
	if e.High != nil {
		high = l.lowerValue(e.High)
	} else {
		high = low
	}
	one := NewConst("1", int64(1), U32())

	// length = (high + 1) - low
	highPlusOne := NewAnonTemp(U32())
	l.emit(&BinaryInst{Dst: highPlusOne, Op: "add", Left: high, Right: one})
	length := NewAnonTemp(U32())
	l.emit(&BinaryInst{Dst: length, Op: "sub", Left: highPlusOne, Right: low})

	// ones = (1 << length) - 1
	shifted := NewAnonTemp(U32())
	l.emit(&BinaryInst{Dst: shifted, Op: "shl", Left: one, Right: length})
	ones := NewAnonTemp(U32())
	l.emit(&BinaryInst{Dst: ones, Op: "sub", Left: shifted, Right: one})

	// mask = ones << low
	mask := NewAnonTemp(U32())
	l.emit(&BinaryInst{Dst: mask, Op: "shl", Left: ones, Right: low})
	return mask
}

// lowerISCheck emits an inline RTTI-based subtype-check for `obj IS T`, where
// rttiName is the RTTI symbol of the target type T.  It returns a *Temp of
// type i1 (true = obj's dynamic type IS T or an extension of T, false = not).
// After returning, l.curBlock is the merge/continuation block that the caller
// can continue emitting into.  Returns a conservative i1 false constant temp
// when RTTI metadata is unavailable.
func (l *Lowerer) lowerISCheck(obj Value, rttiName string, start, end int) *Temp {
	falseTemp := func() *Temp {
		dst := NewAnonTemp(I1())
		l.emit(&BinaryInst{Dst: dst, Op: "xor",
			Left:  NewConst("false", int64(0), I1()),
			Right: NewConst("false", int64(0), I1()),
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
	targetConst := NewConst(fmt.Sprintf("%d", tid), int64(tid), I64())

	rttiRec := rttiPODRecordType()

	// Load the RTTI pointer from the object header (vptr at offset 0).
	objTemp := l.ensureTemp(obj, Ptr(I32()))
	objTemp.IsAddr = true
	instRTTIPtr := NewAnonTemp(Ptr(I32()))
	l.emit(&LoadInst{Dst: instRTTIPtr, Addr: objTemp})
	instRTTIPtr.IsAddr = true // points into RTTI memory

	// Load initial ID field.
	idAddr0 := l.newAddrTemp("is.id", rttiRec)
	l.emit(&GEPInst{Dst: idAddr0, Base: instRTTIPtr, ElemType: rttiRec, Offsets: []int{0}})
	instID := NewAnonTemp(I64())
	l.emit(&LoadInst{Dst: instID, Addr: idAddr0})

	// Labels.
	preludeLabel := l.curBlock.Label
	startLabel := l.newLabel("is.check")
	mismatchLabel := l.newLabel("is.mismatch")
	loadNextLabel := l.newLabel("is.loadnext")
	foundLabel := l.newLabel("is.found")
	notFoundLabel := l.newLabel("is.nofound")
	mergeLabel := l.newLabel("is.merge")

	// Jump from prelude into the loop header.
	jPre := &JumpInst{Target: startLabel}
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
	nextRTTIPtr := NewAnonTemp(Ptr(I32())) // defined in mismatchBlk
	nextID := NewAnonTemp(I64())           // defined in loadNextBlk

	// ── start block: phis + compare ─────────────────────────────────────────
	l.switchTo(startBlk)
	phiPtr := NewAnonTemp(Ptr(I32()))
	phiPtr.IsAddr = true // will be used as GEP base in mismatch
	phiID := NewAnonTemp(I64())
	l.emit(&PhiInst{Dst: phiPtr, Args: []PhiArm{
		{BlockLabel: preludeLabel, Val: instRTTIPtr},
		{BlockLabel: loadNextLabel, Val: nextRTTIPtr},
	}})
	l.emit(&PhiInst{Dst: phiID, Args: []PhiArm{
		{BlockLabel: preludeLabel, Val: instID},
		{BlockLabel: loadNextLabel, Val: nextID},
	}})
	cmp := NewAnonTemp(I1())
	l.emit(&ICmpInst{Dst: cmp, Pred: "eq", Left: phiID, Right: targetConst})
	cbrStart := &CondBrInst{Cond: cmp, TrueLabel: foundLabel, FalseLabel: mismatchLabel}
	l.emit(cbrStart)
	l.curBlock.Term = cbrStart

	// ── mismatch block: load base ptr, null-check ───────────────────────────
	l.switchTo(mismatchBlk)
	baseAddr := l.newAddrTemp("is.base", rttiRec)
	l.emit(&GEPInst{Dst: baseAddr, Base: phiPtr, ElemType: rttiRec, Offsets: []int{1}})
	l.emit(&LoadInst{Dst: nextRTTIPtr, Addr: baseAddr})
	zeroPtr := NewConst("0", int64(0), Ptr(I32()))
	isNull := NewAnonTemp(I1())
	l.emit(&ICmpInst{Dst: isNull, Pred: "eq", Left: nextRTTIPtr, Right: zeroPtr})
	cbrMismatch := &CondBrInst{Cond: isNull, TrueLabel: notFoundLabel, FalseLabel: loadNextLabel}
	l.emit(cbrMismatch)
	l.curBlock.Term = cbrMismatch

	// ── loadNext block: load next ID, jump back to check ───────────────────
	l.switchTo(loadNextBlk)
	nextRTTIPtr.IsAddr = true // mark addressable for GEP
	baseIDAddr := l.newAddrTemp("is.id2", rttiRec)
	l.emit(&GEPInst{Dst: baseIDAddr, Base: nextRTTIPtr, ElemType: rttiRec, Offsets: []int{0}})
	l.emit(&LoadInst{Dst: nextID, Addr: baseIDAddr})
	jBack := &JumpInst{Target: startLabel}
	l.emit(jBack)
	l.curBlock.Term = jBack

	// ── found block: jump to merge ──────────────────────────────────────────
	l.switchTo(foundBlk)
	jFound := &JumpInst{Target: mergeLabel}
	l.emit(jFound)
	foundBlk.Term = jFound

	// ── notFound block: jump to merge ───────────────────────────────────────
	l.switchTo(notFoundBlk)
	jNotFound := &JumpInst{Target: mergeLabel}
	l.emit(jNotFound)
	notFoundBlk.Term = jNotFound

	// ── merge block: phi boolean result ─────────────────────────────────────
	l.switchTo(mergeBlk)
	result := NewAnonTemp(I1())
	l.emit(&PhiInst{Dst: result, Args: []PhiArm{
		{BlockLabel: foundLabel, Val: NewConst("true", int64(1), I1())},
		{BlockLabel: notFoundLabel, Val: NewConst("false", int64(0), I1())},
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
		cbr := &CondBrInst{Cond: cond, TrueLabel: bodyLabels[i], FalseLabel: nextLabel}
		l.emit(cbr)
		l.curBlock.Term = cbr

		// ── body block ────────────────────────────────────────────────────
		bodyBlk := l.newBlock(bodyLabels[i])
		l.fn.Blocks[bodyBlk.ID] = bodyBlk
		l.switchTo(bodyBlk)
		l.lowerStmts(guard.Body)
		// Fall through to end unless already terminated.
		if l.curBlock != nil && l.curBlock.Term == nil {
			jEnd := &JumpInst{Target: endLabel}
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
				jEnd := &JumpInst{Target: endLabel}
				l.emit(jEnd)
				l.curBlock.Term = jEnd
			}
		}
		// If nextLabel == endLabel we fall out of the loop and land in endBlk.
	}

	// Switch to the common end block; caller continues here.
	l.switchTo(endBlk)
}

// lowerSysAdr lowers a system.adr(x) call, returning a value that carries the
// machine address of x as an INTEGER (i64 / ptr-sized).
//
// Three cases are handled:
//
//  1. VAR / IN parameter — the param temp already holds the incoming pointer
//     (the caller passed the address). There is no stack slot to take the
//     address of; we simply bitcast the pointer value to i64 directly.
//
//  2. String literal — the literal is interned as a module-scope global
//     constant. AddrInstr over the GlobalRef lets isel emit the appropriate
//     PC-relative address sequence (e.g. adrp/add on arm64).
//
//  3. Local variable, global variable, or constant reference — lowerAddr
//     returns the alloca / GlobalRef, and AddrInstr lets isel compute the
//     stack-slot or data-section address.
func (l *Lowerer) lowerSysAdr(arg desugar.Expr) Value {
	switch e := arg.(type) {
	case *desugar.Param:
		if e.Kind != desugar.ValueParam {
			// VAR/IN: param IS the pointer — bitcast to i64; no AddrInstr needed.
			ptr := l.lowerAddr(e)
			dst := NewTemp(fmt.Sprintf("%s$adr", ptr.String()), I64())
			l.emit(&CastInst{Dst: dst, Op: "bitcast", Src: ptr})
			return dst
		}
		// Value param: spill to a stack slot via lowerAddr's fallback, then addr-of.
		v := l.lowerAddr(e)
		dst := l.newAddrTemp(fmt.Sprintf("%s$adr", v.String()), Ptr(Void()))
		l.emit(&AddrInstr{Dst: dst, Of: v})
		return dst
	case *desugar.VariableRef, *desugar.ConstantRef:
		v := l.lowerAddr(arg)
		dst := l.newAddrTemp(fmt.Sprintf("%s$adr", v.String()), Ptr(Void()))
		l.emit(&AddrInstr{Dst: dst, Of: v})
		return dst
	case *desugar.Literal:
		if e.Kind == token.STR_LIT {
			strType := LowerType(e.SemaType)
			v := l.internStringLiteral(e.Value, strType)
			dst := l.newAddrTemp(fmt.Sprintf("%s$adr", v.String()), Ptr(Void()))
			l.emit(&AddrInstr{Dst: dst, Of: v})
			return dst
		}
		panic(fmt.Sprintf("lowerSysAdr: non-string literal argument %q", e.Value))
	default:
		panic(fmt.Sprintf("lowerSysAdr: unsupported argument type %T", arg))
	}
}

func (l *Lowerer) lowerCallExpr(call *desugar.FuncCall) Value {
	// Dispatch to inline builtin lowering first.
	// Only dispatch predeclared (unqualified, non-external) functions.
	// Module-qualified calls (e.g. Math.Floor) and FFI calls (e.g. M.floor)
	// share names with builtins but must not be intercepted here.
	if !call.Func.IsExternal && call.Func.Module == "" {
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
				rt = I32()
			}

			if rt == I1() {
				return NewConst("false", int64(0), I1())
			}

			return NewConst("0", int64(0), rt)
		}
	}

	callee := call.Func.Mangled
	if callee == "" {
		callee = call.Func.Name
	}

	// system.adr(x) returns the machine address of x as INTEGER.
	if callee == "system$adr" {
		return l.lowerSysAdr(call.Args[0])
	}

	// Determine which formals expect addresses (VAR/IN) when possible.
	formalAddr := l.formalAddrForCall(call)

	var args []Value
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

	var dst *Temp
	rt := LowerType(call.RetType)
	if rt != nil && rt != Void() {
		dst = NewAnonTemp(rt)
	}

	l.emit(&CallInst{Dst: dst, Callee: callee, Args: args})
	if dst != nil {
		return dst
	}

	return NewConst("0", int64(0), I32())
}

func (l *Lowerer) lowerLiteralExpr(expr desugar.Expr) Value {
	lit, ok := expr.(*desugar.Literal)
	if !ok {
		l.reportLoweringDiagnostic(fmt.Sprintf("lowerLiteralExpr: expected *desugar.Literal, got %T", expr),
			expr.Pos(), expr.End())
		return NewConst("0", int64(0), I32())
	}

	ty := LowerType(lit.SemaType)
	if ty == nil {
		msg := fmt.Sprintf("lowerLiteralExpr: invalid literal type %s", lit.SemaType)
		l.reportLoweringDiagnostic(msg, lit.StartOffset, lit.EndOffset)

		ty = I32()
	}

	switch lit.Kind {
	case token.BYTE_LIT:
		iv, err := strconv.ParseUint(lit.Value, 10, 8)
		if err != nil {
			l.reportLoweringDiagnostic(fmt.Sprintf("lowerLiteralExpr: cannot parse byte literal %q: %v",
				lit.Value, err), lit.StartOffset, lit.EndOffset)
			return NewConst(lit.Value, lit.Value, ty)
		}
		return NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
	case token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
		integer, err := parseIntegerConst(lit.Value, lit.Kind, LowerType(lit.SemaType))
		if err != nil {
			l.reportLoweringDiagnostic(fmt.Sprintf("lowerLiteralExpr: cannot parse integer literal %q: %v",
				lit.Value, err), lit.StartOffset, lit.EndOffset)
			return NewConst(lit.Value, lit.Value, ty)
		}
		return integer
	case token.REAL_LIT, token.LONGREAL_LIT:
		v := lit.Value
		norm := strings.ReplaceAll(v, "D", "E")
		norm = strings.ReplaceAll(norm, "d", "E")
		norm = strings.ReplaceAll(norm, "S", "E")
		norm = strings.ReplaceAll(norm, "s", "E")
		fv, _ := strconv.ParseFloat(norm, 64)
		name := strconv.FormatFloat(fv, 'g', -1, 64)
		return NewConst(name, fv, ty)
	case token.TRUE:
		return NewConst("true", int64(1), I1())
	case token.FALSE:
		return NewConst("false", int64(0), I1())
	case token.CHAR_LIT, token.WCHAR_LIT:
		// Use the sema type when available; fall back to the natural type so
		// that 0x (null char) and similar literals never produce nil-typed constants.
		charTy := LowerType(lit.SemaType)
		if charTy == nil {
			if lit.Kind == token.WCHAR_LIT {
				charTy = U16()
			} else {
				charTy = U8()
			}
		}
		char, err := parseCharConst(lit.Value, lit.Kind, charTy)
		if err != nil {
			l.reportLoweringDiagnostic(fmt.Sprintf("lowerLiteralExpr: cannot parse char literal %q: %v",
				lit.Value, err), lit.StartOffset, lit.EndOffset)
			return NewConst("0", uint64(0), charTy)
		}
		return char
	case token.STR_LIT, token.HEX_STR_LIT:
		strTy := NewArrayType(len(lit.Value)+1, U16())
		return l.internStringLiteral(lit.Value, strTy)
	case token.NIL:
		return NewConst("nil", int64(0), Ptr(Void()))
	default:
		msg := fmt.Sprintf("lowerLiteralExpr: unknown literal kind %q", lit.Kind)
		l.reportLoweringDiagnostic(msg, lit.StartOffset, lit.EndOffset)

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
					if _, ok := pt.(*PointerType); ok {
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

func (l *Lowerer) newBlock(label string) *Block {
	id := l.blockSeq
	l.blockSeq++
	return &Block{
		ID:     id,
		Label:  label,
		Preds:  make(map[int]*Block),
		Succs:  make(map[int]*Block),
		Parent: l.fn,
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
		ty = I32()
	}
	t := NewTemp(name, Ptr(ty))
	t.IsAddr = true
	return t
}

// ── target helpers ─────────────────────────────────────────────────────────────

// wordSize returns the target pointer/word size in bytes (4 or 8).
// Defaults to 4 when dctx is nil (e.g. in unit tests).
func (l *Lowerer) wordSize() int64 {
	if l.dctx != nil && l.dctx.Target.WordSize == 8 {
		return 8
	}
	return 4
}

// dimType returns the IR integer type used for dope-vector dimension fields.
// 64-bit targets (WordSize == 8) use i64; all others use i32.
func (l *Lowerer) dimType() Type {
	if l.wordSize() == 8 {
		return I64()
	}
	return I32()
}

// strlenFuncName returns the C standard-library strlen symbol and ensures an
// ExternalFunc declaration is registered in the current module.
// The symbol is named "strlen" on every supported OS (POSIX libc / Windows msvcrt).
// Its return type is size_t, represented as dimType() (i32 or i64).
func (l *Lowerer) strlenFuncName() string {
	const name = "strlen"
	if l.mod != nil {
		if _, ok := l.mod.SymTab.Lookup(name); !ok {
			sig := &FunctionType{Params: []Type{Ptr(Void())}, Result: l.dimType()}
			ef := &ExternalFunc{Name: name, Sig: sig, Linkage: ExternalLinkage}
			l.mod.Externals = append(l.mod.Externals, ef)
			_ = l.mod.SymTab.Define(name, &GlobalRef{GlobalName: name, Ty: Ptr(Void())})
		}
	}
	return name
}

// memcpyFuncName returns the C standard-library memcpy symbol and ensures an
// ExternalFunc declaration is registered in the current module.
// The symbol is named "memcpy" on every supported OS (POSIX libc / Windows msvcrt).
// Signature: memcpy(dst *void, src *void, n size_t) → *void
func (l *Lowerer) memcpyFuncName() string {
	const name = "memcpy"
	if l.mod != nil {
		if _, ok := l.mod.SymTab.Lookup(name); !ok {
			dt := l.dimType()
			sig := &FunctionType{
				Params: []Type{Ptr(Void()), Ptr(Void()), dt},
				Result: Ptr(Void()),
			}
			ef := &ExternalFunc{Name: name, Sig: sig, Linkage: ExternalLinkage}
			l.mod.Externals = append(l.mod.Externals, ef)
			_ = l.mod.SymTab.Define(name, &GlobalRef{GlobalName: name, Ty: Ptr(Void())})
		}
	}
	return name
}

// scalbnFuncName returns the C math-library scalbn symbol and ensures an
// ExternalFunc declaration is registered in the current module.
// scalbn(x f64, n i32) → f64  i.e.  x * 2^n  (implements Oberon PACK).
// Available as "scalbn" in libm (POSIX) and msvcrt (Windows).
func (l *Lowerer) scalbnFuncName() string {
	const name = "scalbn"
	if l.mod != nil {
		if _, ok := l.mod.SymTab.Lookup(name); !ok {
			sig := &FunctionType{Params: []Type{F64(), I32()}, Result: F64()}
			ef := &ExternalFunc{Name: name, Sig: sig, Linkage: ExternalLinkage}
			l.mod.Externals = append(l.mod.Externals, ef)
			_ = l.mod.SymTab.Define(name, &GlobalRef{GlobalName: name, Ty: Ptr(Void())})
		}
	}
	return name
}

// frexpFuncName returns the C math-library frexp symbol and ensures an
// ExternalFunc declaration is registered in the current module.
// frexp(x f64, exp *i32) → f64  splits x into mantissa ∈ [0.5,1) and exponent.
// Available as "frexp" in libm (POSIX) and msvcrt (Windows).
func (l *Lowerer) frexpFuncName() string {
	const name = "frexp"
	if l.mod != nil {
		if _, ok := l.mod.SymTab.Lookup(name); !ok {
			sig := &FunctionType{Params: []Type{F64(), Ptr(I32())}, Result: F64()}
			ef := &ExternalFunc{Name: name, Sig: sig, Linkage: ExternalLinkage}
			l.mod.Externals = append(l.mod.Externals, ef)
			_ = l.mod.SymTab.Define(name, &GlobalRef{GlobalName: name, Ty: Ptr(Void())})
		}
	}
	return name
}

// allocFuncName returns the heap-allocator symbol appropriate for the target
// OS, and ensures a matching ExternalFunc declaration is registered in the
// current module so downstream passes (codegen, linker) can resolve it.
//
//   - POSIX targets (linux, darwin, and the default empty string): "malloc"
//   - Windows: "HeapAlloc"
func (l *Lowerer) allocFuncName() string {
	name := "malloc"
	if l.dctx != nil && l.dctx.Target.OS == "windows" {
		name = "HeapAlloc"
	}
	// Register an ExternalFunc stub in the module when not already present so
	// downstream passes see the dependency without relying on a hardcoded string.
	if l.mod != nil {
		if _, ok := l.mod.SymTab.Lookup(name); !ok {
			sig := &FunctionType{Params: []Type{I64()}, Result: Ptr(Void())}
			ef := &ExternalFunc{Name: name, Sig: sig, Linkage: ExternalLinkage}
			l.mod.Externals = append(l.mod.Externals, ef)
			_ = l.mod.SymTab.Define(name, &GlobalRef{GlobalName: name, Ty: Ptr(Void())})
		}
	}
	return name
}

// resolveVar resolves a variable name to its address Value.
// Lookup order:
//  1. local varEnv (alloca *Temp for function parameters and locals)
//  2. module SymTab (returns the *GlobalRef for module-scope variables, or the
//     *Constant value for module-scope constants)
//
// Panics when the name is not found in either scope.
func (l *Lowerer) resolveVar(mangled, bare string) Value {
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
	// Undefined variable: check if this is a cross-module reference (name
	// contains '$') before falling back to a synthetic alloca.  Cross-module
	// references arise when IO.obx accesses imported DEFINITION module globals
	// such as Stdio$stdout.  Emit an external GlobalVar for the name so that
	// the backend knows to emit an extern declaration and resolves the load
	// against the correct object file.
	if l.mod != nil && strings.Contains(mangled, "$") {
		ty := Ptr(Void())
		// Register the external global in this module's globals and symbol table.
		// IsExternRef=true tells the backend to emit .extern _Name rather than a
		// local BSS allocation (the symbol is defined in another module's object).
		gv := &GlobalVar{Name: mangled, Ty: ty, Linkage: ExternalLinkage, IsExternRef: true}
		l.mod.Globals = append(l.mod.Globals, gv)
		ref := &GlobalRef{GlobalName: mangled, Ty: ty}
		_ = l.mod.SymTab.Define(mangled, ref)
		return ref
	}

	msg := fmt.Sprintf("lowerer: undefined variable %q / %q", mangled, bare)
	l.reportLoweringDiagnostic(msg, 0, 0)
	// Create an addressable alloca as a fallback.
	addr := l.newAddrTemp(bare, I32())
	l.emit(&AllocaInst{Dst: addr, AllocType: I32()})
	return addr
}

// ensureTemp coerces v to a *Temp. Constants are materialized via a trivial
// identity (add 0 / xor false) so the result is a proper SSA def.
func (l *Lowerer) ensureTemp(v Value, ty Type) *Temp {
	if t, ok := v.(*Temp); ok {
		return t
	}
	if ty == nil {
		ty = I32()
	}
	dst := NewAnonTemp(ty)
	if ty == I1() {
		zero := NewConst("false", int64(0), I1())
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
	return t == F32() || t == F64()
}

// tokenToICmpPred maps a comparison token to an ICmpInst predicate string.
func tokenToICmpPred(op token.Kind) string {
	switch op {
	case token.EQUAL:
		return "eq"
	case token.NEQ:
		return "ne"
	case token.LESS:
		return "lt"
	case token.LEQ:
		return "le"
	case token.GREAT:
		return "gt"
	case token.GEQ:
		return "ge"
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

func parseIntegerConst(value string, kind token.Kind, ty Type) (Constant, error) {
	var bitSize int
	switch kind {
	case token.INT8:
		bitSize = 8
	case token.INT16:
		bitSize = 16
	case token.INT32:
		bitSize = 32
	case token.INT64:
		bitSize = 64
	default:
		return nil, fmt.Errorf("parseIntegerConst: invalid kind %v", kind)
	}

	v := value
	var iv int64
	var err error
	if strings.HasSuffix(v, "h") || strings.HasSuffix(v, "H") {
		v2 := v[:len(v)-1]
		iv, err = strconv.ParseInt(v2, 16, bitSize)
	} else {
		iv, err = strconv.ParseInt(v, 10, bitSize)
	}

	if err != nil {
		return nil, err
	}

	return NewConst(fmt.Sprintf("%d", iv), iv, ty), nil
}

func parseCharConst(value string, kind token.Kind, ty Type) (Constant, error) {
	switch kind {
	case token.CHAR_LIT, token.WCHAR_LIT:
		// The scanner emits the actual Unicode rune as the value string (via
		// string(character)).  Decode the first rune to obtain the numeric value.
		if len(value) == 0 {
			return NewConst("0", uint64(0), ty), nil
		}
		r, _ := utf8.DecodeRuneInString(value)
		iv := uint64(r)
		return NewConst(fmt.Sprintf("%d", iv), iv, ty), nil
	default:
		return nil, fmt.Errorf("parseCharConst: invalid kind %v", kind)
	}
}
