package minir

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/desugar"
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
	mod *Module // the module being built; set by Lower()

	// per-function mutable state, reset by lowerFunction
	blockSeq int               // monotone block-ID counter
	labelSeq int               // label-suffix counter
	fn       *Function         // function currently being lowered
	curBlock *Block            // active basic block
	varEnv   map[string]*Temp  // variable name → alloca address temp (IsAddr=true)
	constEnv map[string]Value  // constant name → pre-computed Value
	loopExit map[string]string // loop label → exit-block label; "" = innermost loop
	// diagnostics/context
	dctx     *compiler.Context
	reported map[string]bool // dedupe map keyed by rttiName@module
}

func New(dctx *compiler.Context) *Lowerer {
	return &Lowerer{
		dctx:     dctx,
		varEnv:   make(map[string]*Temp),
		constEnv: make(map[string]Value),
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
func Lower(prog *desugar.Program) *Program {
	return New(nil).Lower(prog)
}

// currentModule is a package-level hook used by LowerType to register
// module-scoped constants (vtable arrays, RTTI PODs) while lowering a
// particular module. It is set by Lower() for the module currently being
// processed and cleared afterward.
var currentModule *Module

// RTTI ID assignment state
var (
	nextRTTIID uint64 = 1
	rttiID            = map[string]uint64{}
)

// Offsets (in bytes) within the RTTI POD for inline loads. We choose a
// stable layout: ID(uint64) at offset 0, Base pointer at offset 8, VTable
// pointer at offset 16, Name pointer at offset 24, Size(uint64) at offset 32.
const (
	rttiIDOff      = 0
	rttiBasePtrOff = 8
	rttiVTableOff  = 16
	rttiNameOff    = 24
	rttiSizeOff    = 32
)

// rttiPODRecordType returns a RecordType descriptor matching the RTTI POD
// layout: { ID uint64, Base *i32, VTable *i32, Name *i32, Size uint64 }.
// Field indices: 0=ID, 1=Base, 2=VTable, 3=Name, 4=Size.
func rttiPODRecordType() *RecordType {
	return NewRecordType("", []RecordField{
		{Name: "ID", Type: primI64, Offset: rttiIDOff},
		{Name: "Base", Type: Ptr(primI32), Offset: rttiBasePtrOff},
		{Name: "VTable", Type: Ptr(primI32), Offset: rttiVTableOff},
		{Name: "Name", Type: Ptr(primI32), Offset: rttiNameOff},
		{Name: "Size", Type: primI64, Offset: rttiSizeOff},
	})
}

// rttiNameForType extracts the RTTI symbol name from a sema type,
// unwrapping NamedType → RecordType (and optionally PointerType base) if needed.
func rttiNameForType(ty types.Type) string {
	switch tt := ty.(type) {
	case *types.NamedType:
		if rec, ok := tt.Def.(*types.RecordType); ok && rec.Layout != nil {
			return rec.Layout.RTTIName
		}
	case *types.RecordType:
		if tt.Layout != nil {
			return tt.Layout.RTTIName
		}
	case *types.PointerType:
		return rttiNameForType(tt.Base)
	}
	return ""
}

// Lower lowers a desugared program using a compiler Context for diagnostic
// emission translates a desugar.Program into a *minir.Program, producing one
// minir.Module per desugar.Module.
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

	for _, hirMod := range prog.Modules {
		mod := &Module{Name: hirMod.Name}
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

// lowerGlobalVar lowers a module-scope variable declaration into a *GlobalVar
// and registers its *GlobalRef address in the module SymTab.
func (l *Lowerer) lowerGlobalVar(d *desugar.Variable) {
	vt := LowerType(d.Type)
	if vt == nil {
		vt = primI32
	}

	lk := InternalLinkage
	if d.IsExport {
		lk = ExternalLinkage
	}

	name := d.Name
	if d.Mangled != "" {
		name = d.Mangled
	}

	gv := &GlobalVar{Name: name, Ty: vt, Linkage: lk}
	l.mod.Globals = append(l.mod.Globals, gv)
	if err := l.mod.SymTab.Define(name, gv.Ref()); err != nil {
		l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
	}

	// Also, register the bare (unmangled) name when it differs.
	if d.Mangled != "" && d.Mangled != d.Name {
		if err := l.mod.SymTab.Define(d.Name, gv.Ref()); err != nil {
			l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
		}
	}
}

// lowerGlobalConst lowers a module-scope constant declaration into a
// *GlobalConst and registers the immediate *Constant value (not the Ref)
// in the module SymTab so that ConstantRef resolution inside functions
// returns the value directly without a load.
func (l *Lowerer) lowerGlobalConst(d *desugar.Constant) {
	vt := LowerType(d.Type)
	if vt == nil {
		vt = primI32
	}

	lk := InternalLinkage
	if d.IsExport {
		lk = ExternalLinkage
	}

	name := d.Name
	if d.Mangled != "" {
		name = d.Mangled
	}

	init := lowerConstant(d.Value)
	gc := &GlobalConst{Name: name, Ty: vt, Init: init, Linkage: lk}
	l.mod.Constants = append(l.mod.Constants, gc)

	// Insert the immediate value so ConstantRef does not need a load.
	// otherwise, references to this constant would resolve to the GlobalRef.
	if init != nil {
		if err := l.mod.SymTab.Define(name, init); err != nil {
			l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
		}
	} else {
		if err := l.mod.SymTab.Define(name, gc.Ref()); err != nil {
			l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
		}
	}

	// if mangled name differs from the original name, also register the
	// original name for direct lookup
	if d.Mangled != "" && d.Mangled != d.Name {
		if init != nil {
			if err := l.mod.SymTab.Define(d.Name, init); err != nil {
				l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
			}
		} else {
			if err := l.mod.SymTab.Define(d.Name, gc.Ref()); err != nil {
				l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
			}
		}
	}
}

// lowerExternalFunc lowers an FFI / external function declaration into an
// *ExternalFunc (only a signature).
func (l *Lowerer) lowerExternalFunc(d *desugar.Function) {
	var params []Type
	for _, p := range d.Params {
		pt := LowerType(p.Typ)
		if pt == nil {
			pt = primI32
		}

		// VAR and IN formals are passed by reference: use pointer types.
		if p.Kind == desugar.VarParam || p.Kind == desugar.InParam {
			pt = Ptr(pt)
		}
		params = append(params, pt)
	}

	sig := &FunctionType{Params: params, Result: LowerType(d.Result)}

	// Build ExternalFunc and attach optional external attributes so downstream
	// phases (IR, codegen, symbol emission) can access C-name, DLL override,
	// and variadic / calling-convention metadata.
	cName := d.Mangled
	if cName == "" {
		cName = d.Name
	}

	ef := &ExternalFunc{
		Name:    d.FnName(),
		Sig:     sig,
		Linkage: ExternalLinkage,
		Attrs: &ExternalAttrs{
			CName:    cName,
			DLLName:  d.DLLName,
			Variadic: d.IsVarArgs,
			CallConv: "",
		},
	}

	l.mod.Externals = append(l.mod.Externals, ef)
}

// lowerConstant evaluates a Literal expression to a Constant without
// requiring an active basic block.  Returns nil for non-literal expressions.
func lowerConstant(expr desugar.Expr) Constant {
	lit, ok := expr.(*desugar.Literal)
	if !ok {
		return nil
	}

	ty := LowerType(lit.SemaType)
	if ty == nil {
		ty = primI32
	}

	switch lit.Kind {
	case token.BYTE_LIT:
		iv, err := strconv.ParseUint(lit.Value, 10, 8)
		if err != nil {
			return NewConst(lit.Value, lit.Value, ty)
		}
		return NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
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
			return NewConst(lit.Value, lit.Value, ty)
		}

		return NewConst(fmt.Sprintf("%d", iv), iv, ty)
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
		return NewConst("true", int64(1), primI1)
	case token.FALSE:
		return NewConst("false", int64(0), primI1)
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
			return NewConst(lit.Value, lit.Value, ty)
		}

		return NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
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
			return NewConst(lit.Value, lit.Value, ty)
		}

		return NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
	case token.STR_LIT, token.HEX_STR_LIT:
		return NewConst(lit.Value, lit.Value, NewArrayType(len(lit.Value)+1, primU16))
	case token.NIL:
		return NewConst("nil", int64(0), Ptr(primVoid))
	default:
		iv, err := strconv.ParseInt(lit.Value, 10, 64)
		if err == nil {
			return NewConst(lit.Value, iv, ty)
		}
		return NewConst(lit.Value, lit.Value, ty)
	}
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
		case types.BYTE, types.CHAR:
			return primU8
		case types.INT8:
			return primI8
		case types.INT16, types.SHORTINT:
			return primI16
		case types.INT32, types.INTEGER:
			return primI32
		case types.WCHAR:
			return primU16
		case types.SET:
			return primU32
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
			return Ptr(primVoid) // void*
		}
		return Ptr(base)
	case *types.CPointerType:
		base := LowerType(t.Base)
		if base == nil {
			return Ptr(primVoid)
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
			offset += f.Type.Width()
		}

		// Construct record type
		rec := NewRecordType("", fields)

		// If a runtime layout is available and a current module is being
		// lowered, emit module-level vtable and RTTI constants so downstream
		// codegen can place them in .rodata. We keep this in minir rather than
		// obxir to avoid changing many call sites.
		if t.Layout != nil && currentModule != nil {
			// VTable: emit as a uint32 array constant
			if t.Layout.VTableName != "" {
				vtabLen := len(t.Layout.VTable)
				vtabTyp := NewArrayType(vtabLen, primI32)
				vals := make([]uint32, 0, vtabLen)
				for _, ms := range t.Layout.VTable {
					vals = append(vals, uint32(ms.FuncIndex))
				}
				vtabConst := NewConst(t.Layout.VTableName, vals, vtabTyp)
				gv := &GlobalConst{Name: t.Layout.VTableName, Ty: vtabTyp, Init: vtabConst, Linkage: InternalLinkage}
				currentModule.Constants = append(currentModule.Constants, gv)
				_ = currentModule.SymTab.Define(t.Layout.VTableName, gv.Ref())
			}

			// RTTI: emit a name string and an extended RTTI POD
			if t.Layout.RTTIName != "" {
				// name symbol
				nameSym := t.Layout.RTTIName + "_name"
			nameConst := NewConst(nameSym, t.String(), NewArrayType(len(t.String())+1, primI32))
			nameGV := &GlobalConst{Name: nameSym, Ty: nameConst.Type(), Init: nameConst, Linkage: PrivateLinkage}
				currentModule.Constants = append(currentModule.Constants, nameGV)
				_ = currentModule.SymTab.Define(nameSym, nameGV.Ref())

				// Assign a stable RTTI numeric ID for this type.
				id := rttiID[t.Layout.RTTIName]
				if id == 0 {
					id = nextRTTIID
					nextRTTIID++
					rttiID[t.Layout.RTTIName] = id
				}

				// Base RTTI symbol name (may be empty)
				baseSym := ""
				if t.Base != nil && t.Base.Layout != nil {
					baseSym = t.Base.Layout.RTTIName
				}

				// Extended RTTI POD (ID, BasePtr, VTableName, NameSym, Size).
				rttiVal := struct {
					ID     uint64
					Base   string
					VTable string
					Name   string
					Size   uint64
				}{
					ID:     id,
					Base:   baseSym,
					VTable: t.Layout.VTableName,
					Name:   nameSym,
					Size:   uint64(t.Layout.Size),
				}
				rttiConst := NewConst(t.Layout.RTTIName, rttiVal, Ptr(primI32))
				rttiGV := &GlobalConst{Name: t.Layout.RTTIName, Ty: Ptr(primI32), Init: rttiConst, Linkage: InternalLinkage}
				currentModule.Constants = append(currentModule.Constants, rttiGV)
				_ = currentModule.SymTab.Define(t.Layout.RTTIName, rttiGV.Ref())
			}
		}

		return rec
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
		return primU32
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

	// Keep the exit as a simple sentinel return; actual return values and
	// halting semantics remain in their original blocks and are handled
	// by later phases.
	l.switchTo(fn.Exit)

	ret := &ReturnInst{}
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
			et = primI32
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
				vt = primI32
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
	l.emit(&StoreInst{Val: val, Addr: addr})
}

func (l *Lowerer) lowerReturn(st *desugar.ReturnStmt) {
	// Emit a ReturnInst in the current block. The return value (if any)
	// is materialized into a temp so the ReturnInst holds a proper SSA def.
	var result *Temp
	if st.Result != nil {
		v := l.lowerValue(st.Result)
		result = l.ensureTemp(v, LowerType(st.Result.Type()))
	}
	ret := &ReturnInst{Result: result}
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
		condTemp := l.ensureTemp(condVal, primI1)

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
			j := &JumpInst{Target: endLabel}
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
		if isAddrValue(v) {
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
			ty = primI32
		}
		return NewConst("0", int64(0), ty)
	case *desugar.TypeGuardExpr:
		// Lower the subject expression to a pointer/value temp we can pass to
		// a runtime helper. We keep the helper responsible for the subtype
		// walk; on failure we emit a HaltInst to abort as required.
		subj := l.lowerValue(e.Expr)
		// ensure a pointer-like temp for passing to the runtime helper
		obj := l.ensureTemp(subj, Ptr(primI32))

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
		instRTTIPtr := NewAnonTemp(Ptr(primI32))
		// Treat obj as an address (pointer-to-object) for the load.
		obj.IsAddr = true
		l.emit(&LoadInst{Dst: instRTTIPtr, Addr: obj})
		instRTTIPtr.IsAddr = true // it points into RTTI memory; mark addressable for GEP

		// Use the shared RTTI POD record type descriptor.
		rttiRec := rttiPODRecordType()
		// GEP to field 0 (ID)
		idAddr := l.newAddrTemp("rtti.id", rttiRec)
		l.emit(&GEPInst{Dst: idAddr, Base: instRTTIPtr, ElemType: rttiRec, Offsets: []int{0}})
		instID := NewAnonTemp(primI64)
		l.emit(&LoadInst{Dst: instID, Addr: idAddr})

		// Resolve target ID from rttiID map (assigned during LowerType emission).
		tid := rttiID[rttiName]
		if tid == 0 {
			// no id assigned: conservatively continue
			return obj
		}
		targetConst := NewConst(fmt.Sprintf("%d", tid), int64(tid), primI64)

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
		nextRTTIPtr := NewAnonTemp(Ptr(primI32)) // defined in mismatchBlk
		nextID := NewAnonTemp(primI64)           // defined in loadNextBlk

		// ── start block: phis + compare ─────────────────────────────────────
		l.switchTo(startBlk)
		phiID := NewAnonTemp(primI64)
		phiPtr := NewAnonTemp(Ptr(primI32))
		phiPtr.IsAddr = true // mark addressable so it can be used as GEP base in mismatch
		l.emit(&PhiInst{Dst: phiPtr, Args: []PhiArm{
			{BlockLabel: prevLabel, Val: instRTTIPtr},
			{BlockLabel: loadNextLabel, Val: nextRTTIPtr},
		}})
		l.emit(&PhiInst{Dst: phiID, Args: []PhiArm{
			{BlockLabel: prevLabel, Val: instID},
			{BlockLabel: loadNextLabel, Val: nextID},
		}})
		cmp := NewAnonTemp(primI1)
		l.emit(&ICmpInst{Dst: cmp, Pred: "eq", Left: phiID, Right: targetConst})
		cbr := &CondBrInst{Cond: cmp, TrueLabel: passLabel, FalseLabel: mismatchLabel}
		l.emit(cbr)
		l.curBlock.Term = cbr

		// ── mismatch block: load base ptr, check null ────────────────────────
		l.switchTo(mismatchBlk)
		baseAddr := l.newAddrTemp("rtti.base", rttiRec)
		l.emit(&GEPInst{Dst: baseAddr, Base: phiPtr, ElemType: rttiRec, Offsets: []int{1}})
		l.emit(&LoadInst{Dst: nextRTTIPtr, Addr: baseAddr})
		zeroPtr := NewConst("0", int64(0), Ptr(primI32))
		isNull := NewAnonTemp(primI1)
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
		halt := &HaltInst{Code: NewConst("1", int64(1), primI32)}
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
		return NewConst(e.Name, e.Name, Ptr(primI32))
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
			ty = primI32
		}

		if ty == primI1 {
			return NewConst("false", int64(0), primI1)
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
		if !isAddrValue(v) {
			// Attempt to take address of a non-addressable value parameter.
			// Emit a warning and create an addressable stack slot, storing the
			// parameter value into it so lowering can continue.
			msg := fmt.Sprintf("lowerAddr: attempt to take address of value parameter '%s'", e.Name)
			l.reportLoweringDiagnostic(msg, e.StartOffset, e.EndOffset)
			pt := LowerType(e.Typ)
			if pt == nil {
				pt = primI32
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
			ty = primI32
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

	var recTy *RecordType
	if pt, ok := base.Type().(*PointerType); ok {
		recTy, _ = pt.Elem.(*RecordType)
	}

	var elemType = ft
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

	var elemType = et
	var arrTy *ArrayType
	if pt, ok := base.Type().(*PointerType); ok {
		arrTy, _ = pt.Elem.(*ArrayType)
	}

	if arrTy != nil {
		elemType = arrTy
	}

	l.emit(&GEPInst{Dst: dst, Base: base, ElemType: elemType, Offsets: offsets, Indices: indices})

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
		ty = primI32
	}

	switch e.Op {
	case token.EQUAL, token.NEQ, token.LESS, token.LEQ, token.GREAT, token.GEQ:
		left, right = l.alignOperands(left, right)
		pred := tokenToICmpPred(e.Op)
		dst := NewAnonTemp(primI1)

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
		left, right = l.sameWidthOperands(left, right, primU32)
		// set membership: ((1 << left) & right) != 0
		one := NewConst("1", int64(1), primU32)
		shifted := NewAnonTemp(primU32)
		l.emit(&BinaryInst{Dst: shifted, Op: "shl", Left: one, Right: left})

		andRes := NewAnonTemp(primU32)
		l.emit(&BinaryInst{Dst: andRes, Op: "and", Left: shifted, Right: right})

		zero := NewConst("0", int64(0), primU32)
		dst := NewAnonTemp(primI1)
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
		ty = primI32
	}

	switch e.Op {
	case token.NOT:
		operand = l.sameType(operand, primI1)
		one := NewConst("true", int64(1), primI1)
		dst := NewAnonTemp(primI1)
		l.emit(&BinaryInst{Dst: dst, Op: "xor", Left: operand, Right: one})
		return dst
	case token.MINUS:
		operand = l.sameType(operand, ty)
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
func (l *Lowerer) sameWidthOperands(left, right Value, ty Type) (Value, Value) {
	if ty != nil {
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
func (l *Lowerer) alignOperands(left, right Value) (Value, Value) {
	if left == nil || right == nil {
		return left, right
	}
	lt, rt := left.Type(), right.Type()
	if lt == nil || rt == nil || lt.Equal(rt) {
		return left, right
	}
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
func (l *Lowerer) lowerSetExpr(s *desugar.SetExpr) Value {
	if len(s.Elems) == 0 {
		return NewConst("0", int64(0), primU32)
	}

	var acc Value = NewConst("0", int64(0), primU32)
	for _, elem := range s.Elems {
		var mask Value
		if re, ok := elem.(*desugar.RangeExpr); ok {
			mask = l.lowerRangeExpr(re)
		} else {
			// singleton: mask = 1 << elem
			idx := l.lowerValue(elem)
			one := NewConst("1", int64(1), primU32)
			shifted := NewAnonTemp(primU32)
			l.emit(&BinaryInst{Dst: shifted, Op: "shl", Left: one, Right: idx})
			mask = shifted
		}
		newAcc := NewAnonTemp(primU32)
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
	one := NewConst("1", int64(1), primU32)

	// length = (high + 1) - low
	highPlusOne := NewAnonTemp(primU32)
	l.emit(&BinaryInst{Dst: highPlusOne, Op: "add", Left: high, Right: one})
	length := NewAnonTemp(primU32)
	l.emit(&BinaryInst{Dst: length, Op: "sub", Left: highPlusOne, Right: low})

	// ones = (1 << length) - 1
	shifted := NewAnonTemp(primU32)
	l.emit(&BinaryInst{Dst: shifted, Op: "shl", Left: one, Right: length})
	ones := NewAnonTemp(primU32)
	l.emit(&BinaryInst{Dst: ones, Op: "sub", Left: shifted, Right: one})

	// mask = ones << low
	mask := NewAnonTemp(primU32)
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
		dst := NewAnonTemp(primI1)
		l.emit(&BinaryInst{Dst: dst, Op: "xor",
			Left:  NewConst("false", int64(0), primI1),
			Right: NewConst("false", int64(0), primI1),
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
	targetConst := NewConst(fmt.Sprintf("%d", tid), int64(tid), primI64)

	rttiRec := rttiPODRecordType()

	// Load the RTTI pointer from the object header (vptr at offset 0).
	objTemp := l.ensureTemp(obj, Ptr(primI32))
	objTemp.IsAddr = true
	instRTTIPtr := NewAnonTemp(Ptr(primI32))
	l.emit(&LoadInst{Dst: instRTTIPtr, Addr: objTemp})
	instRTTIPtr.IsAddr = true // points into RTTI memory

	// Load initial ID field.
	idAddr0 := l.newAddrTemp("is.id", rttiRec)
	l.emit(&GEPInst{Dst: idAddr0, Base: instRTTIPtr, ElemType: rttiRec, Offsets: []int{0}})
	instID := NewAnonTemp(primI64)
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
	nextRTTIPtr := NewAnonTemp(Ptr(primI32)) // defined in mismatchBlk
	nextID := NewAnonTemp(primI64)           // defined in loadNextBlk

	// ── start block: phis + compare ─────────────────────────────────────────
	l.switchTo(startBlk)
	phiPtr := NewAnonTemp(Ptr(primI32))
	phiPtr.IsAddr = true // will be used as GEP base in mismatch
	phiID := NewAnonTemp(primI64)
	l.emit(&PhiInst{Dst: phiPtr, Args: []PhiArm{
		{BlockLabel: preludeLabel, Val: instRTTIPtr},
		{BlockLabel: loadNextLabel, Val: nextRTTIPtr},
	}})
	l.emit(&PhiInst{Dst: phiID, Args: []PhiArm{
		{BlockLabel: preludeLabel, Val: instID},
		{BlockLabel: loadNextLabel, Val: nextID},
	}})
	cmp := NewAnonTemp(primI1)
	l.emit(&ICmpInst{Dst: cmp, Pred: "eq", Left: phiID, Right: targetConst})
	cbrStart := &CondBrInst{Cond: cmp, TrueLabel: foundLabel, FalseLabel: mismatchLabel}
	l.emit(cbrStart)
	l.curBlock.Term = cbrStart

	// ── mismatch block: load base ptr, null-check ───────────────────────────
	l.switchTo(mismatchBlk)
	baseAddr := l.newAddrTemp("is.base", rttiRec)
	l.emit(&GEPInst{Dst: baseAddr, Base: phiPtr, ElemType: rttiRec, Offsets: []int{1}})
	l.emit(&LoadInst{Dst: nextRTTIPtr, Addr: baseAddr})
	zeroPtr := NewConst("0", int64(0), Ptr(primI32))
	isNull := NewAnonTemp(primI1)
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
	result := NewAnonTemp(primI1)
	l.emit(&PhiInst{Dst: result, Args: []PhiArm{
		{BlockLabel: foundLabel, Val: NewConst("true", int64(1), primI1)},
		{BlockLabel: notFoundLabel, Val: NewConst("false", int64(0), primI1)},
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

func (l *Lowerer) lowerCallExpr(call *desugar.FuncCall) Value {
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
			rt = primI32
		}

		if rt == primI1 {
			return NewConst("false", int64(0), primI1)
		}

		return NewConst("0", int64(0), rt)
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

	var dst *Temp
	rt := LowerType(call.RetType)
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
		l.reportLoweringDiagnostic(fmt.Sprintf("lowerLiteralExpr: expected *desugar.Literal, got %T", expr),
			expr.Pos(), expr.End())
		return NewConst("0", int64(0), primI32)
	}

	ty := LowerType(lit.SemaType)
	if ty == nil {
		ty = primI32
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
			return NewConst(lit.Value, lit.Value, ty)
		}

		return NewConst(fmt.Sprintf("%d", iv), iv, ty)
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
		return NewConst("true", int64(1), primI1)
	case token.FALSE:
		return NewConst("false", int64(0), primI1)
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
			return NewConst(lit.Value, lit.Value, ty)
		}

		return NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
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
			return NewConst(lit.Value, lit.Value, ty)
		}

		return NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
	case token.STR_LIT, token.HEX_STR_LIT:
		return NewConst(lit.Value, lit.Value, NewArrayType(len(lit.Value)+1, primU16))
	case token.NIL:
		return NewConst("nil", int64(0), Ptr(primVoid))
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
		ty = primI32
	}
	t := NewTemp(name, Ptr(ty))
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
	// Undefined variable: emit a diagnostic and return a synthetic addressable
	// temporary so lowering can continue. The reporter (when present) will
	// include no source Range for this site.
	msg := fmt.Sprintf("lowerer: undefined variable %q / %q", mangled, bare)
	l.reportLoweringDiagnostic(msg, 0, 0)
	// Create an addressable alloca as a fallback.
	addr := l.newAddrTemp(bare, primI32)
	l.emit(&AllocaInst{Dst: addr, AllocType: primI32})
	return addr
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
