package miniropt_test

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// ── shared HIR construction helpers ──────────────────────────────────────────

func hirProg(fn *desugar.Function) *desugar.Program {
	return &desugar.Program{
		Modules: []*desugar.Module{{Name: "Test", Decls: []desugar.Decl{fn}}},
	}
}

func iParam(name string, ty types.Type) *desugar.Param {
	return &desugar.Param{Name: name, Kind: desugar.ValueParam, Typ: ty}
}

// iParamRef creates a *desugar.Param reference used in the function body to
// refer back to a value parameter.  The lowerer handles *desugar.Param
// correctly (it checks isAddrValue before emitting a load), whereas
// *desugar.VariableRef always emits a load and must only be used for local
// variables.
func iParamRef(name string, ty types.Type) *desugar.Param {
	return &desugar.Param{Name: name, Kind: desugar.ValueParam, Typ: ty}
}

func iLit(val string, ty types.Type) *desugar.Literal {
	return &desugar.Literal{Kind: token.INT32_LIT, Value: val, SemaType: ty}
}

func iVarRef(name string, ty types.Type) *desugar.VariableRef {
	return &desugar.VariableRef{Name: name, Mangled: name, SemaType: ty}
}

// lowerAndPromote lowers the HIR program and runs Mem2Reg on every function.
// It returns (module, promoted count per function, verify errors per function).
func lowerAndPromote(
	t *testing.T,
	prog *desugar.Program,
) (*minir.Module, []int, [][]minir.VerifyError) {
	t.Helper()
	lowered := minir.Lower(prog)
	if len(lowered.Modules) == 0 {
		t.Fatal("Lower produced no modules")
	}
	mod := lowered.Modules[0]

	counts := make([]int, len(mod.Functions))
	verrs := make([][]minir.VerifyError, len(mod.Functions))

	for i, fn := range mod.Functions {
		counts[i] = miniropt.Mem2Reg(fn)
		verrs[i] = minir.VerifyIR(fn)
	}
	return mod, counts, verrs
}

// assertNoVerifyErrors fails t if any verify error was reported.
func assertNoVerifyErrors(t *testing.T, verrs [][]minir.VerifyError) {
	t.Helper()
	for _, fnErrs := range verrs {
		for _, e := range fnErrs {
			t.Errorf("verify error: %s", e.Error())
		}
	}
}

// countInstrKind counts instructions of a given type across the function.
func countInstrKind[T minir.Instr](fn *minir.Function) int {
	n := 0
	for _, b := range fn.Blocks {
		for _, ins := range b.Instrs {
			if _, ok := ins.(T); ok {
				n++
			}
		}
	}
	return n
}

// ── Test 1: single-block scalar local lifted directly ─────────────────────────
//
//	FUNCTION identity(x: INT32): INT32
//	  VAR r: INT32;
//	  r := x;
//	  RETURN r;
//
// After lowering:
//   entry:  alloca r.addr; store %x, %r.addr; %v = load %r.addr; return %v
//
// After Mem2Reg:
//   entry:  add %v, %x, 0  (materialiser, constant-foldable) OR
//           return %x  (if load replaced by a temp via subst)
//
// In both cases: zero allocas, zero raw loads/stores for r.

func TestMem2Reg_SingleBlockLocal(t *testing.T) {
	minir.ResetTempCounter()
	i32 := types.Int32Type

	fn := &desugar.Function{
		Name:   "identity",
		Params: []*desugar.Param{iParam("x", i32)},
		Result: i32,
		Locals: []desugar.Decl{&desugar.Variable{Name: "r", Type: i32}},
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.AssignStmt{Left: iVarRef("r", i32), Right: iParamRef("x", i32)},
			&desugar.ReturnStmt{Result: iVarRef("r", i32)},
		}},
	}
	_, counts, verrs := lowerAndPromote(t, hirProg(fn))
	assertNoVerifyErrors(t, verrs)

	if counts[0] == 0 {
		t.Error("expected at least one alloca to be promoted")
	}
	t.Logf("promoted %d alloca(s)", counts[0])
}

// ── Test 2: store in entry, load across single CFG edge ───────────────────────
//
//	FUNCTION clamp(x: INT32): INT32
//	  VAR r: INT32;
//	  r := 0;
//	  IF x > 0 THEN r := x END;
//	  RETURN r;
//
// The alloca for r has two stores (entry and then-branch) and a load in the
// merge block.  A φ-node must be inserted at the merge.

func TestMem2Reg_PhiInsertedAtMerge(t *testing.T) {
	minir.ResetTempCounter()
	i32 := types.Int32Type

	rVar := &desugar.Variable{Name: "r", Type: i32}

	fn := &desugar.Function{
		Name:   "clamp",
		Params: []*desugar.Param{iParam("x", i32)},
		Result: i32,
		Locals: []desugar.Decl{rVar},
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			// r := 0
			&desugar.AssignStmt{Left: iVarRef("r", i32), Right: iLit("0", i32)},
			// IF x > 0 THEN r := x END
			&desugar.IfStmt{
			Cond: &desugar.BinaryExpr{
				Op:       token.GREAT,
				Left:     iParamRef("x", i32),
				Right:    iLit("0", i32),
				SemaType: types.BooleanType,
			},
			Then: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
				&desugar.AssignStmt{Left: iVarRef("r", i32), Right: iParamRef("x", i32)},
			}},
			},
			// RETURN r
			&desugar.ReturnStmt{Result: iVarRef("r", i32)},
		}},
	}
	mod, counts, verrs := lowerAndPromote(t, hirProg(fn))
	assertNoVerifyErrors(t, verrs)

	if counts[0] == 0 {
		t.Error("expected at least one alloca to be promoted")
	}

	fn0 := mod.Functions[0]
	allocaCount := countInstrKind[*minir.AllocaInst](fn0)
	if allocaCount != 0 {
		t.Errorf("expected 0 AllocaInsts after promotion, got %d", allocaCount)
	}
	phiCount := countInstrKind[*minir.PhiInst](fn0)
	if phiCount == 0 {
		t.Error("expected at least one PhiInst after promotion of alloca with two stores")
	}
	t.Logf("promoted %d alloca(s); found %d phi(s)", counts[0], phiCount)
}

// ── Test 3: loop counter — alloca with back-edge store ────────────────────────
//
//	FUNCTION sum10(): INT32
//	  VAR n, s: INT32;
//	  n := 0; s := 0;
//	  LOOP IF n > 9 THEN EXIT END; s := s + n; n := n + 1 END;
//	  RETURN s;
//
// Both n and s must be promoted.  The loop back-edge causes φ-nodes.

func TestMem2Reg_LoopCounterPromoted(t *testing.T) {
	minir.ResetTempCounter()
	i32 := types.Int32Type

	nVar := &desugar.Variable{Name: "n", Type: i32}
	sVar := &desugar.Variable{Name: "s", Type: i32}

	fn := &desugar.Function{
		Name:   "sum10",
		Result: i32,
		Locals: []desugar.Decl{nVar, sVar},
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.AssignStmt{Left: iVarRef("n", i32), Right: iLit("0", i32)},
			&desugar.AssignStmt{Left: iVarRef("s", i32), Right: iLit("0", i32)},
			&desugar.LoopStmt{Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
				&desugar.IfStmt{
					Cond: &desugar.BinaryExpr{
						Op:       token.GREAT,
						Left:     iVarRef("n", i32),
						Right:    iLit("9", i32),
						SemaType: types.BooleanType,
					},
					Then: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
						&desugar.ExitStmt{},
					}},
				},
				&desugar.AssignStmt{
					Left: iVarRef("s", i32),
					Right: &desugar.BinaryExpr{
						Op: token.PLUS, Left: iVarRef("s", i32), Right: iVarRef("n", i32), SemaType: i32,
					},
				},
				&desugar.AssignStmt{
					Left: iVarRef("n", i32),
					Right: &desugar.BinaryExpr{
						Op: token.PLUS, Left: iVarRef("n", i32), Right: iLit("1", i32), SemaType: i32,
					},
				},
			}}},
			&desugar.ReturnStmt{Result: iVarRef("s", i32)},
		}},
	}
	mod, counts, verrs := lowerAndPromote(t, hirProg(fn))
	assertNoVerifyErrors(t, verrs)

	if counts[0] < 2 {
		t.Errorf("expected at least 2 allocas promoted (n, s), got %d", counts[0])
	}

	fn0 := mod.Functions[0]
	allocaCount := countInstrKind[*minir.AllocaInst](fn0)
	if allocaCount != 0 {
		t.Errorf("expected 0 AllocaInsts after loop promotion, got %d", allocaCount)
	}
	t.Logf("promoted %d alloca(s)", counts[0])
}

// ── Test 4: escaping alloca must NOT be promoted ──────────────────────────────
//
// We construct a raw minir.Function that mimics the escape scenario: the alloca
// address is passed as a call argument.  Mem2Reg must leave it untouched.

func TestMem2Reg_EscapingAllocaPreserved(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()

	// Build the function by hand so we can explicitly pass the address to a call.
	//
	//   entry:
	//     %a.addr = alloca i32          ← escaping alloca
	//     store 42, %a.addr
	//     call sink(%a.addr)            ← address passed to callee ⇒ escapes
	//     ret
	//   exit:
	//     ret

	aAddr := minir.NewTemp("a.addr", minir.Ptr(i32))
	aAddr.IsAddr = true

	entry := &minir.Block{
		ID:    0,
		Label: "entry",
		Preds: make(map[int]*minir.Block),
		Succs: make(map[int]*minir.Block),
	}
	exit := &minir.Block{
		ID:    1,
		Label: "fn_exit",
		Preds: make(map[int]*minir.Block),
		Succs: make(map[int]*minir.Block),
	}

	jmp := &minir.JumpInst{Target: exit.Label}
	entry.Instrs = []minir.Instr{
		&minir.AllocaInst{Dst: aAddr, AllocType: i32},
		&minir.StoreInst{Val: minir.NewConst("42", int64(42), i32), Addr: aAddr},
		&minir.CallInst{Callee: "sink", Args: []minir.Value{aAddr}},
		jmp,
	}
	entry.Term = jmp

	exitRet := &minir.ReturnInst{}
	exit.Instrs = []minir.Instr{exitRet}
	exit.Term = exitRet

	// Wire CFG.
	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn := &minir.Function{
		FnName: "escaping",
		Entry:  entry,
		Exit:   exit,
		Blocks: map[int]*minir.Block{0: entry, 1: exit},
	}

	promoted := miniropt.Mem2Reg(fn)
	if promoted != 0 {
		t.Errorf("expected 0 promotions for escaping alloca, got %d", promoted)
	}

	// AllocaInst must still be present.
	allocaCount := countInstrKind[*minir.AllocaInst](fn)
	if allocaCount == 0 {
		t.Error("AllocaInst was removed even though the address escaped")
	}
	t.Logf("promoted %d alloca(s) (expected 0)", promoted)
}

// ── Test 5: aggregate alloca (RecordType) must NOT be promoted ────────────────
//
// An alloca of aggregate type must be treated as non-promotable and left
// in place pending a future SROA pass.

func TestMem2Reg_AggregateAllocaSkipped(t *testing.T) {
	minir.ResetTempCounter()

	recTy := minir.NewRecordType("Point", []minir.RecordField{
		{Name: "x", Type: minir.I32(), Offset: 0},
		{Name: "y", Type: minir.I32(), Offset: 4},
	})
	pAddr := minir.NewTemp("p.addr", minir.Ptr(recTy))
	pAddr.IsAddr = true

	exit := &minir.Block{
		ID:    1,
		Label: "fn_exit",
		Preds: make(map[int]*minir.Block),
		Succs: make(map[int]*minir.Block),
	}
	exitRet := &minir.ReturnInst{}
	exit.Instrs = []minir.Instr{exitRet}
	exit.Term = exitRet

	jmp := &minir.JumpInst{Target: exit.Label}
	entry := &minir.Block{
		ID:    0,
		Label: "entry",
		Preds: make(map[int]*minir.Block),
		Succs: make(map[int]*minir.Block),
	}
	entry.Instrs = []minir.Instr{
		&minir.AllocaInst{Dst: pAddr, AllocType: recTy},
		jmp,
	}
	entry.Term = jmp

	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn := &minir.Function{
		FnName: "rectest",
		Entry:  entry,
		Exit:   exit,
		Blocks: map[int]*minir.Block{0: entry, 1: exit},
	}

	promoted := miniropt.Mem2Reg(fn)
	if promoted != 0 {
		t.Errorf("expected 0 promotions for aggregate alloca, got %d", promoted)
	}

	allocaCount := countInstrKind[*minir.AllocaInst](fn)
	if allocaCount == 0 {
		t.Error("aggregate AllocaInst was removed by mem2reg — SROA boundary violated")
	}
}

