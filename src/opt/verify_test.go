package opt

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/obxir"
)

// ─── helpers ──────────────────────────────────────────────────────────────

// makeValidFn builds the smallest structurally-sound function:
//
//	func valid(): void
//	  entry:
//	    ret
func makeValidFn() *obxir.Function {
	fn := obxir.NewFunction("valid", false, obxir.Void, nil)
	entry := obxir.NewBlock("entry")
	ret := &obxir.ReturnInst{}
	entry.Instrs = append(entry.Instrs, ret)
	entry.Term = ret
	fn.Blocks[entry.ID] = entry
	fn.Entry = entry
	return fn
}

// ─── VerifyIR tests ───────────────────────────────────────────────────────

func TestVerifyIR_Valid(t *testing.T) {
	fn := makeValidFn()
	errs := obxir.VerifyIR(fn)
	if len(errs) != 0 {
		t.Errorf("expected 0 errors, got %d:", len(errs))
		for _, e := range errs {
			t.Logf("  %s", e.Error())
		}
	}
}

func TestVerifyIR_MissingTerminator(t *testing.T) {
	fn := makeValidFn()
	// Add a block with no terminator.
	bad := obxir.NewBlock("noterm")
	bad.Instrs = append(bad.Instrs, &obxir.MoveInst{
		Target: &obxir.Temp{Ident: "t0", Typ: obxir.Int32Type},
		Value:  &obxir.IntegerLit{LitValue: 1, Signed: true, Bits: 32, Typ: obxir.Int32Type},
	})
	// Term intentionally not set.
	fn.Blocks[bad.ID] = bad

	errs := obxir.VerifyIR(fn)
	if !containsMsg(errs, "no terminator") {
		t.Errorf("expected 'no terminator' error; got: %v", errs)
	}
}

func TestVerifyIR_UnresolvedBranchLabel(t *testing.T) {
	fn := makeValidFn()
	// Replace the valid entry ret with a jump to a non-existent label.
	entry := fn.Entry
	jmp := &obxir.JumpInst{Target: "ghost"}
	entry.Instrs = []obxir.Instr{jmp}
	entry.Term = jmp

	errs := obxir.VerifyIR(fn)
	if !containsMsg(errs, "does not exist") {
		t.Errorf("expected branch-target error; got: %v", errs)
	}
}

func TestVerifyIR_BrokenPredSuccSymmetry(t *testing.T) {
	fn := makeValidFn()
	entry := fn.Entry

	// Create a successor block but forget to add entry to its Preds.
	exit := obxir.NewBlock("exit")
	ret := &obxir.ReturnInst{}
	exit.Instrs = append(exit.Instrs, ret)
	exit.Term = ret
	fn.Blocks[exit.ID] = exit

	jmp := &obxir.JumpInst{Target: "exit"}
	entry.Instrs = []obxir.Instr{jmp}
	entry.Term = jmp
	entry.Succs[exit.ID] = exit
	// Deliberately NOT adding entry to exit.Preds.

	errs := obxir.VerifyIR(fn)
	if !containsMsg(errs, "does not list this block as a predecessor") {
		t.Errorf("expected pred/succ symmetry error; got: %v", errs)
	}
}

func TestVerifyIR_StoreNonMemAddr(t *testing.T) {
	fn := makeValidFn()
	entry := fn.Entry

	// A Temp without IsAddr=true is not IsMem().
	addr := &obxir.Temp{Ident: "t0", Typ: obxir.Int32Type, IsAddr: false}
	val := &obxir.IntegerLit{LitValue: 42, Signed: true, Bits: 32, Typ: obxir.Int32Type}
	store := &obxir.StoreInst{Addr: addr, Val: val}

	ret := &obxir.ReturnInst{}
	entry.Instrs = []obxir.Instr{store, ret}
	entry.Term = ret

	errs := obxir.VerifyIR(fn)
	if !containsMsg(errs, "not a memory value") {
		t.Errorf("expected non-IsMem store error; got: %v", errs)
	}
}

func TestVerifyIR_PhiArgNotPred(t *testing.T) {
	fn := makeValidFn()
	entry := fn.Entry

	// Create a block with a phi whose arm references a block that is NOT in Preds.
	stranger := obxir.NewBlock("stranger")
	fn.Blocks[stranger.ID] = stranger

	join := obxir.NewBlock("join")
	target := &obxir.Temp{Ident: "p0", Typ: obxir.Int32Type}
	phi := &obxir.PhiInst{
		Target: target,
		Args: []*obxir.PHIArg{
			{Block: stranger, Value: &obxir.IntegerLit{LitValue: 1, Signed: true, Bits: 32, Typ: obxir.Int32Type}},
		},
	}
	ret := &obxir.ReturnInst{}
	join.Instrs = []obxir.Instr{phi, ret}
	join.Term = ret
	// join.Preds is empty → stranger is NOT a pred.
	fn.Blocks[join.ID] = join

	// Wire entry → join so CFG is otherwise consistent.
	jmp := &obxir.JumpInst{Target: join.Label}
	entry.Instrs = []obxir.Instr{jmp}
	entry.Term = jmp
	entry.Succs[join.ID] = join
	join.Preds[entry.ID] = entry

	errs := obxir.VerifyIR(fn)
	if !containsMsg(errs, "not a predecessor") {
		t.Errorf("expected phi-pred error; got: %v", errs)
	}
}

func TestVerifyIR_BinaryTypeMismatch(t *testing.T) {
	fn := makeValidFn()
	entry := fn.Entry

	left := &obxir.IntegerLit{LitValue: 1, Signed: true, Bits: 32, Typ: obxir.Int32Type}
	right := &obxir.IntegerLit{LitValue: 2, Signed: true, Bits: 64, Typ: obxir.Int64Type}
	add := &obxir.BinaryInst{
		Target: &obxir.Temp{Ident: "t0", Typ: obxir.Int32Type},
		Op:     obxir.ADD,
		Left:   left,
		Right:  right,
	}
	ret := &obxir.ReturnInst{}
	entry.Instrs = []obxir.Instr{add, ret}
	entry.Term = ret

	errs := obxir.VerifyIR(fn)
	if !containsMsg(errs, "type mismatch") {
		t.Errorf("expected type-mismatch error; got: %v", errs)
	}
}

// ─── VerifySSA tests ──────────────────────────────────────────────────────

func TestVerifySSA_NotSSA(t *testing.T) {
	fn := makeValidFn()
	fn.IsSSA = false
	errs := VerifySSA(fn)
	if len(errs) != 1 {
		t.Errorf("expected exactly 1 error for IsSSA=false, got %d", len(errs))
	}
}

func TestVerifySSA_Valid(t *testing.T) {
	fn := makeValidFn()
	fn.IsSSA = true
	errs := VerifySSA(fn)
	if len(errs) != 0 {
		t.Errorf("expected 0 SSA errors on valid fn, got %d: %v", len(errs), errs)
	}
}

func TestVerifySSA_DoubleDefinedTemp(t *testing.T) {
	fn := makeValidFn()
	fn.IsSSA = true
	entry := fn.Entry

	lit := &obxir.IntegerLit{LitValue: 1, Signed: true, Bits: 32, Typ: obxir.Int32Type}
	def1 := &obxir.MoveInst{Target: &obxir.Temp{Ident: "t0", Typ: obxir.Int32Type}, Value: lit}
	def2 := &obxir.MoveInst{Target: &obxir.Temp{Ident: "t0", Typ: obxir.Int32Type}, Value: lit}
	ret := &obxir.ReturnInst{}
	entry.Instrs = []obxir.Instr{def1, def2, ret}
	entry.Term = ret

	errs := VerifySSA(fn)
	if !containsMsg(errs, "defined more than once") {
		t.Errorf("expected double-def error; got: %v", errs)
	}
}

func TestVerifySSA_PhiArmCountMismatch(t *testing.T) {
	fn := makeValidFn()
	fn.IsSSA = true
	entry := fn.Entry

	// Build: entry → join, but join's phi claims 0 args despite 1 pred.
	join := obxir.NewBlock("join")
	target := &obxir.Temp{Ident: "p0", Typ: obxir.Int32Type}
	phi := &obxir.PhiInst{Target: target, Args: nil} // 0 arms, 1 pred
	ret := &obxir.ReturnInst{}
	join.Instrs = []obxir.Instr{phi, ret}
	join.Term = ret
	join.Preds[entry.ID] = entry
	fn.Blocks[join.ID] = join

	jmp := &obxir.JumpInst{Target: join.Label}
	entry.Instrs = []obxir.Instr{jmp}
	entry.Term = jmp
	entry.Succs[join.ID] = join

	errs := VerifySSA(fn)
	if !containsMsg(errs, "arm(s) but block has") {
		t.Errorf("expected phi arm-count error; got: %v", errs)
	}
}

func TestVerifySSA_UseBeforeDef(t *testing.T) {
	fn := makeValidFn()
	fn.IsSSA = true
	entry := fn.Entry

	// Use t0 before defining it.
	undef := &obxir.Temp{Ident: "t0", Typ: obxir.Int32Type}
	lit := &obxir.IntegerLit{LitValue: 1, Signed: true, Bits: 32, Typ: obxir.Int32Type}
	use := &obxir.BinaryInst{
		Target: &obxir.Temp{Ident: "t1", Typ: obxir.Int32Type},
		Op:     obxir.ADD,
		Left:   undef,
		Right:  lit,
	}
	ret := &obxir.ReturnInst{}
	entry.Instrs = []obxir.Instr{use, ret}
	entry.Term = ret

	errs := VerifySSA(fn)
	if !containsMsg(errs, "use-before-def") {
		t.Errorf("expected use-before-def error; got: %v", errs)
	}
}

// ─── VerifyIRPass / VerifySSAPass integration ─────────────────────────────

func TestVerifyIRPass_StoresErrorsInContext(t *testing.T) {
	fn := makeValidFn()
	ctx := NewPassContext()
	VerifyIRPass{}.Run(fn, ctx)
	raw, ok := ctx.Get(KeyVerifyIR)
	if !ok {
		t.Fatal("KeyVerifyIR not found in PassContext")
	}
	errs := raw.([]obxir.VerifyError)
	if len(errs) != 0 {
		t.Errorf("expected 0 errors, got %d", len(errs))
	}
}

func TestVerifySSAPass_StoresErrorsInContext(t *testing.T) {
	fn := makeValidFn()
	fn.IsSSA = true
	ctx := NewPassContext()
	VerifySSAPass{}.Run(fn, ctx)
	raw, ok := ctx.Get(KeyVerifySSA)
	if !ok {
		t.Fatal("KeyVerifySSA not found in PassContext")
	}
	errs := raw.([]obxir.VerifyError)
	if len(errs) != 0 {
		t.Errorf("expected 0 errors, got %d", len(errs))
	}
}

// ─── helpers ──────────────────────────────────────────────────────────────

func containsMsg(errs []obxir.VerifyError, substr string) bool {
	for _, e := range errs {
		if contains2(e.Error(), substr) {
			return true
		}
	}
	return false
}

func contains2(s, sub string) bool {
	return len(s) >= len(sub) && (s == sub || len(sub) == 0 ||
		func() bool {
			for i := 0; i <= len(s)-len(sub); i++ {
				if s[i:i+len(sub)] == sub {
					return true
				}
			}
			return false
		}())
}
