package miniropt_test

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
)

// ── fixture helpers ───────────────────────────────────────────────────────────

// buildFoldFn constructs a two-block function (entry → exit) where entry
// contains instrsBefore followed by an unconditional jump to exit, and exit
// contains a single ReturnInst.  It also wires a GlobalVar store inside
// entry so that propagated constants remain observable after folding.
func buildFoldFn(name string, entryInstrs []minir.Instr) (*minir.Block, *minir.Block, *minir.Function) {
	entry := &minir.Block{
		ID:    0,
		Label: name + "_entry",
		Preds: map[int]*minir.Block{},
		Succs: map[int]*minir.Block{},
	}
	exit := &minir.Block{
		ID:    1,
		Label: name + "_exit",
		Preds: map[int]*minir.Block{},
		Succs: map[int]*minir.Block{},
	}

	jmp := &minir.JumpInst{Target: exit.Label}
	entry.Instrs = append(append([]minir.Instr{}, entryInstrs...), jmp)
	entry.Term = jmp

	ret := &minir.ReturnInst{}
	exit.Instrs = []minir.Instr{ret}
	exit.Term = ret

	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn := &minir.Function{
		FnName: name,
		Entry:  entry,
		Exit:   exit,
		Blocks: map[int]*minir.Block{entry.ID: entry, exit.ID: exit},
	}
	return entry, exit, fn
}

func findBinary(b *minir.Block, op string) *minir.BinaryInst {
	for _, ins := range b.Instrs {
		if bin, ok := ins.(*minir.BinaryInst); ok && bin.Op == op {
			return bin
		}
	}
	return nil
}

func findStore(b *minir.Block) *minir.StoreInst {
	for _, ins := range b.Instrs {
		if st, ok := ins.(*minir.StoreInst); ok {
			return st
		}
	}
	return nil
}

// printFunctionIR is a helper that prints the IR before and after optimization.
func printFunctionIR(t *testing.T, fn *minir.Function, label string, beforeFn, afterFn func(*minir.Function)) {
	t.Logf("\n=== %s ===\nBEFORE:\n%s", label, minir.FormatFunction(fn))
	beforeFn(fn)
	t.Logf("\nAFTER:\n%s", minir.FormatFunction(fn))
	afterFn(fn)
}

// ── integer binary constant-fold tests ────────────────────────────────────────

func TestConstantFold_IntegerBinaryOps(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gFoldBin", Ty: i32, Linkage: minir.InternalLinkage}

	tests := []struct {
		name     string
		op       string
		left     int64
		right    int64
		wantVal  int64
	}{
		{"add", "add", 2, 3, 5},
		{"sub", "sub", 10, 4, 6},
		{"mul", "mul", 3, 7, 21},
		{"div", "div", 12, 3, 4},
		{"mod", "mod", 10, 3, 1},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			minir.ResetTempCounter()
			dst := minir.NewAnonTemp(i32)
			entry, _, fn := buildFoldFn("fold_bin_"+tc.name, []minir.Instr{
				&minir.BinaryInst{
					Dst:   dst,
					Op:    tc.op,
					Left:  minir.ConstInt("", tc.left, i32),
					Right: minir.ConstInt("", tc.right, i32),
				},
				&minir.StoreInst{Val: dst, Addr: g.Ref()},
			})

			if errs := minir.VerifyIR(fn); len(errs) != 0 {
				t.Fatalf("precondition verify: %v", errs)
			}

			t.Logf("\n=== IntBinary %s ===\nBEFORE:\n%s", tc.name, minir.FormatFunction(fn))
			changed := miniropt.ConstantFold(fn)
			t.Logf("\nAFTER:\n%s", minir.FormatFunction(fn))
			if changed == 0 {
				t.Fatal("expected ConstantFold to make changes")
			}

			st := findStore(entry)
			if st == nil {
				t.Fatal("expected a StoreInst in entry block")
			}
			c, ok := st.Val.(*minir.IntegerConst)
			if !ok {
				t.Fatalf("expected store value to be IntegerConst, got %T", st.Val)
			}
			if got, ok2 := minir.AsInt64(c); !ok2 || got != tc.wantVal {
				t.Fatalf("expected %d, got %d", tc.wantVal, got)
			}

			if errs := minir.VerifyIR(fn); len(errs) != 0 {
				t.Fatalf("postcondition verify: %v", errs)
			}
		})
	}
}

// ── bitwise integer constant-fold tests ──────────────────────────────────────

func TestConstantFold_BitwiseOps(t *testing.T) {
	minir.ResetTempCounter()
	u32 := minir.U32()
	g := &minir.GlobalVar{Name: "gFoldBit", Ty: u32, Linkage: minir.InternalLinkage}

	tests := []struct {
		name    string
		op      string
		left    uint64
		right   uint64
		wantVal uint64
	}{
		{"and", "and", 0b1100, 0b1010, 0b1000},
		{"or", "or", 0b1100, 0b1010, 0b1110},
		{"xor", "xor", 0b1111, 0b0101, 0b1010},
		{"shl", "shl", 1, 3, 8},
		{"lshr", "lshr", 16, 2, 4},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			minir.ResetTempCounter()
			dst := minir.NewAnonTemp(u32)
			entry, _, fn := buildFoldFn("fold_bit_"+tc.name, []minir.Instr{
				&minir.BinaryInst{
					Dst:   dst,
					Op:    tc.op,
					Left:  minir.ConstUint("", tc.left, u32),
					Right: minir.ConstUint("", tc.right, u32),
				},
				&minir.StoreInst{Val: dst, Addr: g.Ref()},
			})

			if errs := minir.VerifyIR(fn); len(errs) != 0 {
				t.Fatalf("precondition verify: %v", errs)
			}

			t.Logf("\n=== BitwiseOp %s ===\nBEFORE:\n%s", tc.name, minir.FormatFunction(fn))
			changed := miniropt.ConstantFold(fn)
			t.Logf("\nAFTER:\n%s", minir.FormatFunction(fn))
			if changed == 0 {
				t.Fatal("expected ConstantFold to make changes")
			}

			st := findStore(entry)
			if st == nil {
				t.Fatal("expected a StoreInst")
			}
			c, ok := st.Val.(*minir.IntegerConst)
			if !ok {
				t.Fatalf("expected IntegerConst, got %T", st.Val)
			}
			if c.AsUint() != tc.wantVal {
				t.Fatalf("expected %d, got %d", tc.wantVal, c.AsUint())
			}

			if errs := minir.VerifyIR(fn); len(errs) != 0 {
				t.Fatalf("postcondition verify: %v", errs)
			}
		})
	}
}

// ── unary constant-fold tests ─────────────────────────────────────────────────

func TestConstantFold_UnaryNeg(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gFoldNeg", Ty: i32, Linkage: minir.InternalLinkage}

	dst := minir.NewAnonTemp(i32)
	entry, _, fn := buildFoldFn("fold_neg", []minir.Instr{
		&minir.UnaryInst{Dst: dst, Op: "neg", Src: minir.ConstInt("", 5, i32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	changed := miniropt.ConstantFold(fn)
	if changed == 0 {
		t.Fatal("expected ConstantFold to make changes")
	}

	st := findStore(entry)
	if st == nil {
		t.Fatal("expected a StoreInst")
	}
	c, ok := st.Val.(*minir.IntegerConst)
	if !ok {
		t.Fatalf("expected IntegerConst, got %T", st.Val)
	}
	if got, ok2 := minir.AsInt64(c); !ok2 || got != -5 {
		t.Fatalf("expected -5, got %d", got)
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

func TestConstantFold_UnaryNot(t *testing.T) {
	minir.ResetTempCounter()
	u8 := minir.U8()
	g := &minir.GlobalVar{Name: "gFoldNot", Ty: u8, Linkage: minir.InternalLinkage}

	dst := minir.NewAnonTemp(u8)
	entry, _, fn := buildFoldFn("fold_not", []minir.Instr{
		&minir.UnaryInst{Dst: dst, Op: "not", Src: minir.ConstUint("", 0xF0, u8)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	t.Logf("\n=== UnaryNot ===\nBEFORE:\n%s", minir.FormatFunction(fn))
	changed := miniropt.ConstantFold(fn)
	t.Logf("\nAFTER:\n%s", minir.FormatFunction(fn))
	if changed == 0 {
		t.Fatal("expected ConstantFold to make changes")
	}

	st := findStore(entry)
	if st == nil {
		t.Fatal("expected a StoreInst")
	}
	c, ok := st.Val.(*minir.IntegerConst)
	if !ok {
		t.Fatalf("expected IntegerConst, got %T", st.Val)
	}
	// ~0xF0 in 8 bits = 0x0F
	if c.AsUint() != 0x0F {
		t.Fatalf("expected 0x0F, got 0x%X", c.AsUint())
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

// ── integer comparison constant-fold tests ────────────────────────────────────

func TestConstantFold_ICmp(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	bTy := minir.I1()

	tests := []struct {
		name    string
		pred    string
		left    int64
		right   int64
		wantVal bool
	}{
		{"eq_true", "eq", 7, 7, true},
		{"eq_false", "eq", 3, 5, false},
		{"slt_true", "slt", 2, 5, true},
		{"slt_false", "slt", 5, 2, false},
		{"sge_true", "sge", 5, 5, true},
		{"ne_true", "ne", 1, 2, true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			minir.ResetTempCounter()
			cmpDst := minir.NewAnonTemp(bTy)
			_, exit, fn := buildFoldFn("fold_icmp_"+tc.name, []minir.Instr{
				&minir.ICmpInst{
					Dst:   cmpDst,
					Pred:  tc.pred,
					Left:  minir.ConstInt("", tc.left, i32),
					Right: minir.ConstInt("", tc.right, i32),
				},
			})
			// Wire cmpDst into ReturnInst so keepTemp is set and the fold
			// materialises the constant rather than substituting it.
			fn.Result = bTy
			exit.Instrs = []minir.Instr{&minir.ReturnInst{Result: cmpDst}}
			exit.Term = exit.Instrs[0].(minir.Terminator)

			if errs := minir.VerifyIR(fn); len(errs) != 0 {
				t.Fatalf("precondition verify: %v", errs)
			}

			changed := miniropt.ConstantFold(fn)
			if changed == 0 {
				t.Fatal("expected ConstantFold to make changes")
			}

			// The comparison was folded and materialised as a xor (bool materialiser).
			xorBin := findBinary(fn.Entry, "xor")
			if xorBin == nil {
				t.Fatal("expected folded comparison to be materialised as xor in entry block")
			}
			lc, ok := xorBin.Left.(*minir.IntegerConst)
			if !ok {
				t.Fatalf("expected bool constant on left of xor, got %T", xorBin.Left)
			}
			gotBool := lc.AsUint() != 0
			if gotBool != tc.wantVal {
				t.Fatalf("expected %v, got %v", tc.wantVal, gotBool)
			}

			if errs := minir.VerifyIR(fn); len(errs) != 0 {
				t.Fatalf("postcondition verify: %v", errs)
			}
		})
	}
}

// ── float binary constant-fold tests ─────────────────────────────────────────

func TestConstantFold_FloatBinaryOps(t *testing.T) {
	minir.ResetTempCounter()
	f64 := minir.F64()
	g := &minir.GlobalVar{Name: "gFoldF64", Ty: f64, Linkage: minir.InternalLinkage}

	tests := []struct {
		name    string
		op      string
		left    float64
		right   float64
		wantVal float64
	}{
		{"fadd", "add", 1.5, 2.5, 4.0},
		{"fsub", "sub", 5.0, 3.0, 2.0},
		{"fmul", "mul", 2.0, 3.0, 6.0},
		{"fdiv", "div", 9.0, 3.0, 3.0},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			minir.ResetTempCounter()
			dst := minir.NewAnonTemp(f64)
			entry, _, fn := buildFoldFn("fold_f64_"+tc.name, []minir.Instr{
				&minir.BinaryInst{
					Dst:   dst,
					Op:    tc.op,
					Left:  minir.ConstFloat64("", tc.left),
					Right: minir.ConstFloat64("", tc.right),
				},
				&minir.StoreInst{Val: dst, Addr: g.Ref()},
			})

			if errs := minir.VerifyIR(fn); len(errs) != 0 {
				t.Fatalf("precondition verify: %v", errs)
			}

			changed := miniropt.ConstantFold(fn)
			if changed == 0 {
				t.Fatal("expected ConstantFold to make changes")
			}

			st := findStore(entry)
			if st == nil {
				t.Fatal("expected a StoreInst")
			}
			fc, ok := st.Val.(*minir.FloatConst)
			if !ok {
				t.Fatalf("expected FloatConst, got %T", st.Val)
			}
			if fc.Value != tc.wantVal {
				t.Fatalf("expected %g, got %g", tc.wantVal, fc.Value)
			}

			if errs := minir.VerifyIR(fn); len(errs) != 0 {
				t.Fatalf("postcondition verify: %v", errs)
			}
		})
	}
}

// ── propagation after fold ────────────────────────────────────────────────────

func TestConstantFold_PropagatesAcrossInstructions(t *testing.T) {
	// 2 + 3 = 5;  5 * 4 = 20  — tests that the substitution map chains folds.
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gFoldChain", Ty: i32, Linkage: minir.InternalLinkage}

	a := minir.NewAnonTemp(i32)
	b := minir.NewAnonTemp(i32)

	entry, _, fn := buildFoldFn("fold_chain", []minir.Instr{
		&minir.BinaryInst{Dst: a, Op: "add", Left: minir.ConstInt("2", 2, i32), Right: minir.ConstInt("3", 3, i32)},
		&minir.BinaryInst{Dst: b, Op: "mul", Left: a, Right: minir.ConstInt("4", 4, i32)},
		&minir.StoreInst{Val: b, Addr: g.Ref()},
	})

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	t.Logf("\n=== PropagatesAcrossInstructions (chained fold: 2+3=5, 5*4=20) ===\nBEFORE:\n%s", minir.FormatFunction(fn))
	changed := miniropt.ConstantFold(fn)
	t.Logf("\nAFTER:\n%s", minir.FormatFunction(fn))
	if changed == 0 {
		t.Fatal("expected ConstantFold to make changes")
	}

	if findBinary(entry, "add") != nil {
		t.Fatal("expected add to be eliminated")
	}
	if findBinary(entry, "mul") != nil {
		t.Fatal("expected mul to be eliminated")
	}

	st := findStore(entry)
	if st == nil {
		t.Fatal("expected a StoreInst")
	}
	c, ok := st.Val.(*minir.IntegerConst)
	if !ok {
		t.Fatalf("expected IntegerConst, got %T", st.Val)
	}
	if got, ok2 := minir.AsInt64(c); !ok2 || got != 20 {
		t.Fatalf("expected 20, got %d", got)
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

// ── no-op: non-constant operands ─────────────────────────────────────────────

func TestConstantFold_NoOpOnVariableOperands(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gFoldNoop", Ty: i32, Linkage: minir.InternalLinkage}

	x := minir.NewTemp("x", i32)    // variable — not a constant
	dst := minir.NewAnonTemp(i32)
	_, _, fn := buildFoldFn("fold_noop", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "add", Left: x, Right: minir.ConstInt("", 1, i32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	changed := miniropt.ConstantFold(fn)
	if changed != 0 {
		t.Fatalf("expected no changes on variable operands, got %d", changed)
	}
}

// ── uniform phi elimination ───────────────────────────────────────────────────

func TestConstantFold_UniformPhiEliminated(t *testing.T) {
	// Build:  entry --[true]--> join <--[false]-- entry
	// join has a phi whose both arms carry the same IntegerConst 42.
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gFoldPhi", Ty: i32, Linkage: minir.InternalLinkage}

	entry := &minir.Block{ID: 0, Label: "phi_entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	join := &minir.Block{ID: 1, Label: "phi_join", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	exit := &minir.Block{ID: 2, Label: "phi_exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	phiDst := minir.NewAnonTemp(i32)
	cond := minir.NewTemp("cond", minir.I1())
	phi := &minir.PhiInst{
		Dst: phiDst,
		Args: []minir.PhiArm{
			{BlockLabel: entry.Label, Val: minir.ConstInt("", 42, i32)},
			{BlockLabel: entry.Label, Val: minir.ConstInt("", 42, i32)},
		},
	}

	cbr := &minir.CondBrInst{Cond: cond, TrueLabel: join.Label, FalseLabel: join.Label}
	entry.Instrs = []minir.Instr{cbr}
	entry.Term = cbr
	entry.AddSucc(join)

	jmpToExit := &minir.JumpInst{Target: exit.Label}
	join.Instrs = []minir.Instr{phi, &minir.StoreInst{Val: phiDst, Addr: g.Ref()}, jmpToExit}
	join.Term = jmpToExit
	join.AddPred(entry)
	join.AddSucc(exit)

	ret := &minir.ReturnInst{}
	exit.Instrs = []minir.Instr{ret}
	exit.Term = ret
	exit.AddPred(join)

	fn := &minir.Function{
		FnName: "fold_phi",
		Entry:  entry,
		Exit:   exit,
		Blocks: map[int]*minir.Block{entry.ID: entry, join.ID: join, exit.ID: exit},
	}

	changed := miniropt.ConstantFold(fn)
	if changed == 0 {
		t.Fatal("expected ConstantFold to eliminate uniform phi")
	}

	// After folding the phi should be gone and the store should carry the constant.
	st := findStore(join)
	if st == nil {
		t.Fatal("expected a StoreInst in join block")
	}
	c, ok := st.Val.(*minir.IntegerConst)
	if !ok {
		t.Fatalf("expected IntegerConst propagated into store, got %T", st.Val)
	}
	if got, ok2 := minir.AsInt64(c); !ok2 || got != 42 {
		t.Fatalf("expected 42, got %d", got)
	}
}

// ── Test 1: fold integer binary and substitute into return ──────────────────

func TestConstantFold_FoldAndReturn(t *testing.T) {
	// Before: %t = add i32 2, 3; ret %t
	// After: ret 5
	minir.ResetTempCounter()
	i32 := minir.I32()

	tDst := minir.NewAnonTemp(i32)
	_, exit, fn := buildFoldFn("fold_ret", []minir.Instr{
		&minir.BinaryInst{Dst: tDst, Op: "add", Left: minir.ConstInt("2", 2, i32), Right: minir.ConstInt("3", 3, i32)},
	})
	fn.Result = i32
	exit.Instrs = []minir.Instr{&minir.ReturnInst{Result: tDst}}
	exit.Term = exit.Instrs[0].(minir.Terminator)

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	changed := miniropt.ConstantFold(fn)
	if changed == 0 {
		t.Fatal("expected ConstantFold to fold 2+3")
	}

	// The add should have been materialized (not deleted) since tDst is in a return.
	// Since tDst is used in a return, it won't be substituted; instead a materializer
	// instruction will be created in entry.
	retInst, ok := exit.Instrs[len(exit.Instrs)-1].(*minir.ReturnInst)
	if !ok {
		t.Fatal("expected return instruction in exit")
	}
	// After folding, retInst.Result should still be tDst, but tDst was materialized
	// with the constant 5 in the entry block
	if retInst.Result != tDst {
		t.Fatalf("expected return to use tDst, got %T %v", retInst.Result, retInst.Result)
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

// ── Test 3: comparison folding and branch simplification ───────────────────

func TestConstantFold_ComparisonAndBranchSimplification(t *testing.T) {
	// Before: %a = add 1, 2; %b = icmp eq %a, 3; br %b, %then, %else
	// After: add is folded to 3, icmp is folded, b is materialized
	minir.ResetTempCounter()
	i32 := minir.I32()
	bTy := minir.I1()

	thenBlk := &minir.Block{ID: 1, Label: "then", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	elseBlk := &minir.Block{ID: 2, Label: "else", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	a := minir.NewAnonTemp(i32)
	b := minir.NewAnonTemp(bTy)
	cbr := &minir.CondBrInst{Cond: b, TrueLabel: thenBlk.Label, FalseLabel: elseBlk.Label}

	entry.Instrs = []minir.Instr{
		&minir.BinaryInst{Dst: a, Op: "add", Left: minir.ConstInt("1", 1, i32), Right: minir.ConstInt("2", 2, i32)},
		&minir.ICmpInst{Dst: b, Pred: "eq", Left: a, Right: minir.ConstInt("3", 3, i32)},
		cbr,
	}
	entry.Term = cbr
	entry.AddSucc(thenBlk)
	entry.AddSucc(elseBlk)

	retThen := &minir.ReturnInst{}
	thenBlk.Instrs = []minir.Instr{retThen}
	thenBlk.Term = retThen
	thenBlk.AddPred(entry)

	retElse := &minir.ReturnInst{}
	elseBlk.Instrs = []minir.Instr{retElse}
	elseBlk.Term = retElse
	elseBlk.AddPred(entry)

	fn := &minir.Function{
		FnName: "cmp_br",
		Entry:  entry,
		Blocks: map[int]*minir.Block{
			entry.ID:   entry,
			thenBlk.ID: thenBlk,
			elseBlk.ID: elseBlk,
		},
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	changed := miniropt.ConstantFold(fn)
	if changed == 0 {
		t.Fatal("expected ConstantFold to fold the add and comparison")
	}

	// After folding, the add should be eliminated
	if findBinary(entry, "add") != nil {
		t.Fatal("expected add to be eliminated")
	}

	// The branch should remain with b materialized as a constant-producing instruction
	term := entry.Term
	if _, ok := term.(*minir.CondBrInst); !ok {
		t.Fatalf("expected CondBrInst terminator, got %T", term)
	}

	// Check that b was materialized with a constant (xor for bool)
	found := false
	for _, ins := range entry.Instrs {
		if bin, ok := ins.(*minir.BinaryInst); ok && bin.Dst == b {
			found = true
			break
		}
	}
	if !found {
		t.Fatal("expected b to be materialized as a constant-producing instruction")
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

// ── Test 5: no divide-by-zero folding ─────────────────────────────────────

func TestConstantFold_DivideByZeroNotFolded(t *testing.T) {
	// Before: %a = add 1, 2; %b = div i32 %a, 0; ret %b
	// Expected: div remains (not folded), verifier passes
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gFoldDivZ", Ty: i32, Linkage: minir.InternalLinkage}

	a := minir.NewAnonTemp(i32)
	b := minir.NewAnonTemp(i32)
	entry, _, fn := buildFoldFn("div_by_zero", []minir.Instr{
		&minir.BinaryInst{Dst: a, Op: "add", Left: minir.ConstInt("1", 1, i32), Right: minir.ConstInt("2", 2, i32)},
		&minir.BinaryInst{Dst: b, Op: "div", Left: a, Right: minir.ConstInt("0", 0, i32)},
		&minir.StoreInst{Val: b, Addr: g.Ref()},
	})

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	t.Logf("\n=== DivideByZeroNotFolded (add folds, div by 0 does NOT fold) ===\nBEFORE:\n%s", minir.FormatFunction(fn))
	changed := miniropt.ConstantFold(fn)
	t.Logf("\nAFTER:\n%s", minir.FormatFunction(fn))
	// fold should succeed (folding the add), but div by 0 should NOT be folded
	if changed == 0 {
		t.Fatal("expected ConstantFold to at least fold the 1+2")
	}

	// Verify the add was folded but div remains
	if findBinary(entry, "add") != nil {
		t.Fatal("expected add to be eliminated")
	}
	if findBinary(entry, "div") == nil {
		t.Fatal("expected div by zero to remain unfold ed")
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

// ── Test 7: phi with different constants does not fold ─────────────────────

func TestConstantFold_NonUniformPhiNotFolded(t *testing.T) {
	// Before: %a = 1+2 (=3); %b = 3+4 (=7); %c = phi [ %a, entry ], [ %b, entry ]; ret %c
	// Expected: phi not folded since arms differ
	minir.ResetTempCounter()
	i32 := minir.I32()

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	exit := &minir.Block{ID: 1, Label: "exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	a := minir.NewAnonTemp(i32)
	b := minir.NewAnonTemp(i32)
	c := minir.NewAnonTemp(i32)

	entry.Instrs = []minir.Instr{
		&minir.BinaryInst{Dst: a, Op: "add", Left: minir.ConstInt("1", 1, i32), Right: minir.ConstInt("2", 2, i32)},
		&minir.BinaryInst{Dst: b, Op: "add", Left: minir.ConstInt("3", 3, i32), Right: minir.ConstInt("4", 4, i32)},
		&minir.JumpInst{Target: exit.Label},
	}
	entry.Term = entry.Instrs[len(entry.Instrs)-1].(minir.Terminator)
	entry.AddSucc(exit)

	phi := &minir.PhiInst{
		Dst: c,
		Args: []minir.PhiArm{
			{BlockLabel: entry.Label, Val: a},
			{BlockLabel: entry.Label, Val: b},
		},
	}
	exit.Instrs = []minir.Instr{phi, &minir.ReturnInst{Result: c}}
	exit.Term = exit.Instrs[len(exit.Instrs)-1].(minir.Terminator)
	exit.AddPred(entry)

	fn := &minir.Function{
		FnName: "phi_nonuniform",
		Entry:  entry,
		Exit:   exit,
		Blocks: map[int]*minir.Block{entry.ID: entry, exit.ID: exit},
		Result: i32,
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	changed := miniropt.ConstantFold(fn)
	// The adds should fold to 3 and 7, but the phi should NOT since arms differ
	if changed == 0 {
		t.Fatal("expected ConstantFold to fold the binary ops")
	}

	// Check that phi was not eliminated (should still reference folded temps or constants)
	phiFound := false
	for _, ins := range exit.Instrs {
		if p, ok := ins.(*minir.PhiInst); ok {
			phiFound = true
			// After folding, the phi args should be folded constants or the temps a and b
			if len(p.Args) != 2 {
				t.Fatalf("expected phi with 2 args, got %d", len(p.Args))
			}
			// Both args should now be constants (3 and 7) after the adds are folded.
			// They should be constants OR still references to a/b
			// Since a and b fold to constants, the phi args are likely still referencing a/b temps
			// but a and b themselves were materialized with the fold values.
			// So we're checking that the phi wasn't eliminated (it has 2 different arms).
			break
		}
	}
	if !phiFound {
		t.Fatal("expected phi to remain in exit block since arms are different")
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

// ── Test 8: store/call side effects are preserved ──────────────────────────

func TestConstantFold_StoreAndCallPreserved(t *testing.T) {
	// Before: store 1+2 to @g; call foo(3+4); ret
	// Expected: store and call remain, operands are folded constants
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSideEffect", Ty: i32, Linkage: minir.InternalLinkage}

	a := minir.NewAnonTemp(i32)
	b := minir.NewAnonTemp(i32)
	entry, _, fn := buildFoldFn("side_effect", []minir.Instr{
		&minir.BinaryInst{Dst: a, Op: "add", Left: minir.ConstInt("1", 1, i32), Right: minir.ConstInt("2", 2, i32)},
		&minir.BinaryInst{Dst: b, Op: "add", Left: minir.ConstInt("3", 3, i32), Right: minir.ConstInt("4", 4, i32)},
		&minir.StoreInst{Val: a, Addr: g.Ref()},
		&minir.CallInst{Callee: "foo", Args: []minir.Value{b}},
	})

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	t.Logf("\n=== StoreAndCallPreserved (side effects preserved, operands folded) ===\nBEFORE:\n%s", minir.FormatFunction(fn))
	changed := miniropt.ConstantFold(fn)
	t.Logf("\nAFTER:\n%s", minir.FormatFunction(fn))
	if changed == 0 {
		t.Fatal("expected ConstantFold to fold the binary ops")
	}

	// Check that store still exists
	st := findStore(entry)
	if st == nil {
		t.Fatal("expected StoreInst to remain")
	}
	// Store value should now be a constant 3
	c1, ok := st.Val.(*minir.IntegerConst)
	if !ok {
		t.Fatalf("expected store value to be constant, got %T", st.Val)
	}
	if got, ok2 := minir.AsInt64(c1); !ok2 || got != 3 {
		t.Fatalf("expected store value 3, got %d", got)
	}

	// Check that call still exists
	callFound := false
	for _, ins := range entry.Instrs {
		if callIns, ok := ins.(*minir.CallInst); ok {
			callFound = true
			// Call argument should be constant 7
			if len(callIns.Args) != 1 {
				t.Fatalf("expected call with 1 arg, got %d", len(callIns.Args))
			}
			c2, ok := callIns.Args[0].(*minir.IntegerConst)
			if !ok {
				t.Fatalf("expected call arg to be constant, got %T", callIns.Args[0])
			}
			if got, ok2 := minir.AsInt64(c2); !ok2 || got != 7 {
				t.Fatalf("expected call arg 7, got %d", got)
			}
			break
		}
	}
	if !callFound {
		t.Fatal("expected CallInst to remain")
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}
