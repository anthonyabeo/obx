package miniropt_test

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
)

// buildStrengthFn constructs a two-block function whose entry block contains
// the given instructions plus an unconditional jump to exit.
func buildStrengthFn(name string, entryInstr []minir.Instr) (*minir.Block, *minir.Function) {
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
	entry.Instrs = append(append([]minir.Instr{}, entryInstr...), jmp)
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
	return entry, fn
}

// findOp returns the first BinaryInst in b that has the given op.
func findOp(b *minir.Block, op string) *minir.BinaryInst {
	for _, ins := range b.Instrs {
		if bin, ok := ins.(*minir.BinaryInst); ok && bin.Op == op {
			return bin
		}
	}
	return nil
}

// shiftAmount extracts the integer value from the Right operand of a BinaryInst.
func shiftAmount(b *minir.BinaryInst) (int64, bool) {
	c, ok := b.Right.(*minir.IntegerConst)
	if !ok {
		return 0, false
	}
	v, ok2 := minir.AsInt64(c)
	return v, ok2
}

// ── mul → shl ─────────────────────────────────────────────────────────────────

func TestStrengthReduce_MulByPow2_Right(t *testing.T) {
	// x * 8  →  shl x, 3
	minir.ResetTempCounter()
	u32 := minir.U32()
	g := &minir.GlobalVar{Name: "gStrMul8", Ty: u32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", u32)
	dst := minir.NewAnonTemp(u32)

	entry, fn := buildStrengthFn("str_mul8", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "mul", Left: x, Right: minir.ConstUint("8", 8, u32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	changed := miniropt.StrengthReduce(fn)
	if changed == 0 {
		t.Fatal("expected StrengthReduce to reduce x*8")
	}

	if findOp(entry, "mul") != nil {
		t.Fatal("expected mul to be gone after strength reduction")
	}
	shl := findOp(entry, "shl")
	if shl == nil {
		t.Fatal("expected shl instruction after strength reduction")
	}
	if shl.Left != x {
		t.Fatalf("expected left operand to be x, got %T %v", shl.Left, shl.Left)
	}
	if amt, ok := shiftAmount(shl); !ok || amt != 3 {
		t.Fatalf("expected shift amount 3, got %v (ok=%v)", amt, ok)
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

func TestStrengthReduce_MulByPow2_Left(t *testing.T) {
	// 4 * x  →  shl x, 2 (commutative: constant on left)
	minir.ResetTempCounter()
	u32 := minir.U32()
	g := &minir.GlobalVar{Name: "gStrMul4L", Ty: u32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", u32)
	dst := minir.NewAnonTemp(u32)

	entry, fn := buildStrengthFn("str_mul4_left", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "mul", Left: minir.ConstUint("4", 4, u32), Right: x},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	changed := miniropt.StrengthReduce(fn)
	if changed == 0 {
		t.Fatal("expected StrengthReduce to reduce 4*x")
	}

	shl := findOp(entry, "shl")
	if shl == nil {
		t.Fatal("expected shl instruction")
	}
	if shl.Left != x {
		t.Fatalf("expected left operand to be x (commuted), got %T %v", shl.Left, shl.Left)
	}
	if amt, ok := shiftAmount(shl); !ok || amt != 2 {
		t.Fatalf("expected shift amount 2, got %v", amt)
	}
}

func TestStrengthReduce_MulByTwo(t *testing.T) {
	// x * 2  →  shl x, 1
	minir.ResetTempCounter()
	u32 := minir.U32()
	g := &minir.GlobalVar{Name: "gStrMul2", Ty: u32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", u32)
	dst := minir.NewAnonTemp(u32)

	entry, fn := buildStrengthFn("str_mul2", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "mul", Left: x, Right: minir.ConstUint("2", 2, u32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.StrengthReduce(fn)
	if changed == 0 {
		t.Fatal("expected StrengthReduce to reduce x*2")
	}

	shl := findOp(entry, "shl")
	if shl == nil {
		t.Fatal("expected shl instruction")
	}
	if amt, ok := shiftAmount(shl); !ok || amt != 1 {
		t.Fatalf("expected shift amount 1, got %v", amt)
	}
}

// ── div → lshr (unsigned only) ────────────────────────────────────────────────

func TestStrengthReduce_DivByPow2_Unsigned(t *testing.T) {
	// x / 4  →  lshr x, 2  (unsigned)
	minir.ResetTempCounter()
	u32 := minir.U32()
	g := &minir.GlobalVar{Name: "gStrDiv4", Ty: u32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", u32)
	dst := minir.NewAnonTemp(u32)

	entry, fn := buildStrengthFn("str_div4", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "div", Left: x, Right: minir.ConstUint("4", 4, u32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	changed := miniropt.StrengthReduce(fn)
	if changed == 0 {
		t.Fatal("expected StrengthReduce to reduce x/4 (unsigned)")
	}

	if findOp(entry, "div") != nil {
		t.Fatal("expected div to be gone")
	}
	lshr := findOp(entry, "lshr")
	if lshr == nil {
		t.Fatal("expected lshr instruction")
	}
	if lshr.Left != x {
		t.Fatalf("expected left operand x, got %T %v", lshr.Left, lshr.Left)
	}
	if amt, ok := shiftAmount(lshr); !ok || amt != 2 {
		t.Fatalf("expected shift amount 2, got %v", amt)
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

func TestStrengthReduce_DivByPow2_Signed_NoReduction(t *testing.T) {
	// Signed div by power-of-two must NOT be reduced (wrong for negatives).
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gStrDivSgn", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	dst := minir.NewAnonTemp(i32)

	_, fn := buildStrengthFn("str_div_signed", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "div", Left: x, Right: minir.ConstInt("4", 4, i32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.StrengthReduce(fn)
	if changed != 0 {
		t.Fatalf("expected NO reduction for signed x/4, got %d changes", changed)
	}
}

// ── mod → and (unsigned only) ─────────────────────────────────────────────────

func TestStrengthReduce_ModByPow2_Unsigned(t *testing.T) {
	// x % 8  →  and x, 7 (unsigned)
	minir.ResetTempCounter()
	u32 := minir.U32()
	g := &minir.GlobalVar{Name: "gStrMod8", Ty: u32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", u32)
	dst := minir.NewAnonTemp(u32)

	entry, fn := buildStrengthFn("str_mod8", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "mod", Left: x, Right: minir.ConstUint("8", 8, u32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	changed := miniropt.StrengthReduce(fn)
	if changed == 0 {
		t.Fatal("expected StrengthReduce to reduce x%8 (unsigned)")
	}

	if findOp(entry, "mod") != nil {
		t.Fatal("expected mod to be gone")
	}
	and := findOp(entry, "and")
	if and == nil {
		t.Fatal("expected and instruction")
	}
	mask, ok := and.Right.(*minir.IntegerConst)
	if !ok {
		t.Fatalf("expected mask to be IntegerConst, got %T", and.Right)
	}
	if mask.AsUint() != 7 {
		t.Fatalf("expected mask 7 (2^3 - 1), got %d", mask.AsUint())
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

// ── no-op: non-power-of-two multiplier ───────────────────────────────────────

func TestStrengthReduce_NoOpOnNonPow2(t *testing.T) {
	minir.ResetTempCounter()
	u32 := minir.U32()
	g := &minir.GlobalVar{Name: "gStrNoop", Ty: u32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", u32)
	dst := minir.NewAnonTemp(u32)

	_, fn := buildStrengthFn("str_noop", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "mul", Left: x, Right: minir.ConstUint("3", 3, u32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.StrengthReduce(fn)
	if changed != 0 {
		t.Fatalf("expected no reduction for x*3 (non-power-of-two), got %d", changed)
	}
}

// ── combined: strength-reduce → algebraic chain ──────────────────────────────

func TestStrengthReduce_ChainedWithAlgebraicSimplify(t *testing.T) {
	// x * 8 → shl x, 3
	// (shl x, 3) mod 8 → this is handled by a separate mod pass, but here we
	// just verify that running StrengthReduce → AlgebraicSimplify in sequence
	// produces the expected results for the original scalar_test.go scenario.
	minir.ResetTempCounter()
	u32 := minir.U32()
	g := &minir.GlobalVar{Name: "g2", Ty: u32, Linkage: minir.InternalLinkage}

	x := minir.NewTemp("x", u32)
	y := minir.NewAnonTemp(u32)
	z := minir.NewAnonTemp(u32)
	w := minir.NewAnonTemp(u32)

	entry := &minir.Block{
		ID:    0,
		Label: "chain_entry",
		Preds: map[int]*minir.Block{},
		Succs: map[int]*minir.Block{},
	}
	exit := &minir.Block{
		ID:    1,
		Label: "chain_exit",
		Preds: map[int]*minir.Block{},
		Succs: map[int]*minir.Block{},
	}

	jmp := &minir.JumpInst{Target: exit.Label}
	entry.Instrs = []minir.Instr{
		&minir.BinaryInst{Dst: y, Op: "mul", Left: x, Right: minir.ConstUint("8", 8, u32)},
		&minir.BinaryInst{Dst: z, Op: "mod", Left: y, Right: minir.ConstUint("8", 8, u32)},
		&minir.BinaryInst{Dst: w, Op: "add", Left: z, Right: minir.ConstUint("0", 0, u32)},
		&minir.StoreInst{Val: w, Addr: g.Ref()},
		jmp,
	}
	entry.Term = jmp

	ret := &minir.ReturnInst{}
	exit.Instrs = []minir.Instr{ret}
	exit.Term = ret
	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn := &minir.Function{
		FnName: "chain",
		Entry:  entry,
		Exit:   exit,
		Blocks: map[int]*minir.Block{entry.ID: entry, exit.ID: exit},
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("precondition verify: %v", errs)
	}

	// Run both passes in sequence.
	srChanged := miniropt.StrengthReduce(fn)
	if srChanged == 0 {
		t.Fatal("expected StrengthReduce to make changes")
	}

	simChanged := miniropt.AlgebraicSimplify(fn)
	if simChanged == 0 {
		t.Fatal("expected AlgebraicSimplify to make changes after strength reduction")
	}

	// mul → shl
	shl := findOp(entry, "shl")
	if shl == nil {
		t.Fatal("expected mul to be strength-reduced to shl")
	}
	if amt, ok2 := shiftAmount(shl); !ok2 || amt != 3 {
		t.Fatalf("expected shl shift 3, got %v", amt)
	}

	// mod → and
	and := findOp(entry, "and")
	if and == nil {
		t.Fatal("expected mod to be strength-reduced to and")
	}
	mask, ok := and.Right.(*minir.IntegerConst)
	if !ok || mask.AsUint() != 7 {
		t.Fatalf("expected mask 7, got %v", and.Right)
	}

	// add z, 0 → z propagated into store
	st := func() *minir.StoreInst {
		for _, ins := range entry.Instrs {
			if s, ok := ins.(*minir.StoreInst); ok {
				return s
			}
		}
		return nil
	}()
	if st == nil {
		t.Fatal("expected a StoreInst")
	}
	if st.Val != z {
		t.Fatalf("expected add-by-zero simplified so store uses z, got %T %v", st.Val, st.Val)
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}
