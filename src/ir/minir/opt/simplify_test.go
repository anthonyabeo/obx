package miniropt_test

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
)

// buildSimplifyFn builds a two-block function whose entry block contains
// instrsBefore (followed by an unconditional jump to exit) and whose exit
// block contains a single ReturnInst.  It returns the entry block, exit
// block, and the Function so callers can inspect/mutate after the pass.
func buildSimplifyFn(name string, entryInstrs []minir.Instr) (*minir.Block, *minir.Block, *minir.Function) {
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

// inspectStore returns the address and value from the first StoreInst in block b.
func inspectStore(b *minir.Block) (minir.Value, bool) {
	for _, ins := range b.Instrs {
		if st, ok := ins.(*minir.StoreInst); ok {
			return st.Val, true
		}
	}
	return nil, false
}

// ── add identity tests ────────────────────────────────────────────────────────

func TestAlgebraicSimplify_AddZeroRight(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimAddZR", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	dst := minir.NewAnonTemp(i32)

	entry, _, fn := buildSimplifyFn("sim_add_zero_right", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "add", Left: x, Right: minir.ConstInt("", 0, i32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.AlgebraicSimplify(fn)
	if changed == 0 {
		t.Fatal("expected AlgebraicSimplify to eliminate x+0")
	}

	val, ok := inspectStore(entry)
	if !ok {
		t.Fatal("expected a StoreInst")
	}
	if val != x {
		t.Fatalf("expected store value to be x, got %T %v", val, val)
	}

	if errs := minir.VerifyIR(fn); len(errs) != 0 {
		t.Fatalf("postcondition verify: %v", errs)
	}
}

func TestAlgebraicSimplify_AddZeroLeft(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimAddZL", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	dst := minir.NewAnonTemp(i32)

	entry, _, fn := buildSimplifyFn("sim_add_zero_left", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "add", Left: minir.ConstInt("", 0, i32), Right: x},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.AlgebraicSimplify(fn)
	if changed == 0 {
		t.Fatal("expected AlgebraicSimplify to eliminate 0+x")
	}

	val, ok := inspectStore(entry)
	if !ok {
		t.Fatal("expected a StoreInst")
	}
	if val != x {
		t.Fatalf("expected store value to be x, got %T %v", val, val)
	}
}

// ── sub identity tests ────────────────────────────────────────────────────────

func TestAlgebraicSimplify_SubZero(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimSubZ", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	dst := minir.NewAnonTemp(i32)

	entry, _, fn := buildSimplifyFn("sim_sub_zero", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "sub", Left: x, Right: minir.ConstInt("", 0, i32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.AlgebraicSimplify(fn)
	if changed == 0 {
		t.Fatal("expected AlgebraicSimplify to eliminate x-0")
	}

	val, ok := inspectStore(entry)
	if !ok {
		t.Fatal("expected a StoreInst")
	}
	if val != x {
		t.Fatalf("expected store value to be x, got %T %v", val, val)
	}
}

func TestAlgebraicSimplify_SubSelf(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimSubSelf", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	dst := minir.NewAnonTemp(i32)

	entry, _, fn := buildSimplifyFn("sim_sub_self", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "sub", Left: x, Right: x},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.AlgebraicSimplify(fn)
	if changed == 0 {
		t.Fatal("expected AlgebraicSimplify to eliminate x-x")
	}

	val, ok := inspectStore(entry)
	if !ok {
		t.Fatal("expected a StoreInst")
	}
	c, isConst := val.(*minir.IntegerConst)
	if !isConst {
		t.Fatalf("expected IntegerConst 0, got %T %v", val, val)
	}
	if c.AsUint() != 0 {
		t.Fatalf("expected 0, got %d", c.AsUint())
	}
}

// ── mul identity/annihilator tests ────────────────────────────────────────────

func TestAlgebraicSimplify_MulByOne(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimMul1", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	dst := minir.NewAnonTemp(i32)

	entry, _, fn := buildSimplifyFn("sim_mul_one", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "mul", Left: x, Right: minir.ConstInt("", 1, i32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.AlgebraicSimplify(fn)
	if changed == 0 {
		t.Fatal("expected AlgebraicSimplify to eliminate x*1")
	}

	val, ok := inspectStore(entry)
	if !ok {
		t.Fatal("expected a StoreInst")
	}
	if val != x {
		t.Fatalf("expected store value to be x, got %T %v", val, val)
	}
}

func TestAlgebraicSimplify_MulByZero(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimMul0", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	dst := minir.NewAnonTemp(i32)

	entry, _, fn := buildSimplifyFn("sim_mul_zero", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "mul", Left: x, Right: minir.ConstInt("", 0, i32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.AlgebraicSimplify(fn)
	if changed == 0 {
		t.Fatal("expected AlgebraicSimplify to eliminate x*0")
	}

	val, ok := inspectStore(entry)
	if !ok {
		t.Fatal("expected a StoreInst")
	}
	c, isConst := val.(*minir.IntegerConst)
	if !isConst {
		t.Fatalf("expected IntegerConst 0, got %T %v", val, val)
	}
	if c.AsUint() != 0 {
		t.Fatalf("expected 0, got %d", c.AsUint())
	}
}

// ── bitwise identity tests ────────────────────────────────────────────────────

func TestAlgebraicSimplify_AndZero(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimAndZ", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	dst := minir.NewAnonTemp(i32)

	entry, _, fn := buildSimplifyFn("sim_and_zero", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "and", Left: x, Right: minir.ConstInt("", 0, i32)},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.AlgebraicSimplify(fn)
	if changed == 0 {
		t.Fatal("expected AlgebraicSimplify to eliminate x & 0")
	}

	val, ok := inspectStore(entry)
	if !ok {
		t.Fatal("expected a StoreInst")
	}
	c, isConst := val.(*minir.IntegerConst)
	if !isConst {
		t.Fatalf("expected IntegerConst, got %T", val)
	}
	if c.AsUint() != 0 {
		t.Fatalf("expected 0, got %d", c.AsUint())
	}
}

func TestAlgebraicSimplify_OrSelf(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimOrSelf", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	dst := minir.NewAnonTemp(i32)

	entry, _, fn := buildSimplifyFn("sim_or_self", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "or", Left: x, Right: x},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.AlgebraicSimplify(fn)
	if changed == 0 {
		t.Fatal("expected AlgebraicSimplify to eliminate x | x")
	}

	val, ok := inspectStore(entry)
	if !ok {
		t.Fatal("expected a StoreInst")
	}
	if val != x {
		t.Fatalf("expected store value to be x, got %T %v", val, val)
	}
}

func TestAlgebraicSimplify_XorSelf(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimXorSelf", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	dst := minir.NewAnonTemp(i32)

	entry, _, fn := buildSimplifyFn("sim_xor_self", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "xor", Left: x, Right: x},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.AlgebraicSimplify(fn)
	if changed == 0 {
		t.Fatal("expected AlgebraicSimplify to eliminate x ^ x")
	}

	val, ok := inspectStore(entry)
	if !ok {
		t.Fatal("expected a StoreInst")
	}
	c, isConst := val.(*minir.IntegerConst)
	if !isConst {
		t.Fatalf("expected IntegerConst 0, got %T %v", val, val)
	}
	if c.AsUint() != 0 {
		t.Fatalf("expected 0, got %d", c.AsUint())
	}
}

// ── shift by zero ─────────────────────────────────────────────────────────────

func TestAlgebraicSimplify_ShiftByZero(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimShlZ", Ty: i32, Linkage: minir.InternalLinkage}

	for _, op := range []string{"shl", "lshr", "ashr"} {
		t.Run(op, func(t *testing.T) {
			minir.ResetTempCounter()
			x := minir.NewTemp("x", i32)
			dst := minir.NewAnonTemp(i32)
			entry, _, fn := buildSimplifyFn("sim_shift_z_"+op, []minir.Instr{
				&minir.BinaryInst{Dst: dst, Op: op, Left: x, Right: minir.ConstInt("", 0, i32)},
				&minir.StoreInst{Val: dst, Addr: g.Ref()},
			})

			changed := miniropt.AlgebraicSimplify(fn)
			if changed == 0 {
				t.Fatalf("expected AlgebraicSimplify to eliminate %s x, 0", op)
			}

			val, ok := inspectStore(entry)
			if !ok {
				t.Fatal("expected a StoreInst")
			}
			if val != x {
				t.Fatalf("expected store value to be x, got %T %v", val, val)
			}
		})
	}
}

// ── ICmp identity: x == x / x != x ──────────────────────────────────────────

func TestAlgebraicSimplify_ICmpSelf(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	bTy := minir.I1()

	tests := []struct {
		pred    string
		wantVal bool
	}{
		{"eq", true},
		{"sle", true},
		{"ne", false},
		{"sgt", false},
	}

	for _, tc := range tests {
		t.Run(tc.pred, func(t *testing.T) {
			minir.ResetTempCounter()
			x := minir.NewTemp("x", i32)
			cmpDst := minir.NewAnonTemp(bTy)
			_, exit, fn := buildSimplifyFn("sim_icmp_self_"+tc.pred, []minir.Instr{
				&minir.ICmpInst{Dst: cmpDst, Pred: tc.pred, Left: x, Right: x},
			})
			fn.Result = bTy
			exit.Instrs = []minir.Instr{&minir.ReturnInst{Result: cmpDst}}
			exit.Term = exit.Instrs[0].(minir.Terminator)

			changed := miniropt.AlgebraicSimplify(fn)
			if changed == 0 {
				t.Fatalf("expected AlgebraicSimplify to simplify x %s x", tc.pred)
			}

			// The simplified result is materialised as a xor (bool materialiser).
			xorBin := func(b *minir.Block) *minir.BinaryInst {
				for _, ins := range b.Instrs {
					if bin, ok := ins.(*minir.BinaryInst); ok && bin.Op == "xor" {
						return bin
					}
				}
				return nil
			}(fn.Entry)

			if xorBin == nil {
				t.Fatal("expected bool to be materialised as xor in entry")
			}
			lc, ok := xorBin.Left.(*minir.IntegerConst)
			if !ok {
				t.Fatalf("expected IntegerConst on left of xor, got %T", xorBin.Left)
			}
			gotBool := lc.AsUint() != 0
			if gotBool != tc.wantVal {
				t.Fatalf("expected %v, got %v", tc.wantVal, gotBool)
			}
		})
	}
}

// ── no-op: no applicable identity ────────────────────────────────────────────

func TestAlgebraicSimplify_NoOpOnGenericBinary(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()
	g := &minir.GlobalVar{Name: "gSimNoop", Ty: i32, Linkage: minir.InternalLinkage}
	x := minir.NewTemp("x", i32)
	y := minir.NewTemp("y", i32)
	dst := minir.NewAnonTemp(i32)

	_, _, fn := buildSimplifyFn("sim_noop", []minir.Instr{
		&minir.BinaryInst{Dst: dst, Op: "add", Left: x, Right: y},
		&minir.StoreInst{Val: dst, Addr: g.Ref()},
	})

	changed := miniropt.AlgebraicSimplify(fn)
	if changed != 0 {
		t.Fatalf("expected no changes for x+y with no identities, got %d", changed)
	}
}

