package miniropt_test

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
)

// ── shared helpers ────────────────────────────────────────────────────────────

// buildSimpleFn constructs a two-block (entry → exit) minir.Function whose
// entry block contains bodyInstrs followed by an unconditional jump to exit.
func buildSimpleFn(name string, bodyInstrs []minir.Instr) *minir.Function {
	exit := &minir.Block{
		ID: 1, Label: name + "_exit",
		Preds: make(map[int]*minir.Block),
		Succs: make(map[int]*minir.Block),
	}
	exitRet := &minir.ReturnInst{}
	exit.Instrs = []minir.Instr{exitRet}
	exit.Term = exitRet

	jmp := &minir.JumpInst{Target: exit.Label}
	entry := &minir.Block{
		ID: 0, Label: "entry",
		Preds: make(map[int]*minir.Block),
		Succs: make(map[int]*minir.Block),
	}
	entry.Instrs = append(append([]minir.Instr{}, bodyInstrs...), jmp)
	entry.Term = jmp

	entry.AddSucc(exit)
	exit.AddPred(entry)

	return &minir.Function{
		FnName: name,
		Entry:  entry,
		Exit:   exit,
		Blocks: map[int]*minir.Block{0: entry, 1: exit},
	}
}

func countLoadsFwd(fn *minir.Function) int {
	n := 0
	for _, b := range fn.Blocks {
		for _, ins := range b.Instrs {
			if _, ok := ins.(*minir.LoadInst); ok {
				n++
			}
		}
	}
	return n
}

func findICmpFwd(fn *minir.Function) *minir.ICmpInst {
	for _, ins := range fn.Entry.Instrs {
		if c, ok := ins.(*minir.ICmpInst); ok {
			return c
		}
	}
	return nil
}

// ── Test 1: store-then-load of a global is forwarded ─────────────────────────
//
// Mirrors the fib/__init_Main pattern:
//
//	%t31 = call fib(21)
//	store %t31, @res           ← writes @res
//	%v   = load i32, @res      ← forwarded → %t31
//	%c   = icmp.eq %v, 10946   ← rewritten → icmp.eq %t31, 10946
func TestLoadForward_GlobalForwarded(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()

	res := &minir.GlobalVar{Name: "Main$res", Ty: i32, Linkage: minir.InternalLinkage}
	resRef := res.Ref()

	callDst := minir.NewAnonTemp(i32)
	loadDst := minir.NewAnonTemp(i32)
	icmpDst := minir.NewAnonTemp(minir.I1())

	callInst := &minir.CallInst{
		Dst: callDst, Callee: "Main$fib",
		Args: []minir.Value{minir.NewConst("21", int64(21), i32)},
	}
	storeInst := &minir.StoreInst{Val: callDst, Addr: resRef}
	loadInst := &minir.LoadInst{Dst: loadDst, Addr: resRef}
	icmpInst := &minir.ICmpInst{
		Dst: icmpDst, Pred: "eq",
		Left: loadDst, Right: minir.NewConst("10946", int64(10946), i32),
	}

	fn := buildSimpleFn("init", []minir.Instr{callInst, storeInst, loadInst, icmpInst})

	eliminated := miniropt.LoadForward(fn)

	if eliminated != 1 {
		t.Errorf("expected 1 load eliminated, got %d", eliminated)
	}
	if countLoadsFwd(fn) != 0 {
		t.Error("expected no LoadInst to remain after forwarding")
	}

	// icmp must now use callDst (%t31) directly.
	cmp := findICmpFwd(fn)
	if cmp == nil {
		t.Fatal("ICmpInst not found after forwarding")
	}
	if cmp.Left != callDst {
		t.Errorf("expected icmp left = callDst (%s), got %s", callDst, cmp.Left)
	}
}

// ── Test 2: intervening call flushes knowledge ────────────────────────────────
//
//	store %x, @g
//	call foo()        ← flushes: @g may have been modified
//	%v = load @g      ← must NOT be forwarded
func TestLoadForward_CallFlushesKnowledge(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()

	g := &minir.GlobalVar{Name: "g", Ty: i32, Linkage: minir.InternalLinkage}
	gRef := g.Ref()

	xTemp := minir.NewAnonTemp(i32)
	loadDst := minir.NewAnonTemp(i32)

	fn := buildSimpleFn("callflush", []minir.Instr{
		&minir.StoreInst{Val: xTemp, Addr: gRef},
		&minir.CallInst{Callee: "foo"},
		&minir.LoadInst{Dst: loadDst, Addr: gRef},
	})

	eliminated := miniropt.LoadForward(fn)

	if eliminated != 0 {
		t.Errorf("expected 0 loads eliminated (call should flush), got %d", eliminated)
	}
	if countLoadsFwd(fn) != 1 {
		t.Error("expected the load to survive after intervening call")
	}
}

// ── Test 3: back-to-back loads — second forwarded from first ──────────────────
//
//	%a = load i32, @g   ← first; records known[@g] = %a
//	%b = load i32, @g   ← forwarded → %a
//	%c = add %a, %b     ← rewritten → add %a, %a
func TestLoadForward_RepeatedLoad(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()

	g := &minir.GlobalVar{Name: "g2", Ty: i32, Linkage: minir.InternalLinkage}
	gRef := g.Ref()

	aDst := minir.NewAnonTemp(i32)
	bDst := minir.NewAnonTemp(i32)
	addDst := minir.NewAnonTemp(i32)

	fn := buildSimpleFn("repeated", []minir.Instr{
		&minir.LoadInst{Dst: aDst, Addr: gRef},
		&minir.LoadInst{Dst: bDst, Addr: gRef},
		&minir.BinaryInst{Dst: addDst, Op: "add", Left: aDst, Right: bDst},
	})

	eliminated := miniropt.LoadForward(fn)

	if eliminated != 1 {
		t.Errorf("expected 1 load eliminated (repeated load), got %d", eliminated)
	}
	// The add must now use aDst for both operands.
	for _, ins := range fn.Entry.Instrs {
		if b, ok := ins.(*minir.BinaryInst); ok && b.Op == "add" {
			if b.Left != aDst || b.Right != aDst {
				t.Errorf("add operands not both forwarded to aDst: left=%s right=%s",
					b.Left, b.Right)
			}
		}
	}
}

// ── Test 4: store-to-alloca forwarded to load from same alloca ────────────────
//
//	%addr = alloca i32         (IsAddr=true; escaped so Mem2Reg left it alone)
//	store 99, %addr
//	%v = load i32, %addr       ← forwarded → Const(99)
//	%c = icmp.eq i32 %v, 99   ← rewritten → icmp.eq 99, 99
func TestLoadForward_AllocaForwarded(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()

	addr := minir.NewTemp("addr", minir.Ptr(i32))
	addr.IsAddr = true

	loadDst := minir.NewAnonTemp(i32)
	icmpDst := minir.NewAnonTemp(minir.I1())
	c99 := minir.NewConst("99", int64(99), i32)

	fn := buildSimpleFn("allocafwd", []minir.Instr{
		&minir.AllocaInst{Dst: addr, AllocType: i32},
		&minir.StoreInst{Val: c99, Addr: addr},
		&minir.LoadInst{Dst: loadDst, Addr: addr},
		&minir.ICmpInst{Dst: icmpDst, Pred: "eq", Left: loadDst, Right: c99},
	})

	eliminated := miniropt.LoadForward(fn)

	if eliminated != 1 {
		t.Errorf("expected 1 load eliminated (alloca forward), got %d", eliminated)
	}

	cmp := findICmpFwd(fn)
	if cmp == nil {
		t.Fatal("ICmpInst not found after forwarding")
	}
	// %v must have been replaced with the constant 99.
	if _, isConst := cmp.Left.(minir.Constant); !isConst {
		t.Errorf("expected icmp left to be a Constant after forwarding, got %T (%s)",
			cmp.Left, cmp.Left)
	}
}

// ── Test 5: two different globals are tracked independently ───────────────────
//
//	store %a, @x
//	store %b, @y
//	%va = load i32, @x    ← forwarded → %a
//	%vb = load i32, @y    ← forwarded → %b
func TestLoadForward_TwoGlobalsIndependent(t *testing.T) {
	minir.ResetTempCounter()
	i32 := minir.I32()

	gx := &minir.GlobalVar{Name: "x", Ty: i32, Linkage: minir.InternalLinkage}
	gy := &minir.GlobalVar{Name: "y", Ty: i32, Linkage: minir.InternalLinkage}

	aTemp := minir.NewAnonTemp(i32)
	bTemp := minir.NewAnonTemp(i32)
	vaDst := minir.NewAnonTemp(i32)
	vbDst := minir.NewAnonTemp(i32)
	addDst := minir.NewAnonTemp(i32)

	fn := buildSimpleFn("twoglobals", []minir.Instr{
		&minir.StoreInst{Val: aTemp, Addr: gx.Ref()},
		&minir.StoreInst{Val: bTemp, Addr: gy.Ref()},
		&minir.LoadInst{Dst: vaDst, Addr: gx.Ref()},
		&minir.LoadInst{Dst: vbDst, Addr: gy.Ref()},
		&minir.BinaryInst{Dst: addDst, Op: "add", Left: vaDst, Right: vbDst},
	})

	eliminated := miniropt.LoadForward(fn)

	if eliminated != 2 {
		t.Errorf("expected 2 loads eliminated, got %d", eliminated)
	}
	// The add must use aTemp and bTemp directly.
	for _, ins := range fn.Entry.Instrs {
		if b, ok := ins.(*minir.BinaryInst); ok && b.Op == "add" {
			if b.Left != aTemp {
				t.Errorf("add left: want %s, got %s", aTemp, b.Left)
			}
			if b.Right != bTemp {
				t.Errorf("add right: want %s, got %s", bTemp, b.Right)
			}
		}
	}
}
