package core

import (
	"testing"
)

// Test that builds the sample IR from the spec, runs the verifier, and
// prints the formatted function. The test asserts there are no verifier
// errors.
func TestFormatAndVerifySample(t *testing.T) {
	// Types (use canonical factories)
	i32 := I32()
	boolT := Bool()

	// Reset temp ID counter for deterministic IDs in tests.
	tempIDCounter = 0

	// Temps (params and constants) — use constructor helpers for canonicalization
	paramX := NewTemp("x", i32)
	const0 := NewConst("0", 0, i32)
	const1 := NewConst("1", 1, i32)

	// Blocks
	entry := &Block{ID: 0, Label: "entry", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	then := &Block{ID: 1, Label: "then", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	els := &Block{ID: 2, Label: "else", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	merge := &Block{ID: 3, Label: "merge", Preds: map[int]*Block{}, Succs: map[int]*Block{}}

	// Instrs
	// %tmp1 = alloca i32
	tmp1 := NewTemp("tmp1", Ptr(i32))
	tmp1.IsAddr = true
	alloca := &AllocaInst{Dst: tmp1, AllocType: i32}

	// store %x, %tmp1
	store := &StoreInst{Val: paramX, Addr: tmp1}

	// %v = load %tmp1
	v := NewTemp("v", i32)
	load := &LoadInst{Dst: v, Addr: tmp1}

	// %cond = icmp.eq %v, %0
	cond := NewTemp("cond", boolT)
	icmp := &ICmpInst{Dst: cond, Pred: "eq", Left: v, Right: const0}

	// br %cond, then, else
	br := &CondBrInst{Cond: cond, TrueLabel: then.Label, FalseLabel: els.Label}

	// then: %a = add %v, 1 ; jmp merge
	a := NewTemp("a", i32)
	add := &BinaryInst{Dst: a, Op: "add", Left: v, Right: const1}
	jmpThen := &JumpInst{Target: merge.Label}

	// else: %b = sub %v, 1 ; jmp merge
	b := NewTemp("b", i32)
	sub := &BinaryInst{Dst: b, Op: "sub", Left: v, Right: const1}
	jmpElse := &JumpInst{Target: merge.Label}

	// merge: %r = phi [then:%a, else:%b] ; ret %r
	r := NewTemp("r", i32)
	phi := &PhiInst{Dst: r, Args: []PhiArm{{BlockLabel: then.Label, Val: a}, {BlockLabel: els.Label, Val: b}}}
	ret := &ReturnInst{Result: r}

	// assemble blocks (terminators must be last in Instrs and Term set)
	entry.Instrs = []Instr{alloca, store, load, icmp, br}
	entry.Term = br

	then.Instrs = []Instr{add, jmpThen}
	then.Term = jmpThen

	els.Instrs = []Instr{sub, jmpElse}
	els.Term = jmpElse

	merge.Instrs = []Instr{phi, ret}
	merge.Term = ret

	// link CFG (use AddSucc/AddPred so ordering is recorded)
	entry.AddSucc(then)
	entry.AddSucc(els)
	then.AddPred(entry)
	els.AddPred(entry)

	then.AddSucc(merge)
	els.AddSucc(merge)
	merge.AddPred(then)
	merge.AddPred(els)

	// function
	fn := &Function{
		FnName: "add1",
		Params: []*Temp{paramX},
		Result: i32,
		Entry:  entry,
		Exit:   merge,
		Blocks: map[int]*Block{entry.ID: entry, then.ID: then, els.ID: els, merge.ID: merge},
	}

	// verify
	errs := VerifyIR(fn)
	if len(errs) != 0 {
		for _, e := range errs {
			t.Logf("verify error: %s", e.Error())
		}
		t.Fatalf("expected no verify errors, got %d", len(errs))
	}

	// print
	//out := FormatFunction(fn)
	//t.Logf("Formatted function:\n%s", out)
}

// Test a simple counted loop implemented with a phi and conditional branch.
func TestFormatAndVerifyCountedLoop(t *testing.T) {
	i32 := I32()

	// deterministic temps
	tempIDCounter = 0

	// constants
	zero := NewConst("0", 0, i32)
	one := NewConst("1", 1, i32)
	ten := NewConst("10", 10, i32)

	// blocks
	entry := &Block{ID: 0, Label: "entry", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	cond := &Block{ID: 1, Label: "cond", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	body := &Block{ID: 2, Label: "body", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	exit := &Block{ID: 3, Label: "exit", Preds: map[int]*Block{}, Succs: map[int]*Block{}}

	// temps
	i := NewTemp("i", i32)
	iNext := NewTemp("i.next", i32)
	cmp := NewTemp("lt", Bool())

	// instrs
	// entry -> cond
	entry.Term = &JumpInst{Target: cond.Label}
	entry.Instrs = []Instr{entry.Term}

	// cond: %i = phi [entry:0, body:%i.next]
	phi := &PhiInst{Dst: i, Args: []PhiArm{{BlockLabel: entry.Label, Val: zero}, {BlockLabel: body.Label, Val: iNext}}}
	icmp := &ICmpInst{Dst: cmp, Pred: "slt", Left: i, Right: ten}
	br := &CondBrInst{Cond: cmp, TrueLabel: body.Label, FalseLabel: exit.Label}
	cond.Instrs = []Instr{phi, icmp, br}
	cond.Term = br

	// body: %i.next = add %i, 1 ; jmp cond
	add := &BinaryInst{Dst: iNext, Op: "add", Left: i, Right: one}
	jmp := &JumpInst{Target: cond.Label}
	body.Instrs = []Instr{add, jmp}
	body.Term = jmp

	// exit: ret %i
	ret := &ReturnInst{Result: i}
	exit.Instrs = []Instr{ret}
	exit.Term = ret

	// link CFG
	entry.AddSucc(cond)
	cond.AddPred(entry)

	cond.AddSucc(body)
	cond.AddSucc(exit)
	body.AddPred(cond)
	exit.AddPred(cond)

	body.AddSucc(cond)
	cond.AddPred(body)

	fn := &Function{
		FnName: "countloop",
		Params: nil,
		Result: i32,
		Entry:  entry,
		Exit:   exit,
		Blocks: map[int]*Block{entry.ID: entry, cond.ID: cond, body.ID: body, exit.ID: exit},
	}

	errs := VerifyIR(fn)
	if len(errs) != 0 {
		for _, e := range errs {
			t.Logf("verify error: %s", e.Error())
		}
		t.Fatalf("expected no verify errors, got %d", len(errs))
	}

	//out := FormatFunction(fn)
	//t.Logf("Formatted counted loop:\n%s", out)
}

// Test a simple while-style loop where the condition is evaluated at the top.
func TestFormatAndVerifyWhileLoop(t *testing.T) {
	i32 := I32()

	tempIDCounter = 0

	zero := NewConst("0", 0, i32)
	one := NewConst("1", 1, i32)

	entry := &Block{ID: 0, Label: "entry", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	loop := &Block{ID: 1, Label: "loop", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	body := &Block{ID: 2, Label: "body", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	exit := &Block{ID: 3, Label: "exit", Preds: map[int]*Block{}, Succs: map[int]*Block{}}

	// temps
	v := NewTemp("v", i32)
	cmp := NewTemp("neq", Bool())
	vNext := NewTemp("v.next", i32)

	// entry initializes v and jumps to loop
	init := NewConst("5", 5, i32)
	entry.Term = &JumpInst{Target: loop.Label}
	entry.Instrs = []Instr{entry.Term}

	// loop: %v = phi [entry:5, body:%v.next]
	phi := &PhiInst{Dst: v, Args: []PhiArm{{BlockLabel: entry.Label, Val: init}, {BlockLabel: body.Label, Val: vNext}}}
	icmp := &ICmpInst{Dst: cmp, Pred: "ne", Left: v, Right: zero}
	br := &CondBrInst{Cond: cmp, TrueLabel: body.Label, FalseLabel: exit.Label}
	loop.Instrs = []Instr{phi, icmp, br}
	loop.Term = br

	// body: do something (v = v - 1) then jump back to loop
	dec := &BinaryInst{Dst: vNext, Op: "sub", Left: v, Right: one}
	jmp := &JumpInst{Target: loop.Label}
	body.Instrs = []Instr{dec, jmp}
	body.Term = jmp

	// exit: ret v
	ret := &ReturnInst{Result: v}
	exit.Instrs = []Instr{ret}
	exit.Term = ret

	// link CFG
	entry.AddSucc(loop)
	loop.AddPred(entry)

	loop.AddSucc(body)
	loop.AddSucc(exit)
	body.AddPred(loop)
	exit.AddPred(loop)

	body.AddSucc(loop)
	loop.AddPred(body)

	fn := &Function{
		FnName: "whileloop",
		Params: nil,
		Result: i32,
		Entry:  entry,
		Exit:   exit,
		Blocks: map[int]*Block{entry.ID: entry, loop.ID: loop, body.ID: body, exit.ID: exit},
	}

	errs := VerifyIR(fn)
	if len(errs) != 0 {
		for _, e := range errs {
			t.Logf("verify error: %s", e.Error())
		}
		t.Fatalf("expected no verify errors, got %d", len(errs))
	}

	//out := FormatFunction(fn)
	//t.Logf("Formatted while loop:\n%s", out)
}
