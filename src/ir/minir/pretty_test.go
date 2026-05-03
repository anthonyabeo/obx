package minir

import "testing"

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
    out := FormatFunction(fn)
    t.Logf("Formatted function:\n%s", out)
}

