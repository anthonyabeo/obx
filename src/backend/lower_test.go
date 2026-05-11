package backend

import (
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
	btarget "github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/minir"
)

func TestLowerAndPlan(t *testing.T) {
	lhs := minir.NewTemp("lhs", minir.I32())
	rhs := minir.NewTemp("rhs", minir.I32())
	cond := minir.NewTemp("cond", minir.I1())
	thenVal := minir.NewTemp("thenVal", minir.I32())
	elseVal := minir.NewTemp("elseVal", minir.I32())
	joined := minir.NewTemp("joined", minir.I32())

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	thenBlk := &minir.Block{ID: 1, Label: "then", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	elseBlk := &minir.Block{ID: 2, Label: "else", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	joinBlk := &minir.Block{ID: 3, Label: "join", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	cmp := &minir.ICmpInst{Dst: cond, Pred: "eq", Left: lhs, Right: rhs}
	br := &minir.CondBrInst{Cond: cond, TrueLabel: "then", FalseLabel: "else"}
	entry.Instrs = []minir.Instr{cmp, br}
	entry.Term = br

	thenAdd := &minir.BinaryInst{Dst: thenVal, Op: "add", Left: lhs, Right: minir.NewConst("one", 1, minir.I32())}
	thenJmp := &minir.JumpInst{Target: "join"}
	thenBlk.Instrs = []minir.Instr{thenAdd, thenJmp}
	thenBlk.Term = thenJmp

	elseSub := &minir.BinaryInst{Dst: elseVal, Op: "sub", Left: lhs, Right: minir.NewConst("one", 1, minir.I32())}
	elseJmp := &minir.JumpInst{Target: "join"}
	elseBlk.Instrs = []minir.Instr{elseSub, elseJmp}
	elseBlk.Term = elseJmp

	phi := &minir.PhiInst{Dst: joined, Args: []minir.PhiArm{{BlockLabel: "then", Val: thenVal}, {BlockLabel: "else", Val: elseVal}}}
	ret := &minir.ReturnInst{Result: joined}
	joinBlk.Instrs = []minir.Instr{phi, ret}
	joinBlk.Term = ret

	entry.Succs[1] = thenBlk
	entry.Succs[2] = elseBlk
	thenBlk.Preds[0] = entry
	thenBlk.Succs[3] = joinBlk
	elseBlk.Preds[0] = entry
	elseBlk.Succs[3] = joinBlk
	joinBlk.Preds[1] = thenBlk
	joinBlk.Preds[2] = elseBlk

	fn := &minir.Function{
		FnName: "main",
		Result: minir.I32(),
		Entry:  entry,
		Exit:   joinBlk,
		Blocks: map[int]*minir.Block{0: entry, 1: thenBlk, 2: elseBlk, 3: joinBlk},
	}
	mod := &minir.Module{Name: "M", Functions: []*minir.Function{fn}}
	prog := &minir.Program{Modules: []*minir.Module{mod}}

	lowered, err := LowerAndPlan(prog, btarget.NewRISCV64Target())
	if err != nil {
		t.Fatalf("LowerAndPlan failed: %v", err)
	}
	m := lowered.MIR.ModuleByName("M")
	if m == nil {
		t.Fatalf("expected lowered module M")
	}
	lfn := m.FunctionByName("main")
	if lfn == nil {
		t.Fatalf("expected lowered function main")
	}
	if len(lfn.Blocks) != 4 {
		t.Fatalf("expected 4 lowered blocks, got %d", len(lfn.Blocks))
	}
	entryLowered := lfn.BlockByLabel("entry")
	if entryLowered == nil {
		t.Fatalf("missing lowered entry block")
	}
	if _, ok := entryLowered.Instrs[0].(*mir.CompareInstr); !ok {
		t.Fatalf("expected first lowered instruction to be CompareInstr, got %T", entryLowered.Instrs[0])
	}
	plans := lowered.Plans["M.main"]
	if plans == nil {
		t.Fatalf("expected plans for M.main")
	}
	if len(plans.Phi) != 1 {
		t.Fatalf("expected one phi plan, got %d", len(plans.Phi))
	}
	if plans.Phi[0].JoinLabel != "join" || len(plans.Phi[0].Edges) != 2 {
		t.Fatalf("unexpected phi plan: %#v", plans.Phi[0])
	}
	if len(plans.Calls) != 0 || len(plans.Switch) != 0 {
		t.Fatalf("unexpected non-empty plans: %#v", plans)
	}
}
