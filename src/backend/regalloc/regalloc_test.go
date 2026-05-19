package regalloc

import (
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

func TestRewriteTerminatorUsesScratchRegisterForSpilledOperand(t *testing.T) {
	i64 := mir.NewScalarType("i64", 8)
	alloc := &regAllocResult{
		mapVRegToPReg: map[string]string{"live": "a0"},
		spillSlots:    map[string]int{"spill": 0},
		scratchRegs:   []string{"t0"},
	}
	frame := mir.NewFrameLayout()
	term := &mir.ReturnInstr{Value: &mir.Register{Name: "spill", Kind: mir.VirtualReg, Ty: i64}}

	rewritten, pre, err := rewriteTerminator(term, map[string]bool{"live": true}, alloc, frame, target.ABI{
		WordSize:     4,
		Align:        16,
		FramePointer: "s0",
		StackPointer: "sp",
	}, &blockAnalysis{})
	if err != nil {
		t.Fatalf("rewriteTerminator failed: %v", err)
	}
	if rewritten != term {
		t.Fatalf("expected terminator to be rewritten in place")
	}
	if len(pre) != 1 {
		t.Fatalf("expected one spill load before return, got %d", len(pre))
	}
	load, ok := pre[0].(*mir.LoadInstr)
	if !ok {
		t.Fatalf("expected prelude instruction to be a load, got %T", pre[0])
	}
	if load.Dst == nil || load.Dst.Name != "t0" {
		t.Fatalf("expected spilled value to use scratch register t0, got %#v", load.Dst)
	}
	mem, ok := load.Addr.(*mir.Memory)
	if !ok {
		t.Fatalf("expected load address to be memory, got %T", load.Addr)
	}
	off, ok := mem.Offset.(*mir.Immediate)
	if !ok {
		t.Fatalf("expected spill offset to be immediate, got %T", mem.Offset)
	}
	if got := off.Value; got != -4 {
		t.Fatalf("expected spill offset -4 for 4-byte word size, got %v", got)
	}
}

func TestRewriteMachineInstrSpillReload(t *testing.T) {
	i64 := mir.NewScalarType("i64", 8)
	alloc := &regAllocResult{
		mapVRegToPReg: map[string]string{"v0": "a0"},
		spillSlots:    map[string]int{"v0": 0},
		scratchRegs:   []string{"t0"},
	}
	frame := mir.NewFrameLayout()
	abi := target.ABI{
		WordSize:     4,
		Align:        16,
		FramePointer: "s0",
		StackPointer: "sp",
	}

	spill := &mir.MachineInstr{Op: "spill", Srcs: []mir.Operand{&mir.Register{Name: "v0", Kind: mir.VirtualReg, Ty: i64}}}
	rewritten, pre, post, err := rewriteInstr(spill, map[string]bool{"v0": true}, alloc, frame, abi, &blockAnalysis{})
	if err != nil {
		t.Fatalf("rewriteInstr(spill) failed: %v", err)
	}
	if len(pre) != 0 || len(post) != 0 {
		t.Fatalf("spill pre/post = %d/%d, want 0/0", len(pre), len(post))
	}
	store, ok := rewritten.(*mir.StoreInstr)
	if !ok {
		t.Fatalf("rewriteInstr(spill) = %T, want *mir.StoreInstr", rewritten)
	}
	if reg, ok := store.Value.(*mir.Register); !ok || reg.Name != "a0" {
		t.Fatalf("spill store value = %#v, want a0", store.Value)
	}
	if mem, ok := store.Addr.(*mir.Memory); !ok {
		t.Fatalf("spill addr = %T, want *mir.Memory", store.Addr)
	} else if off, ok := mem.Offset.(*mir.Immediate); !ok || off.Value != -4 {
		t.Fatalf("spill offset = %#v, want -4", mem.Offset)
	}

	reload := &mir.MachineInstr{Op: "reload", Dsts: []*mir.Register{{Name: "v0", Kind: mir.VirtualReg, Ty: i64}}}
	rewritten, pre, post, err = rewriteInstr(reload, map[string]bool{"v0": true}, alloc, frame, abi, &blockAnalysis{})
	if err != nil {
		t.Fatalf("rewriteInstr(reload) failed: %v", err)
	}
	if len(pre) != 0 || len(post) != 0 {
		t.Fatalf("reload pre/post = %d/%d, want 0/0", len(pre), len(post))
	}
	load, ok := rewritten.(*mir.LoadInstr)
	if !ok {
		t.Fatalf("rewriteInstr(reload) = %T, want *mir.LoadInstr", rewritten)
	}
	if load.Dst == nil || load.Dst.Name != "a0" {
		t.Fatalf("reload dst = %#v, want a0", load.Dst)
	}
	if mem, ok := load.Addr.(*mir.Memory); !ok {
		t.Fatalf("reload addr = %T, want *mir.Memory", load.Addr)
	} else if off, ok := mem.Offset.(*mir.Immediate); !ok || off.Value != -4 {
		t.Fatalf("reload offset = %#v, want -4", mem.Offset)
	}
}

func TestMaterializeEdgeBlocksRedirectsCriticalEdge(t *testing.T) {
	i64 := mir.NewScalarType("i64", 8)
	i1 := mir.NewScalarType("i1", 1)

	pred := mir.NewBlock(0, "pred")
	join := mir.NewBlock(1, "join")
	other := mir.NewBlock(2, "other")

	pred.Term = &mir.CondBrInstr{
		Cond:       &mir.Register{Name: "cond", Kind: mir.VirtualReg, Ty: i1},
		TrueLabel:  "join",
		FalseLabel: "other",
	}
	pred.AddSucc(join)
	pred.AddSucc(other)
	join.AddPred(pred)
	other.AddPred(pred)

	fn := mir.NewFunction("f", i64)
	fn.AddBlock(pred)
	fn.AddBlock(join)
	fn.AddBlock(other)
	fn.SetEntry(pred)
	fn.SetExit(join)

	predBA := &blockAnalysis{block: pred}
	joinBA := &blockAnalysis{block: join}
	otherBA := &blockAnalysis{block: other}
	fa := &functionAnalysis{
		blocks: []*blockAnalysis{predBA, joinBA, otherBA},
		byID: map[int]*blockAnalysis{
			pred.ID:  predBA,
			join.ID:  joinBA,
			other.ID: otherBA,
		},
		byName: map[string]*blockAnalysis{
			pred.Label:  predBA,
			join.Label:  joinBA,
			other.Label: otherBA,
		},
	}

	load := &mir.LoadInstr{
		Dst: &mir.Register{Name: "t0", Kind: mir.PhysicalReg, Ty: i64},
		Addr: &mir.Memory{
			Base:   &mir.Register{Name: "sp", Kind: mir.PhysicalReg, Ty: i64},
			Offset: &mir.Immediate{Value: -8, Ty: i64},
			Ty:     i64,
		},
	}

	if err := materializeEdgeBlocks(fn, fa, map[edgeKey][]mir.Instr{
		{predID: pred.ID, joinLabel: join.Label}: {load},
	}); err != nil {
		t.Fatalf("materializeEdgeBlocks failed: %v", err)
	}

	edge := fn.BlockByLabel("pred.__ra_edge__join")
	if edge == nil {
		t.Fatalf("expected a split edge block between pred and join")
	}
	if len(fn.Blocks) != 4 {
		t.Fatalf("expected 4 blocks after edge splitting, got %d", len(fn.Blocks))
	}
	if _, ok := pred.Succs[join.ID]; ok {
		t.Fatalf("expected pred -> join edge to be redirected away from join")
	}
	if pred.Succs[edge.ID] != edge {
		t.Fatalf("expected pred to target edge block %q", edge.Label)
	}
	if join.Preds[pred.ID] != nil {
		t.Fatalf("expected join to stop referencing pred directly")
	}
	if join.Preds[edge.ID] != edge {
		t.Fatalf("expected join to reference edge block %q", edge.Label)
	}
	if term, ok := pred.Term.(*mir.CondBrInstr); !ok || term.TrueLabel != edge.Label {
		t.Fatalf("expected conditional branch to be retargeted to %q, got %#v", edge.Label, pred.Term)
	}
	if len(edge.Instrs) != 1 {
		t.Fatalf("expected edge block to contain one load, got %d", len(edge.Instrs))
	}
	if jump, ok := edge.Term.(*mir.JumpInstr); !ok || jump.Target != join.Label {
		t.Fatalf("expected edge block to jump to %q, got %#v", join.Label, edge.Term)
	}
}

// TestColorGraphPrecolorsParams verifies that function parameters are mapped to
// the correct ABI argument registers before graph coloring runs.
// On ARM64 the first int param must map to x0, the second to x1, etc.
func TestColorGraphPrecolorsParams(t *testing.T) {
	i64 := mir.NewScalarType("i64", 8)
	abi := target.ABI{
		WordSize:    8,
		Align:       16,
		IntArgRegs:  []string{"x0", "x1", "x2", "x3"},
		IntRetRegs:  []string{"x0"},
		CallerSaved: []string{"x0", "x1", "x2", "x3", "x9", "x10"},
		CalleeSaved: []string{"x19", "x20"},
		StackPointer: "sp",
		FramePointer: "x29",
		LinkRegister: "x30",
	}

	// Build a minimal function:  define i64 @add(i64 %a, i64 %b) { ret %a }
	fn := mir.NewFunction("add", i64)
	fn.AddParam(&mir.Param{Name: "a", Type: i64})
	fn.AddParam(&mir.Param{Name: "b", Type: i64})

	blk := mir.NewBlock(0, "entry")
	blk.Term = &mir.ReturnInstr{Value: &mir.Register{Name: "a", Kind: mir.VirtualReg, Ty: i64}}
	fn.AddBlock(blk)
	fn.SetEntry(blk)
	fn.SetExit(blk)

	analysis := analyzeFunction(fn)
	colors, scratch := abiColorPools(abi)

	result, err := colorGraph(fn, analysis, colors, scratch, abi)
	if err != nil {
		t.Fatalf("colorGraph failed: %v", err)
	}

	if got := result.mapVRegToPReg["a"]; got != "x0" {
		t.Errorf("first parameter %q mapped to %q, want %q", "a", got, "x0")
	}
	if got := result.mapVRegToPReg["b"]; got != "x1" {
		t.Errorf("second parameter %q mapped to %q, want %q", "b", got, "x1")
	}
}

// TestColorGraphPrecolorsThreeParams verifies three-parameter pre-coloring.
func TestColorGraphPrecolorsThreeParams(t *testing.T) {
	i64 := mir.NewScalarType("i64", 8)
	abi := target.ABI{
		WordSize:    8,
		Align:       16,
		IntArgRegs:  []string{"x0", "x1", "x2", "x3"},
		IntRetRegs:  []string{"x0"},
		CallerSaved: []string{"x0", "x1", "x2", "x3", "x9", "x10"},
		CalleeSaved: []string{"x19", "x20"},
		StackPointer: "sp",
		FramePointer: "x29",
		LinkRegister: "x30",
	}

	fn := mir.NewFunction("fib", i64)
	fn.AddParam(&mir.Param{Name: "n", Type: i64})
	fn.AddParam(&mir.Param{Name: "m", Type: i64})
	fn.AddParam(&mir.Param{Name: "k", Type: i64})

	blk := mir.NewBlock(0, "entry")
	blk.Term = &mir.ReturnInstr{Value: &mir.Register{Name: "n", Kind: mir.VirtualReg, Ty: i64}}
	fn.AddBlock(blk)
	fn.SetEntry(blk)
	fn.SetExit(blk)

	analysis := analyzeFunction(fn)
	colors, scratch := abiColorPools(abi)

	result, err := colorGraph(fn, analysis, colors, scratch, abi)
	if err != nil {
		t.Fatalf("colorGraph failed: %v", err)
	}

	for i, tc := range []struct{ param, want string }{
		{"n", "x0"}, {"m", "x1"}, {"k", "x2"},
	} {
		if got := result.mapVRegToPReg[tc.param]; got != tc.want {
			t.Errorf("param[%d] %q mapped to %q, want %q", i, tc.param, got, tc.want)
		}
	}
}

func TestRunOnEmptyProgram(t *testing.T) {
	prog := mir.NewProgram()
	out, err := Run(prog, target.NewRISCV64Target())
	if err != nil {
		t.Fatalf("Run(empty program) failed: %v", err)
	}
	if out != prog {
		t.Fatalf("expected Run to operate in place")
	}
	if len(out.Modules) != 0 {
		t.Fatalf("expected empty program to remain empty, got %d modules", len(out.Modules))
	}
}

func TestRunPopulatesFrameForMinimalFunction(t *testing.T) {
	fn := mir.NewFunction("main", mir.NewScalarType("i64", 8))
	blk := mir.NewBlock(0, "entry")
	blk.Term = &mir.ReturnInstr{}
	fn.AddBlockAndSetEntry(blk)
	fn.SetExit(blk)

	mod := mir.NewModule("M")
	mod.AddFunction(fn)
	prog := mir.NewProgram(mod)

	out, err := Run(prog, target.NewRISCV64Target())
	if err != nil {
		t.Fatalf("Run(minimal function) failed: %v", err)
	}
	if out != prog {
		t.Fatalf("expected Run to reuse the input program")
	}
	if fn.Frame == nil {
		t.Fatal("expected register allocation to populate a frame")
	}
	if fn.Frame.SpillSlots == nil {
		t.Fatal("expected frame spill-slot map to be initialized")
	}
	if got := out.ModuleByName("M"); got == nil || got.FunctionByName("main") == nil {
		t.Fatal("expected module/function to remain present after allocation")
	}
}
