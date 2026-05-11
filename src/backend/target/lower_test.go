package target

import (
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

func TestSupportsIntegerScalar(t *testing.T) {
	if !SupportsIntegerScalar(mir.NewScalarType("i32", 4)) {
		t.Fatalf("expected i32 scalar to be supported")
	}
	if SupportsIntegerScalar(mir.NewPointerType(mir.NewScalarType("i32", 4), 8)) {
		t.Fatalf("expected pointer type to be rejected")
	}
	if SupportsIntegerScalar(nil) {
		t.Fatalf("expected nil type to be rejected")
	}
}

func TestLinearizeParallelCopyBreaksCycle(t *testing.T) {
	x := mir.NewRegister("x", mir.VirtualReg, mir.NewScalarType("i32", 4))
	y := mir.NewRegister("y", mir.VirtualReg, mir.NewScalarType("i32", 4))

	moves := LinearizeParallelCopy(ParallelCopy{Copies: []Copy{
		{Dst: x, Src: y},
		{Dst: y, Src: x},
	}})

	if len(moves) != 3 {
		t.Fatalf("expected 3 moves after cycle breaking, got %d", len(moves))
	}
	if moves[0].Dst == nil || moves[0].Dst.Name != "__pc_tmp0" {
		t.Fatalf("expected first move to save into temp, got %#v", moves[0].Dst)
	}
	if got := moves[1].Dst.Name; got != "x" {
		t.Fatalf("expected second move to target x, got %s", got)
	}
	if got := moves[2].Dst.Name; got != "y" {
		t.Fatalf("expected third move to target y, got %s", got)
	}
}

func TestBuildPhiPlanGroupsByPredecessor(t *testing.T) {
	x := mir.NewRegister("x", mir.VirtualReg, mir.NewScalarType("i32", 4))
	y := mir.NewRegister("y", mir.VirtualReg, mir.NewScalarType("i32", 4))
	z := mir.NewRegister("z", mir.VirtualReg, mir.NewScalarType("i32", 4))

	plan, err := BuildPhiPlan("join", []*mir.PhiInstr{
		{Dst: x, Arms: []mir.PhiArm{{BlockLabel: "L1", Value: y}, {BlockLabel: "L2", Value: z}}},
		{Dst: y, Arms: []mir.PhiArm{{BlockLabel: "L1", Value: x}, {BlockLabel: "L2", Value: z}}},
	})
	if err != nil {
		t.Fatalf("BuildPhiPlan failed: %v", err)
	}
	if plan.JoinLabel != "join" {
		t.Fatalf("unexpected join label: %s", plan.JoinLabel)
	}
	if len(plan.Edges) != 2 {
		t.Fatalf("expected 2 predecessor edges, got %d", len(plan.Edges))
	}
	if plan.Edges[0].PredLabel != "L1" || len(plan.Edges[0].Copies.Copies) != 2 {
		t.Fatalf("unexpected L1 edge plan: %#v", plan.Edges[0])
	}
	if plan.Edges[1].PredLabel != "L2" || len(plan.Edges[1].Copies.Copies) != 2 {
		t.Fatalf("unexpected L2 edge plan: %#v", plan.Edges[1])
	}
}

func TestBuildSwitchPlanUsesJumpTableForDenseCases(t *testing.T) {
	key := mir.NewRegister("k", mir.VirtualReg, mir.NewScalarType("i32", 4))
	sw := &mir.SwitchInstr{
		Value:   key,
		Default: "Ldefault",
		Arms: []mir.SwitchArm{
			{Value: mir.NewImmediate(1, mir.NewScalarType("i32", 4)), Label: "L1"},
			{Value: mir.NewImmediate(2, mir.NewScalarType("i32", 4)), Label: "L2"},
			{Value: mir.NewImmediate(3, mir.NewScalarType("i32", 4)), Label: "L3"},
		},
	}

	plan, err := BuildSwitchPlan(sw, ABI{JumpTableMinDensity: 0.5})
	if err != nil {
		t.Fatalf("BuildSwitchPlan failed: %v", err)
	}
	if plan.JumpTable == nil {
		t.Fatalf("expected dense switch to use a jump table")
	}
	if plan.CompareChain != nil {
		t.Fatalf("expected compare chain to be empty when jump table is chosen")
	}
	if len(plan.JumpTable.Entries) != 3 {
		t.Fatalf("expected 3 jump table entries, got %d", len(plan.JumpTable.Entries))
	}
	if plan.JumpTable.Entries[0] != "L1" || plan.JumpTable.Entries[2] != "L3" {
		t.Fatalf("unexpected jump table entries: %#v", plan.JumpTable.Entries)
	}
}

func TestBuildSwitchPlanFallsBackForSparseCases(t *testing.T) {
	key := mir.NewRegister("k", mir.VirtualReg, mir.NewScalarType("i32", 4))
	sw := &mir.SwitchInstr{
		Value:   key,
		Default: "Ldefault",
		Arms: []mir.SwitchArm{
			{Value: mir.NewImmediate(1, mir.NewScalarType("i32", 4)), Label: "L1"},
			{Value: mir.NewImmediate(10, mir.NewScalarType("i32", 4)), Label: "L10"},
		},
	}

	plan, err := BuildSwitchPlan(sw, ABI{JumpTableMinDensity: 0.5})
	if err != nil {
		t.Fatalf("BuildSwitchPlan failed: %v", err)
	}
	if plan.JumpTable != nil {
		t.Fatalf("expected sparse switch to avoid jump table")
	}
	if len(plan.CompareChain) != 2 {
		t.Fatalf("expected compare chain with 2 cases, got %d", len(plan.CompareChain))
	}
}

func TestBuildCallPlanAssignsRegistersAndStack(t *testing.T) {
	target := NewRISCV64Target()
	args := make([]mir.Operand, 0, 10)
	for i := 0; i < 10; i++ {
		args = append(args, mir.NewImmediate(i, mir.NewScalarType("i64", 8)))
	}
	call := &mir.CallInstr{
		Dst:    mir.NewRegister("res", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Callee: mir.NewSymbol("foo", mir.NewScalarType("i64", 8)),
		Args:   args,
	}

	plan, err := target.LowerCall(call)
	if err != nil {
		t.Fatalf("LowerCall failed: %v", err)
	}
	if len(plan.Args) != 10 {
		t.Fatalf("expected 10 arg locations, got %d", len(plan.Args))
	}
	if !plan.Args[0].InRegister || plan.Args[0].Register != "a0" {
		t.Fatalf("expected first arg in a0, got %#v", plan.Args[0])
	}
	if plan.Args[8].InRegister {
		t.Fatalf("expected ninth arg to spill to stack, got %#v", plan.Args[8])
	}
	if plan.StackBytes != 16 {
		t.Fatalf("expected 16 bytes of outgoing stack space, got %d", plan.StackBytes)
	}
	if plan.Result == nil || !plan.Result.InRegister || plan.Result.Register != "a0" {
		t.Fatalf("expected return value in a0, got %#v", plan.Result)
	}
}

func TestKnownTargetsShareCommonBase(t *testing.T) {
	if arm64Default.Name() != "arm64" {
		t.Fatalf("unexpected arm64 target name")
	}
	for name, factory := range KnownTargets {
		tgt := factory()
		if tgt == nil {
			t.Fatalf("factory for %s returned nil", name)
		}
		if got := tgt.Name(); got != name {
			t.Fatalf("factory %s returned target %q", name, got)
		}
	}
}
