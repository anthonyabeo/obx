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
	// dstIsPendingSource produces [tmp←y, y←x, x←tmp] — both y and x end up
	// with the expected swapped values regardless of the internal ordering.
	if got := moves[1].Dst.Name; got != "y" {
		t.Fatalf("expected second move to target y, got %s", got)
	}
	if got := moves[2].Dst.Name; got != "x" {
		t.Fatalf("expected third move to target x, got %s", got)
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

func TestRegistryLookupAndAvailable(t *testing.T) {
	available := Available()
	if len(available) != 2 {
		t.Fatalf("expected 2 registered targets, got %d", len(available))
	}
	if available[0] != Arm64Name || available[1] != RV64IMAFDName {
		t.Fatalf("unexpected available targets: %#v", available)
	}

	for _, name := range available {
		tgt, err := Lookup(name)
		if err != nil {
			t.Fatalf("Lookup(%s) failed: %v", name, err)
		}
		if tgt == nil {
			t.Fatalf("Lookup(%s) returned nil", name)
		}
		if got := tgt.Name(); got != name {
			t.Fatalf("Lookup(%s) returned target %q", name, got)
		}
	}

	for _, alias := range []string{Arm64AppleMacosName, AArch64AppleDarwinName} {
		tgt, err := Lookup(alias)
		if err != nil {
			t.Fatalf("Lookup(%s) failed: %v", alias, err)
		}
		if tgt == nil {
			t.Fatalf("Lookup(%s) returned nil", alias)
		}
		if got := tgt.Name(); got != Arm64Name {
			t.Fatalf("Lookup(%s) returned target %q, want %q", alias, got, Arm64Name)
		}
	}
}

func TestEmitARM64MachineInstrFormatsCSetConditionCode(t *testing.T) {
	got := emitARM64MachineInstr(&mir.MachineInstr{
		Op:   "cset",
		Dsts: []*mir.Register{mir.NewRegister("x1", mir.PhysicalReg, mir.NewScalarType("i1", 1))},
		Srcs: []mir.Operand{&mir.Label{Name: "eq"}},
	})
	if got != "cset x1, eq" {
		t.Fatalf("emitARM64MachineInstr(cset) = %q, want %q", got, "cset x1, eq")
	}
}

func TestEmitARM64HaltWithImmediateCode(t *testing.T) {
	got := emitARM64Terminator(&mir.HaltInstr{
		Code: mir.NewImmediate(1, mir.NewScalarType("i32", 4)),
	})
	expected := "mov w0, #1\n\tbl _exit"
	if got != expected {
		t.Fatalf("emitARM64Terminator(HaltInstr with immediate) =\n%q\nwant\n%q", got, expected)
	}
}

func TestEmitARM64HaltWithRegisterCode(t *testing.T) {
	got := emitARM64Terminator(&mir.HaltInstr{
		Code: mir.NewRegister("x0", mir.PhysicalReg, mir.NewScalarType("i32", 4)),
	})
	expected := "bl _exit"
	if got != expected {
		t.Fatalf("emitARM64Terminator(HaltInstr with register x0) =\n%q\nwant\n%q", got, expected)
	}
}

func TestEmitARM64HaltWithNilCode(t *testing.T) {
	got := emitARM64Terminator(&mir.HaltInstr{
		Code: nil,
	})
	expected := "mov w0, #1\n\tbl _exit"
	if got != expected {
		t.Fatalf("emitARM64Terminator(HaltInstr with nil code) =\n%q\nwant\n%q", got, expected)
	}
}

// TestEmitARM64CondBranchBothPaths verifies that the MachineTerm "bne" handler
// emits both the conditional branch to the true label and the unconditional
// branch to the false label.  The "cmp" prefix is emitted separately by the
// descriptor rule (BRnz) as a termPrefix instruction; the terminal handler
// itself should NOT re-emit it.
func TestEmitARM64CondBranchBothPaths(t *testing.T) {
	i1 := mir.NewScalarType("i1", 1)
	cond := mir.NewRegister("x8", mir.PhysicalReg, i1)
	trueLabel := &mir.Label{Name: "if_then"}
	falseLabel := &mir.Label{Name: "if_else"}

	term := mir.NewMachineTerm("bne",
		[]mir.Operand{cond, trueLabel, falseLabel},
		[]string{"if_then", "if_else"},
	)

	got := emitARM64Terminator(term)
	want := "b.ne Lif_then\n\tb Lif_else"
	if got != want {
		t.Fatalf("emitARM64Terminator(bne cond, Ltrue, Lfalse) =\n%q\nwant\n%q", got, want)
	}
}

// TestEmitARM64CondBranchCondBrInstr verifies the CondBrInstr fallback (used
// when the descriptor rule does not match) still emits the full sequence.
func TestEmitARM64CondBranchCondBrInstr(t *testing.T) {
	i1 := mir.NewScalarType("i1", 1)
	cond := mir.NewRegister("x8", mir.PhysicalReg, i1)

	term := &mir.CondBrInstr{Cond: cond, TrueLabel: "then_0", FalseLabel: "else_1"}
	got := emitARM64Terminator(term)
	want := "cmp x8, #0\n\tb.ne Lthen_0\n\tb Lelse_1"
	if got != want {
		t.Fatalf("emitARM64Terminator(CondBrInstr) =\n%q\nwant\n%q", got, want)
	}
}
