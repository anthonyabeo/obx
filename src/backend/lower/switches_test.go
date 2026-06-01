package lower_test

import (
	"testing"

	"github.com/anthonyabeo/obx/src/backend/lower"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// switchFn builds a minimal function whose entry block has the given SwitchInstr
// as its terminator.
func switchFn(sw *mir.SwitchInstr) *mir.Function {
	fn := mir.NewFunction("f", nil)
	b := mir.NewBlock(0, "entry")
	b.Term = sw
	fn.AddBlock(b)
	return fn
}

func switchArm(v int, label string) mir.SwitchArm {
	return mir.SwitchArm{
		Value: mir.NewImmediate(v, mir.NewScalarType("i32", 4)),
		Label: label,
	}
}

// TestLowerSwitchesNoArms: switch with no arms → single JumpInstr{default}.
func TestLowerSwitchesNoArms(t *testing.T) {
	tgt := target.NewRISCV64Target()
	key := mir.NewRegister("k", mir.VirtualReg, mir.NewScalarType("i32", 4))
	fn := switchFn(&mir.SwitchInstr{Value: key, Default: "Ldefault", Arms: nil})

	if err := lower.LowerSwitchesInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	entry := fn.Blocks[0]
	jmp, ok := entry.Term.(*mir.JumpInstr)
	if !ok {
		t.Fatalf("expected *mir.JumpInstr, got %T", entry.Term)
	}
	if jmp.Target != "Ldefault" {
		t.Fatalf("jump target = %q, want %q", jmp.Target, "Ldefault")
	}
	if len(entry.Instrs) != 0 {
		t.Fatalf("expected no instrs in entry, got %d", len(entry.Instrs))
	}
}

// TestLowerSwitchesSingleArm: one case → one cmp.eq + CondBrInstr.
func TestLowerSwitchesSingleArm(t *testing.T) {
	tgt := target.NewRISCV64Target()
	key := mir.NewRegister("k", mir.VirtualReg, mir.NewScalarType("i32", 4))
	fn := switchFn(&mir.SwitchInstr{
		Value:   key,
		Default: "Ldefault",
		Arms:    []mir.SwitchArm{switchArm(1, "L1")},
	})

	if err := lower.LowerSwitchesInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	entry := fn.Blocks[0]
	if len(entry.Instrs) != 1 {
		t.Fatalf("expected 1 instr, got %d", len(entry.Instrs))
	}
	cmp, ok := entry.Instrs[0].(*mir.CompareInstr)
	if !ok {
		t.Fatalf("expected *mir.CompareInstr, got %T", entry.Instrs[0])
	}
	if cmp.Pred != "eq" {
		t.Fatalf("pred = %q, want eq", cmp.Pred)
	}

	br, ok := entry.Term.(*mir.CondBrInstr)
	if !ok {
		t.Fatalf("expected *mir.CondBrInstr terminator, got %T", entry.Term)
	}
	if br.TrueLabel != "L1" {
		t.Fatalf("true label = %q, want L1", br.TrueLabel)
	}
	if br.FalseLabel != "Ldefault" {
		t.Fatalf("false label = %q, want Ldefault", br.FalseLabel)
	}
	// No new blocks expected for a single case.
	if len(fn.Blocks) != 1 {
		t.Fatalf("expected 1 block, got %d", len(fn.Blocks))
	}
}

// TestLowerSwitchesThreeArmsCompareChain: three sparse cases → 3 blocks with
// correctly chained CondBrInstr terminators.
func TestLowerSwitchesThreeArmsCompareChain(t *testing.T) {
	tgt := target.NewRISCV64Target()
	key := mir.NewRegister("k", mir.VirtualReg, mir.NewScalarType("i32", 4))
	fn := switchFn(&mir.SwitchInstr{
		Value:   key,
		Default: "Ldefault",
		Arms: []mir.SwitchArm{
			switchArm(1, "L1"),
			switchArm(100, "L100"), // sparse: values 1 and 100 → density < 0.5
			switchArm(200, "L200"),
		},
	})

	if err := lower.LowerSwitchesInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// 3 cases → original block + 2 new check blocks = 3 total.
	if len(fn.Blocks) != 3 {
		t.Fatalf("expected 3 blocks, got %d", len(fn.Blocks))
	}

	// Block 0: entry — has 1 CompareInstr and CondBrInstr{L1, check_1}
	b0 := fn.Blocks[0]
	if len(b0.Instrs) != 1 {
		t.Fatalf("b0: expected 1 instr, got %d", len(b0.Instrs))
	}
	if _, ok := b0.Instrs[0].(*mir.CompareInstr); !ok {
		t.Fatalf("b0: expected CompareInstr, got %T", b0.Instrs[0])
	}
	br0, ok := b0.Term.(*mir.CondBrInstr)
	if !ok {
		t.Fatalf("b0: expected CondBrInstr, got %T", b0.Term)
	}
	if br0.TrueLabel != "L1" {
		t.Fatalf("b0: true label = %q, want L1", br0.TrueLabel)
	}
	// The FalseLabel must be the first check block.
	check1Label := br0.FalseLabel

	// Block 1 (check_1): has 1 CompareInstr and CondBrInstr{L100, check_2}
	var b1, b2 *mir.Block
	for _, b := range fn.Blocks[1:] {
		if b.Label == check1Label {
			b1 = b
		}
	}
	if b1 == nil {
		t.Fatalf("check block %q not found", check1Label)
	}
	if len(b1.Instrs) != 1 {
		t.Fatalf("b1: expected 1 instr, got %d", len(b1.Instrs))
	}
	br1, ok := b1.Term.(*mir.CondBrInstr)
	if !ok {
		t.Fatalf("b1: expected CondBrInstr, got %T", b1.Term)
	}
	if br1.TrueLabel != "L100" {
		t.Fatalf("b1: true label = %q, want L100", br1.TrueLabel)
	}
	check2Label := br1.FalseLabel

	// Block 2 (check_2): has 1 CompareInstr and CondBrInstr{L200, default}
	for _, b := range fn.Blocks[1:] {
		if b.Label == check2Label {
			b2 = b
		}
	}
	if b2 == nil {
		t.Fatalf("check block %q not found", check2Label)
	}
	if len(b2.Instrs) != 1 {
		t.Fatalf("b2: expected 1 instr, got %d", len(b2.Instrs))
	}
	br2, ok := b2.Term.(*mir.CondBrInstr)
	if !ok {
		t.Fatalf("b2: expected CondBrInstr, got %T", b2.Term)
	}
	if br2.TrueLabel != "L200" {
		t.Fatalf("b2: true label = %q, want L200", br2.TrueLabel)
	}
	if br2.FalseLabel != "Ldefault" {
		t.Fatalf("b2: false label = %q, want Ldefault", br2.FalseLabel)
	}
}

// TestLowerSwitchesDenseJumpTable: dense cases → MachineTerm{Op:"switch_table"}.
func TestLowerSwitchesDenseJumpTable(t *testing.T) {
	tgt := target.NewRISCV64Target()
	key := mir.NewRegister("k", mir.VirtualReg, mir.NewScalarType("i32", 4))
	// Values 1, 2, 3 — density = 3/3 = 1.0 ≥ 0.5 → jump table
	fn := switchFn(&mir.SwitchInstr{
		Value:   key,
		Default: "Ldefault",
		Arms: []mir.SwitchArm{
			switchArm(1, "L1"),
			switchArm(2, "L2"),
			switchArm(3, "L3"),
		},
	})

	if err := lower.LowerSwitchesInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Only the entry block — no new blocks for jump table.
	if len(fn.Blocks) != 1 {
		t.Fatalf("expected 1 block, got %d", len(fn.Blocks))
	}

	mt, ok := fn.Blocks[0].Term.(*mir.MachineTerm)
	if !ok {
		t.Fatalf("expected *mir.MachineTerm, got %T", fn.Blocks[0].Term)
	}
	if mt.Op != "switch_table" {
		t.Fatalf("op = %q, want switch_table", mt.Op)
	}
	if len(mt.Srcs) != 1 {
		t.Fatalf("srcs len = %d, want 1 (the key)", len(mt.Srcs))
	}
	// Targets: [L1, L2, L3, Ldefault]
	if len(mt.Targets) != 4 {
		t.Fatalf("targets len = %d, want 4 (3 entries + default)", len(mt.Targets))
	}
	if mt.Targets[3] != "Ldefault" {
		t.Fatalf("last target = %q, want Ldefault", mt.Targets[3])
	}
}

// TestLowerSwitchesNonSwitchTermPassThrough: blocks without SwitchInstr are
// unmodified.
func TestLowerSwitchesNonSwitchTermPassThrough(t *testing.T) {
	tgt := target.NewRISCV64Target()
	fn := mir.NewFunction("f", nil)
	b := mir.NewBlock(0, "entry")
	b.Term = &mir.JumpInstr{Target: "next"}
	fn.AddBlock(b)

	if err := lower.LowerSwitchesInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if jmp, ok := fn.Blocks[0].Term.(*mir.JumpInstr); !ok || jmp.Target != "next" {
		t.Fatalf("non-switch terminator was modified: %T", fn.Blocks[0].Term)
	}
}

// TestLowerSwitchesNilFunctionError: nil function → error.
func TestLowerSwitchesNilFunctionError(t *testing.T) {
	if err := lower.LowerSwitchesInFunction(nil, target.NewRISCV64Target()); err == nil {
		t.Fatal("expected error for nil function")
	}
}

// TestLowerSwitchesNilTargetError: nil target → error.
func TestLowerSwitchesNilTargetError(t *testing.T) {
	fn := mir.NewFunction("f", nil)
	if err := lower.LowerSwitchesInFunction(fn, nil); err == nil {
		t.Fatal("expected error for nil target")
	}
}

// TestLowerSwitchesNilProgram: nil program → empty program (no error).
func TestLowerSwitchesNilProgram(t *testing.T) {
	prog, err := lower.LowerSwitchesInProgram(nil, target.NewRISCV64Target())
	if err != nil {
		t.Fatalf("expected success for nil program, got: %v", err)
	}
	if prog == nil {
		t.Fatal("expected empty program, got nil")
	}
}

// TestLowerSwitchesProgramNilTarget: nil target → error.
func TestLowerSwitchesProgramNilTarget(t *testing.T) {
	if _, err := lower.LowerSwitchesInProgram(mir.NewProgram(), nil); err == nil {
		t.Fatal("expected error for nil target")
	}
}

// TestLowerSwitchesProgramMultipleFunctions: switch lowering propagates through
// all functions in all modules.
func TestLowerSwitchesProgramMultipleFunctions(t *testing.T) {
	tgt := target.NewRISCV64Target()

	key := mir.NewRegister("k", mir.VirtualReg, mir.NewScalarType("i32", 4))
	sw := &mir.SwitchInstr{
		Value:   key,
		Default: "Ldefault",
		Arms:    []mir.SwitchArm{switchArm(5, "L5")},
	}

	fn := mir.NewFunction("g", nil)
	b := mir.NewBlock(0, "entry")
	b.Term = sw
	fn.AddBlock(b)

	mod := mir.NewModule("M")
	mod.AddFunction(fn)
	prog := mir.NewProgram()
	prog.AddModule(mod)

	out, err := lower.LowerSwitchesInProgram(prog, tgt)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	outFn := out.Modules[0].Functions[0]
	if _, ok := outFn.Blocks[0].Term.(*mir.SwitchInstr); ok {
		t.Fatal("SwitchInstr still present after LowerSwitchesInProgram")
	}
}
