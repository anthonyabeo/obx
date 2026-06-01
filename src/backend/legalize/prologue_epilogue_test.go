package legalize

import (
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// buildFn builds a minimal two-block function (entry → exit) and sets a
// FrameLayout so that EmitPrologueEpilogue will actually run.
func buildFn(name string, frameSize int, savedRegs []string) (*mir.Function, *mir.Block, *mir.Block) {
	fn := mir.NewFunction(name, mir.NewScalarType("i64", 8))
	fn.Frame = &mir.FrameLayout{
		TotalSize:  frameSize,
		SavedRegs:  savedRegs,
		SpillSlots: make(map[string]int),
	}

	entry := mir.NewBlock(0, "entry")
	exit := mir.NewBlock(1, "exit")

	// entry → exit via jump
	entry.SetTerminator(&mir.JumpInstr{Target: "exit"})
	// exit terminates with a ReturnInstr (no value — legalization already ran)
	exit.SetTerminator(&mir.ReturnInstr{})

	fn.AddBlock(entry)
	fn.AddBlock(exit)
	fn.SetEntry(entry)
	fn.SetExit(exit)
	return fn, entry, exit
}

func buildProg(fn *mir.Function) *mir.Program {
	prog := mir.NewProgram()
	mod := mir.NewModule("m")
	mod.AddFunction(fn)
	prog.AddModule(mod)
	return prog
}

func buildProgWithModules(mods ...*mir.Module) *mir.Program {
	prog := mir.NewProgram()
	for _, mod := range mods {
		prog.AddModule(mod)
	}
	return prog
}

// ── ARM64 tests ───────────────────────────────────────────────────────────────

func TestARM64PrologueEpilogue_BasicFrame(t *testing.T) {
	fn, entry, exit := buildFn("f", 16, []string{"x29", "x30"})
	prog := buildProg(fn)

	tgt, _ := target.Lookup(target.Arm64Name)
	if err := EmitPrologueEpilogue(prog, tgt); err != nil {
		t.Fatalf("EmitPrologueEpilogue: %v", err)
	}

	// Entry block must start with stp.pre then FP setup (add or mov)
	if len(entry.Instrs) < 2 {
		t.Fatalf("entry.Instrs len = %d, want >= 2", len(entry.Instrs))
	}
	mi0, ok := entry.Instrs[0].(*mir.MachineInstr)
	if !ok || mi0.Op != "stp.pre" {
		t.Errorf("entry.Instrs[0] = %v, want MachineInstr{Op:\"stp.pre\"}", entry.Instrs[0])
	}
	mi1, ok := entry.Instrs[1].(*mir.MachineInstr)
	if !ok || (mi1.Op != "mov" && mi1.Op != "add") {
		t.Errorf("entry.Instrs[1] = %v, want MachineInstr{Op:\"mov\"|\"add\"}", entry.Instrs[1])
	}

	// Exit block must start with ldp.post
	if len(exit.Instrs) < 1 {
		t.Fatalf("exit.Instrs len = %d, want >= 1", len(exit.Instrs))
	}
	me0, ok := exit.Instrs[0].(*mir.MachineInstr)
	if !ok || me0.Op != "ldp.post" {
		t.Errorf("exit.Instrs[0] = %v, want MachineInstr{Op:\"ldp.post\"}", exit.Instrs[0])
	}

	// ReturnInstr must have been replaced with a "ret.bare" MachineTerm
	mt, ok := exit.Term.(*mir.MachineTerm)
	if !ok || mt.Op != "ret.bare" {
		t.Errorf("exit.Term = %v, want MachineTerm{Op:\"ret.bare\"}", exit.Term)
	}
}

func TestARM64PrologueEpilogue_ExtraCalleeSaved(t *testing.T) {
	// x19, x20, x21 added alongside the mandatory x29/x30
	fn, entry, exit := buildFn("g", 48, []string{"x29", "x30", "x19", "x20", "x21"})
	prog := buildProg(fn)

	tgt, _ := target.Lookup(target.Arm64Name)
	if err := EmitPrologueEpilogue(prog, tgt); err != nil {
		t.Fatalf("EmitPrologueEpilogue: %v", err)
	}

	// Prologue: stp.pre, mov, stp(x19,x20), str(x21) = 4 instrs
	if len(entry.Instrs) != 4 {
		t.Fatalf("entry.Instrs len = %d, want 4; instrs = %v", len(entry.Instrs), entry.Instrs)
	}
	stp := entry.Instrs[2].(*mir.MachineInstr)
	if stp.Op != "stp" {
		t.Errorf("entry.Instrs[2].Op = %q, want \"stp\"", stp.Op)
	}
	str := entry.Instrs[3].(*mir.MachineInstr)
	if str.Op != "str" {
		t.Errorf("entry.Instrs[3].Op = %q, want \"str\"", str.Op)
	}

	// Epilogue: ldr(x21), ldp(x19,x20), ldp.post = 3 instrs
	if len(exit.Instrs) != 3 {
		t.Fatalf("exit.Instrs len = %d, want 3; instrs = %v", len(exit.Instrs), exit.Instrs)
	}
	ldr := exit.Instrs[0].(*mir.MachineInstr)
	if ldr.Op != "ldr" {
		t.Errorf("exit.Instrs[0].Op = %q, want \"ldr\"", ldr.Op)
	}
	ldpPair := exit.Instrs[1].(*mir.MachineInstr)
	if ldpPair.Op != "ldp" {
		t.Errorf("exit.Instrs[1].Op = %q, want \"ldp\"", ldpPair.Op)
	}
	ldpPost := exit.Instrs[2].(*mir.MachineInstr)
	if ldpPost.Op != "ldp.post" {
		t.Errorf("exit.Instrs[2].Op = %q, want \"ldp.post\"", ldpPost.Op)
	}
}

func TestARM64PrologueEpilogue_NoFrame_Skipped(t *testing.T) {
	fn := mir.NewFunction("leaf", nil)
	// fn.Frame == nil → pass must be a no-op
	entry := mir.NewBlock(0, "entry")
	exit := mir.NewBlock(1, "exit")
	exit.SetTerminator(&mir.ReturnInstr{})
	fn.AddBlock(entry)
	fn.AddBlock(exit)
	fn.SetEntry(entry)
	fn.SetExit(exit)

	prog := buildProg(fn)
	tgt, _ := target.Lookup(target.Arm64Name)
	if err := EmitPrologueEpilogue(prog, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(entry.Instrs) != 0 || len(exit.Instrs) != 0 {
		t.Errorf("expected no instructions inserted for a frame-less function")
	}
	if _, ok := exit.Term.(*mir.ReturnInstr); !ok {
		t.Errorf("exit.Term should still be ReturnInstr, got %T", exit.Term)
	}
}

func TestARM64PrologueEpilogue_ExitBlockEmptyInstrs(t *testing.T) {
	// Regression: epilogue insertion must work even when exit.Instrs is empty.
	fn, _, exit := buildFn("h", 16, []string{"x29", "x30"})
	exit.Instrs = nil // force empty

	prog := buildProg(fn)
	tgt, _ := target.Lookup(target.Arm64Name)
	if err := EmitPrologueEpilogue(prog, tgt); err != nil {
		t.Fatalf("EmitPrologueEpilogue: %v", err)
	}
	if len(exit.Instrs) == 0 {
		t.Error("epilogue was not inserted into empty exit block")
	}
}

func TestARM64PrologueEpilogue_EntryInitOnlyZerosExitCode(t *testing.T) {
	entryFn, _, entryExit := buildFn("__init_Main", 16, []string{"x29", "x30"})
	otherFn, _, otherExit := buildFn("__init_IO", 16, []string{"x29", "x30"})

	entryMod := mir.NewModule("Main")
	entryMod.IsEntry = true
	entryMod.AddFunction(entryFn)
	otherMod := mir.NewModule("IO")
	otherMod.AddFunction(otherFn)

	prog := buildProgWithModules(entryMod, otherMod)
	tgt, _ := target.Lookup(target.Arm64Name)
	if err := EmitPrologueEpilogue(prog, tgt); err != nil {
		t.Fatalf("EmitPrologueEpilogue: %v", err)
	}

	foundEntryMove := false
	for _, ins := range entryExit.Instrs {
		if mi, ok := ins.(*mir.MachineInstr); ok && mi.Op == "mov" {
			foundEntryMove = true
		}
	}
	if !foundEntryMove {
		t.Fatalf("expected entry init to zero x0 before return")
	}

	for _, ins := range otherExit.Instrs {
		if mi, ok := ins.(*mir.MachineInstr); ok && mi.Op == "mov" {
			t.Fatalf("non-entry init should not zero x0, got %v", mi)
		}
	}
}

// ── RISC-V tests ──────────────────────────────────────────────────────────────

func TestRISCVPrologueEpilogue_BasicFrame(t *testing.T) {
	fn, entry, exit := buildFn("f", 32, []string{"ra", "s0"})
	prog := buildProg(fn)

	tgt, _ := target.Lookup(target.RV64IMAFDName)
	if err := EmitPrologueEpilogue(prog, tgt); err != nil {
		t.Fatalf("EmitPrologueEpilogue: %v", err)
	}

	// Prologue: addi(alloc), sd(ra), sd(s0), addi(fp) = 4 instrs
	if len(entry.Instrs) != 4 {
		t.Fatalf("entry.Instrs len = %d, want 4; instrs = %v", len(entry.Instrs), entry.Instrs)
	}
	checkOp(t, entry.Instrs[0], "addi", "frame alloc")
	checkOp(t, entry.Instrs[1], "sd", "save ra")
	checkOp(t, entry.Instrs[2], "sd", "save s0")
	checkOp(t, entry.Instrs[3], "addi", "set fp")

	// Verify offsets: ra at (frameSize-8) = 24, s0 at (frameSize-16) = 16
	checkMemOffset(t, entry.Instrs[1].(*mir.MachineInstr), 24, "ra save offset")
	checkMemOffset(t, entry.Instrs[2].(*mir.MachineInstr), 16, "s0 save offset")

	// Epilogue: ld(s0), ld(ra), addi(dealloc) = 3 instrs
	if len(exit.Instrs) != 3 {
		t.Fatalf("exit.Instrs len = %d, want 3; instrs = %v", len(exit.Instrs), exit.Instrs)
	}
	checkOp(t, exit.Instrs[0], "ld", "restore s0")
	checkOp(t, exit.Instrs[1], "ld", "restore ra")
	checkOp(t, exit.Instrs[2], "addi", "frame dealloc")

	// Verify symmetry: same offsets used for restores
	checkMemOffset(t, exit.Instrs[0].(*mir.MachineInstr), 16, "s0 restore offset")
	checkMemOffset(t, exit.Instrs[1].(*mir.MachineInstr), 24, "ra restore offset")

	// Terminator replaced with ret.bare
	mt, ok := exit.Term.(*mir.MachineTerm)
	if !ok || mt.Op != "ret.bare" {
		t.Errorf("exit.Term = %T %v, want MachineTerm{Op:\"ret.bare\"}", exit.Term, exit.Term)
	}
}

func TestRISCVPrologueEpilogue_ExtraCalleeSaved(t *testing.T) {
	// s1 and s2 added alongside ra and s0
	fn, entry, exit := buildFn("g", 48, []string{"ra", "s0", "s1", "s2"})
	prog := buildProg(fn)

	tgt, _ := target.Lookup(target.RV64IMAFDName)
	if err := EmitPrologueEpilogue(prog, tgt); err != nil {
		t.Fatalf("EmitPrologueEpilogue: %v", err)
	}

	// Prologue: addi, sd(ra), sd(s0), sd(s1), sd(s2), addi(fp) = 6 instrs
	if len(entry.Instrs) != 6 {
		t.Fatalf("entry.Instrs len = %d, want 6", len(entry.Instrs))
	}
	// s1 at frameSize-24 = 24, s2 at frameSize-32 = 16
	checkMemOffset(t, entry.Instrs[3].(*mir.MachineInstr), 24, "s1 save offset")
	checkMemOffset(t, entry.Instrs[4].(*mir.MachineInstr), 16, "s2 save offset")

	// Epilogue: ld(s2), ld(s1), ld(s0), ld(ra), addi = 5 instrs  (LIFO)
	if len(exit.Instrs) != 5 {
		t.Fatalf("exit.Instrs len = %d, want 5", len(exit.Instrs))
	}
	// Restored in reverse save order: s2 first (offset 16), then s1 (offset 24)
	checkMemOffset(t, exit.Instrs[0].(*mir.MachineInstr), 16, "s2 restore offset")
	checkMemOffset(t, exit.Instrs[1].(*mir.MachineInstr), 24, "s1 restore offset")
}

// ── Helpers ──────────────────────────────────────────────────────────────────

func checkOp(t *testing.T, instr mir.Instr, want, label string) {
	t.Helper()
	mi, ok := instr.(*mir.MachineInstr)
	if !ok {
		t.Errorf("%s: got %T, want *MachineInstr", label, instr)
		return
	}
	if mi.Op != want {
		t.Errorf("%s: Op = %q, want %q", label, mi.Op, want)
	}
}

// checkMemOffset verifies that the last source operand of a store/load
// MachineInstr is a Memory with the expected integer offset.
func checkMemOffset(t *testing.T, mi *mir.MachineInstr, wantOffset int, label string) {
	t.Helper()
	// For stores (sd): operands are [value, Memory]; for loads (ld): srcs=[Memory]
	var memOp mir.Operand
	if len(mi.Srcs) >= 2 {
		memOp = mi.Srcs[len(mi.Srcs)-1]
	} else if len(mi.Srcs) == 1 {
		memOp = mi.Srcs[0]
	} else {
		t.Errorf("%s: no source operands", label)
		return
	}
	mem, ok := memOp.(*mir.Memory)
	if !ok {
		t.Errorf("%s: expected *Memory operand, got %T", label, memOp)
		return
	}
	imm, ok := mem.Offset.(*mir.Immediate)
	if !ok {
		t.Errorf("%s: memory offset is %T, want *Immediate", label, mem.Offset)
		return
	}
	gotOffset, ok := imm.Value.(int)
	if !ok {
		t.Errorf("%s: immediate value is %T %v, want int", label, imm.Value, imm.Value)
		return
	}
	if gotOffset != wantOffset {
		t.Errorf("%s: offset = %d, want %d", label, gotOffset, wantOffset)
	}
}
