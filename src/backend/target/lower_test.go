package target

import (
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

func TestSupportsIntegerScalar(t *testing.T) {
	if !SupportsIntegerScalar(mir.NewScalarType("i32", 4)) {
		t.Fatalf("expected i32 scalar to be supported")
	}
	// Pointer types are passed in integer registers on all supported targets.
	if !SupportsIntegerScalar(mir.NewPointerType(mir.NewScalarType("i32", 4), 8)) {
		t.Fatalf("expected pointer type to be supported (pointers go in integer registers)")
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

func TestBuildCallPlanAssignsFloatRegisters(t *testing.T) {
	tgt, err := Lookup(Arm64Name)
	if err != nil {
		t.Fatalf("Lookup(%s) failed: %v", Arm64Name, err)
	}
	call := &mir.CallInstr{
		Dst:    mir.NewRegister("resf", mir.VirtualReg, mir.NewScalarType("f64", 8)),
		Callee: mir.NewSymbol("foo", mir.NewScalarType("f64", 8)),
		Args: []mir.Operand{
			mir.NewImmediate(3.14, mir.NewScalarType("f64", 8)),
			mir.NewImmediate(float32(2.5), mir.NewScalarType("f32", 4)),
		},
	}

	plan, err := tgt.LowerCall(call)
	if err != nil {
		t.Fatalf("LowerCall failed: %v", err)
	}
	if len(plan.Args) != 2 {
		t.Fatalf("expected 2 arg locations, got %d", len(plan.Args))
	}
	if !plan.Args[0].InRegister || plan.Args[0].Register != "d0" {
		t.Fatalf("expected first float arg in d0, got %#v", plan.Args[0])
	}
	if !plan.Args[1].InRegister || plan.Args[1].Register != "d1" {
		t.Fatalf("expected second float arg in d1, got %#v", plan.Args[1])
	}
	if plan.Result == nil || !plan.Result.InRegister || plan.Result.Register != "d0" {
		t.Fatalf("expected float return value in d0, got %#v", plan.Result)
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

func TestEmitARM64FunctionRenamesEntryInitOnly(t *testing.T) {
	fn := mir.NewFunction("__init_Main", nil)

	entryMod := mir.NewModule("Main")
	entryMod.IsEntry = true
	gotEntry := emitARM64Function(entryMod, fn)
	if !strings.Contains(gotEntry, "_main:") {
		t.Fatalf("entry module init should emit _main, got:\n%s", gotEntry)
	}

	otherMod := mir.NewModule("IO")
	gotOther := emitARM64Function(otherMod, fn)
	if strings.Contains(gotOther, "_main:") {
		t.Fatalf("non-entry module init should not emit _main, got:\n%s", gotOther)
	}
	if !strings.Contains(gotOther, "___init_Main:") {
		t.Fatalf("non-entry module init should keep its symbol, got:\n%s", gotOther)
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

func TestEmitARM64MachineInstrTruncMasksToDstWidth(t *testing.T) {
	got := emitARM64MachineInstr(&mir.MachineInstr{
		Op:   "trunc",
		Dsts: []*mir.Register{mir.NewRegister("x10", mir.PhysicalReg, mir.NewScalarType("i8", 1))},
		Srcs: []mir.Operand{mir.NewRegister("x11", mir.PhysicalReg, mir.NewScalarType("i64", 8))},
	})
	want := "and x10, x11, #255"
	if got != want {
		t.Fatalf("emitARM64MachineInstr(trunc i64->i8) = %q, want %q", got, want)
	}
}

func TestEmitARM64MachineInstrZExtMasksToSrcWidth(t *testing.T) {
	got := emitARM64MachineInstr(&mir.MachineInstr{
		Op:   "zext",
		Dsts: []*mir.Register{mir.NewRegister("x12", mir.PhysicalReg, mir.NewScalarType("i64", 8))},
		Srcs: []mir.Operand{mir.NewRegister("x13", mir.PhysicalReg, mir.NewScalarType("i16", 2))},
	})
	want := "and x12, x13, #65535"
	if got != want {
		t.Fatalf("emitARM64MachineInstr(zext i16->i64) = %q, want %q", got, want)
	}
}

func TestEmitARM64MachineInstrSExtUsesSignExtendOpcode(t *testing.T) {
	got := emitARM64MachineInstr(&mir.MachineInstr{
		Op:   "sext",
		Dsts: []*mir.Register{mir.NewRegister("x14", mir.PhysicalReg, mir.NewScalarType("i64", 8))},
		Srcs: []mir.Operand{mir.NewRegister("x15", mir.PhysicalReg, mir.NewScalarType("i8", 1))},
	})
	want := "sxtb x14, x15"
	if got != want {
		t.Fatalf("emitARM64MachineInstr(sext i8->i64) = %q, want %q", got, want)
	}
}

func TestEmitARM64MachineInstrSIToFP(t *testing.T) {
	got := emitARM64MachineInstr(&mir.MachineInstr{
		Op:   "sitofp",
		Dsts: []*mir.Register{mir.NewRegister("d0", mir.PhysicalReg, mir.NewScalarType("f64", 8))},
		Srcs: []mir.Operand{mir.NewRegister("x0", mir.PhysicalReg, mir.NewScalarType("i64", 8))},
	})
	want := "scvtf d0, x0"
	if got != want {
		t.Fatalf("emitARM64MachineInstr(sitofp i64->f64) = %q, want %q", got, want)
	}
}

func TestEmitARM64MachineInstrFPToSI(t *testing.T) {
	got := emitARM64MachineInstr(&mir.MachineInstr{
		Op:   "fptosi",
		Dsts: []*mir.Register{mir.NewRegister("x1", mir.PhysicalReg, mir.NewScalarType("i64", 8))},
		Srcs: []mir.Operand{mir.NewRegister("d1", mir.PhysicalReg, mir.NewScalarType("f64", 8))},
	})
	want := "fcvtzs x1, d1"
	if got != want {
		t.Fatalf("emitARM64MachineInstr(fptosi f64->i64) = %q, want %q", got, want)
	}
}

func TestEmitARM64MachineInstrFPCvt(t *testing.T) {
	got := emitARM64MachineInstr(&mir.MachineInstr{
		Op:   "fpext",
		Dsts: []*mir.Register{mir.NewRegister("d2", mir.PhysicalReg, mir.NewScalarType("f64", 8))},
		Srcs: []mir.Operand{mir.NewRegister("s2", mir.PhysicalReg, mir.NewScalarType("f32", 4))},
	})
	want := "fcvt d2, s2"
	if got != want {
		t.Fatalf("emitARM64MachineInstr(fpext f32->f64) = %q, want %q", got, want)
	}
}

func TestEmitARM64LoadStoreUsesWidthSpecificMnemonics(t *testing.T) {
	byteTy := mir.NewScalarType("u8", 1)
	wordTy := mir.NewScalarType("i32", 4)

	ldByte := emitARM64Instr(&mir.LoadInstr{
		Dst:  mir.NewRegister("x10", mir.PhysicalReg, byteTy),
		Addr: mir.NewMemory(mir.NewRegister("x1", mir.PhysicalReg, mir.NewPointerType(byteTy, 8)), nil, nil),
	})
	if ldByte != "ldrb w10, [x1]" {
		t.Fatalf("byte load = %q, want %q", ldByte, "ldrb w10, [x1]")
	}

	stByte := emitARM64Instr(&mir.StoreInstr{
		Value: mir.NewRegister("x11", mir.PhysicalReg, byteTy),
		Addr:  mir.NewMemory(mir.NewRegister("x2", mir.PhysicalReg, mir.NewPointerType(byteTy, 8)), nil, nil),
	})
	if stByte != "strb w11, [x2]" {
		t.Fatalf("byte store = %q, want %q", stByte, "strb w11, [x2]")
	}

	ldWord := emitARM64Instr(&mir.LoadInstr{
		Dst:  mir.NewRegister("x3", mir.PhysicalReg, wordTy),
		Addr: mir.NewMemory(mir.NewRegister("x4", mir.PhysicalReg, mir.NewPointerType(wordTy, 8)), nil, nil),
	})
	if ldWord != "ldr w3, [x4]" {
		t.Fatalf("word load = %q, want %q", ldWord, "ldr w3, [x4]")
	}

	stWord := emitARM64Instr(&mir.StoreInstr{
		Value: mir.NewRegister("x5", mir.PhysicalReg, wordTy),
		Addr:  mir.NewMemory(mir.NewRegister("x6", mir.PhysicalReg, mir.NewPointerType(wordTy, 8)), nil, nil),
	})
	if stWord != "str w5, [x6]" {
		t.Fatalf("word store = %q, want %q", stWord, "str w5, [x6]")
	}
}

func TestEmitARM64LoadStoreUsesFloatRegsForFloatTypes(t *testing.T) {
	f64 := mir.NewScalarType("f64", 8)
	ld := emitARM64Instr(&mir.LoadInstr{
		Dst:  mir.NewRegister("x0", mir.PhysicalReg, f64),
		Addr: mir.NewMemory(mir.NewRegister("x1", mir.PhysicalReg, mir.NewPointerType(f64, 8)), nil, nil),
	})
	if ld != "ldr d0, [x1]" {
		t.Fatalf("float load = %q, want %q", ld, "ldr d0, [x1]")
	}

	st := emitARM64Instr(&mir.StoreInstr{
		Value: mir.NewRegister("x2", mir.PhysicalReg, f64),
		Addr:  mir.NewMemory(mir.NewRegister("x3", mir.PhysicalReg, mir.NewPointerType(f64, 8)), nil, nil),
	})
	if st != "str d2, [x3]" {
		t.Fatalf("float store = %q, want %q", st, "str d2, [x3]")
	}
}

func TestEmitARM64ModuleEmitsStringConstants(t *testing.T) {
	mod := mir.NewModule("IO")
	mod.AddConst(mir.NewConstDecl(
		"_Lstr_deadbeef_0",
		mir.NewArrayType(mir.NewScalarType("i8", 1), 5),
		mir.NewImmediate("%d", mir.NewArrayType(mir.NewScalarType("i8", 1), 3)),
		mir.InternalLinkage,
	))

	asm := emitARM64Module(mod)
	if !strings.Contains(asm, "\t.section __TEXT,__cstring,cstring_literals") {
		t.Fatalf("missing cstring section in emitted module:\n%s", asm)
	}
	if !strings.Contains(asm, "__Lstr_deadbeef_0:") {
		t.Fatalf("missing string symbol label in emitted module:\n%s", asm)
	}
	if !strings.Contains(asm, "\t.asciz \"%d\"") {
		t.Fatalf("missing asciz directive for string const in emitted module:\n%s", asm)
	}
}

func TestEmitARM64ModuleExportsNonExternGlobals(t *testing.T) {
	mod := mir.NewModule("Stdio")
	ptrTy := mir.NewPointerType(mir.NewScalarType("void", 0), 8)

	stdin := mir.NewGlobalDecl("Stdio$stdin", ptrTy, mir.ExternalLinkage, nil)
	mod.AddGlobal(stdin)

	externOnly := mir.NewGlobalDecl("Other$stdin", ptrTy, mir.ExternalLinkage, nil)
	externOnly.IsExternRef = true
	mod.AddGlobal(externOnly)

	asm := emitARM64Module(mod)
	if !strings.Contains(asm, "\t.globl _Stdio$stdin") {
		t.Fatalf("missing global export for Stdio global:\n%s", asm)
	}
	if !strings.Contains(asm, "\t.extern _Other$stdin") {
		t.Fatalf("missing extern decl for external-ref global:\n%s", asm)
	}
}

func TestEmitARM64ModuleAddsInitStubWhenMissing(t *testing.T) {
	mod := mir.NewModule("LibM")
	asm := emitARM64Module(mod)
	if !strings.Contains(asm, ".globl ___init_LibM") {
		t.Fatalf("missing __init_ stub export in emitted module:\n%s", asm)
	}
	if !strings.Contains(asm, "___init_LibM:\n\tret") {
		t.Fatalf("missing __init_ stub body in emitted module:\n%s", asm)
	}
}

func TestEmitARM64MoveFromSymbolUsesAddressMaterialization(t *testing.T) {
	got := emitARM64Move("x1", "__Lstr_abc")
	if !strings.Contains(got, "adrp x1, __Lstr_abc@PAGE") {
		t.Fatalf("expected adrp for symbol move, got %q", got)
	}
	if !strings.Contains(got, "add x1, x1, __Lstr_abc@PAGEOFF") {
		t.Fatalf("expected add pageoff for symbol move, got %q", got)
	}
}

func TestEmitARM64MoveFloatImmediateToXRegUsesBitMaterialization(t *testing.T) {
	got := emitARM64Move("x0", "#3.140000104904175")
	if strings.Contains(got, "#3.140000104904175") {
		t.Fatalf("expected float immediate to be bit-materialized, got %q", got)
	}
	if !strings.Contains(got, "mov") {
		t.Fatalf("expected move sequence, got %q", got)
	}
}

func TestEmitARM64MoveFloatImmediateToFloatRegUsesIntegerSeed(t *testing.T) {
	got := emitARM64Move("d0", "#3.14")
	if !strings.Contains(got, "fmov d0, x0") {
		t.Fatalf("expected float move via integer seed register, got %q", got)
	}
	if strings.Contains(got, "#3.14") {
		t.Fatalf("expected literal to be encoded as bits, got %q", got)
	}
}

func TestFormatARM64OperandMapsTempStyleRegisterName(t *testing.T) {
	got := formatARM64Operand(mir.NewRegister("t13", mir.PhysicalReg, mir.NewScalarType("i64", 8)))
	if got != "x13" {
		t.Fatalf("formatARM64Operand(t13) = %q, want x13", got)
	}
}

func TestFormatARM64OperandHighTempFallsBackToScratch(t *testing.T) {
	got := formatARM64Operand(mir.NewRegister("t900", mir.VirtualReg, mir.NewScalarType("i32", 4)))
	if got != "w15" {
		t.Fatalf("formatARM64Operand(t900) = %q, want w15", got)
	}
}

func TestARM64LabelIsScopedPerFunction(t *testing.T) {
	prev := arm64FuncLabelScope
	arm64FuncLabelScope = "Strings$TrimLeft"
	defer func() { arm64FuncLabelScope = prev }()

	if got := arm64Label("if_end.2"); got != "LStrings$TrimLeft$if_end.2" {
		t.Fatalf("scoped arm64Label = %q, want %q", got, "LStrings$TrimLeft$if_end.2")
	}
	if got := arm64Label("Strings$TrimLeft$if_end.2"); got != "LStrings$TrimLeft$if_end.2" {
		t.Fatalf("already-scoped arm64Label = %q, want unchanged scoped label", got)
	}
}

func TestEmitARM64MachineInstrAndWithSymbolMaterializesAddress(t *testing.T) {
	dst := mir.NewRegister("x1", mir.PhysicalReg, mir.NewScalarType("i64", 8))
	mi := &mir.MachineInstr{
		Op:   "and",
		Dsts: []*mir.Register{dst},
		Srcs: []mir.Operand{mir.NewSymbol("_Lstr_deadbeef_0", nil), mir.NewImmediate(int64(255), mir.NewScalarType("i64", 8))},
	}
	out := emitARM64MachineInstr(mi)
	if !strings.Contains(out, "adrp x9") || !strings.Contains(out, "and x1") {
		t.Fatalf("expected symbol materialization for and, got %q", out)
	}
}

func TestEmitARM64SIToFPUsesFloatDestinationReg(t *testing.T) {
	i := &mir.MachineInstr{
		Op:   "sitofp",
		Dsts: []*mir.Register{mir.NewRegister("x0", mir.PhysicalReg, mir.NewScalarType("f64", 8))},
		Srcs: []mir.Operand{mir.NewRegister("x1", mir.PhysicalReg, mir.NewScalarType("i64", 8))},
	}
	out := emitARM64MachineInstr(i)
	if out != "scvtf d0, x1" {
		t.Fatalf("sitofp emission = %q, want %q", out, "scvtf d0, x1")
	}
}

func TestEmitARM64FloatDivWithImmediateMaterializesConstant(t *testing.T) {
	i := &mir.MachineInstr{
		Op:   "fdiv",
		Dsts: []*mir.Register{mir.NewRegister("x0", mir.PhysicalReg, mir.NewScalarType("f64", 8))},
		Srcs: []mir.Operand{mir.NewRegister("x1", mir.PhysicalReg, mir.NewScalarType("f64", 8)), mir.NewImmediate(1e6, mir.NewScalarType("f64", 8))},
	}
	out := emitARM64MachineInstr(i)
	if !strings.Contains(out, "fdiv d0, d1, d15") {
		t.Fatalf("fdiv emission = %q, expected register-operand form", out)
	}
	if strings.Contains(out, "#1e+06") {
		t.Fatalf("fdiv emission still uses raw float immediate: %q", out)
	}
}

func TestFormatARM64DataRegHighIndexFallsBackToScratch(t *testing.T) {
	got := formatARM64DataReg(mir.NewRegister("t268", mir.VirtualReg, mir.NewScalarType("f64", 8)), mir.NewScalarType("f64", 8))
	if got != "d15" {
		t.Fatalf("formatARM64DataReg high index = %q, want d15", got)
	}
}

func TestEmitARM64BinaryAndWithSymbolMaterializesAddress(t *testing.T) {
	b := &mir.BinaryInstr{
		Dst:   mir.NewRegister("x1", mir.PhysicalReg, mir.NewScalarType("i64", 8)),
		Op:    "and",
		Left:  mir.NewSymbol("_Lstr_deadbeef_0", nil),
		Right: mir.NewImmediate(int64(255), mir.NewScalarType("i64", 8)),
	}
	out := emitARM64Instr(b)
	if !strings.Contains(out, "adrp x9") || !strings.Contains(out, "and x1, x9, #255") {
		t.Fatalf("expected symbol materialization in BinaryInstr and, got %q", out)
	}
}

func TestEmitARM64UnaryTruncWithSymbolMaterializesAddress(t *testing.T) {
	u := &mir.UnaryInstr{
		Dst: mir.NewRegister("x1", mir.PhysicalReg, mir.NewScalarType("i32", 4)),
		Op:  "trunc",
		X:   mir.NewSymbol("_Lstr_deadbeef_0", nil),
	}
	out := emitARM64Instr(u)
	if !strings.Contains(out, "adrp x9") || !strings.Contains(out, "and x1, x9, #4294967295") {
		t.Fatalf("expected trunc symbol materialization, got %q", out)
	}
}
