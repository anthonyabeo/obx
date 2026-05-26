package selector

import (
	"path/filepath"
	"runtime"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

func descriptorPath(t *testing.T, name string) string {
	t.Helper()
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller failed")
	}
	return filepath.Join(filepath.Dir(file), "desc", name)
}

func collectRuleNames(items []BlockItem, out map[string]struct{}) {
	for _, item := range items {
		switch n := item.(type) {
		case *Rule:
			out[n.Name] = struct{}{}
		case *Block:
			collectRuleNames(n.Items, out)
		}
	}
}

func headerFieldValue(h *Header, key string) string {
	if h == nil {
		return ""
	}
	for _, f := range h.Fields {
		if f != nil && f.Key == key && f.Val != nil {
			return valueString(f.Val)
		}
	}
	return ""
}

func TestBackendDescriptorFilesParseAndCoverCoreRules(t *testing.T) {
	for _, tc := range []struct{ name, abi string }{
		{"arm64.td", "AAPCS64"},
		{"arm64-apple-macos.td", "AAPCS64-MACOS"},
		{"rv64imafd.td", "LP64D"},
	} {
		name := tc.name
		f, err := ParseFilePath(descriptorPath(t, name))
		if err != nil {
			t.Fatalf("ParseFilePath(%s) failed: %v", name, err)
		}
		if got := headerFieldValue(f.Header, "ABI"); got != tc.abi {
			t.Fatalf("%s: ABI header = %q, want %q", name, got, tc.abi)
		}
		if len(f.Targets) != 1 {
			t.Fatalf("%s: expected 1 target, got %d", name, len(f.Targets))
		}
		rules := map[string]struct{}{}
		collectRuleNames(f.Targets[0].Items, rules)
		for _, want := range []string{"LOADmem", "LOADrr", "LOADsym", "STOREmem", "STORErr", "JMP", "RET_void", "MOVrr", "MOVzero" /* "MOVany",*/, "CALL_direct", "CALL_indirect"} {
			if _, ok := rules[want]; !ok {
				t.Fatalf("%s: missing rule %q", name, want)
			}
		}
	}
}

func TestArm64DescriptorABIReturnLowering(t *testing.T) {
	parsed, err := ParseFilePath(descriptorPath(t, "arm64.td"))
	if err != nil {
		t.Fatalf("ParseFilePath failed: %v", err)
	}
	sel, err := New(parsed)
	if err != nil {
		t.Fatalf("New selector failed: %v", err)
	}

	blk := mir.NewBlock(0, "entry")
	blk.Term = &mir.ReturnInstr{Value: mir.NewRegister("v7", mir.VirtualReg, mir.NewScalarType("i64", 8))}
	out, err := sel.SelectBlock(blk)
	if err != nil {
		t.Fatalf("SelectBlock failed: %v", err)
	}
	if len(out.Instrs) != 0 {
		t.Fatalf("len(out.Instrs) = %d, want 0", len(out.Instrs))
	}
	mt, ok := out.Term.(*mir.MachineTerm)
	if !ok || mt.Op != "ret" || len(mt.Srcs) != 1 || mt.Srcs[0].String() != "v7" {
		t.Fatalf("term = %#v, want ret", out.Term)
	}
}

func TestArm64DescriptorSelectsMoveVariants(t *testing.T) {
	parsed, err := ParseFilePath(descriptorPath(t, "arm64.td"))
	if err != nil {
		t.Fatalf("ParseFilePath failed: %v", err)
	}
	sel, err := New(parsed)
	if err != nil {
		t.Fatalf("New selector failed: %v", err)
	}

	moveCopy := &mir.MoveInstr{
		Dst: mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Src: mir.NewRegister("v1", mir.VirtualReg, mir.NewScalarType("i64", 8)),
	}
	selected, err := sel.SelectInstr(moveCopy)
	if err != nil {
		t.Fatalf("SelectInstr(copy) failed: %v", err)
	}
	if len(selected) != 1 {
		t.Fatalf("copy selected len = %d, want 1", len(selected))
	}
	if mi, ok := selected[0].(*mir.MachineInstr); !ok || mi.Op != "add" || len(mi.Srcs) != 2 {
		t.Fatalf("copy selected = %#v, want add-based specialization", selected[0])
	}

	zero := &mir.MoveInstr{
		Dst: mir.NewRegister("v2", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Src: mir.NewRegister("xzr", mir.PhysicalReg, mir.NewScalarType("i64", 8)),
	}
	selected, err = sel.SelectInstr(zero)
	if err != nil {
		t.Fatalf("SelectInstr(zero) failed: %v", err)
	}
	if len(selected) != 1 {
		t.Fatalf("zero selected len = %d, want 1", len(selected))
	}
	if mi, ok := selected[0].(*mir.MachineInstr); !ok || mi.Op != "add" || len(mi.Srcs) != 2 {
		t.Fatalf("zero selected = %#v, want add-based zero materialization", selected[0])
	}
}

func TestArm64DescriptorSelectsMemoryOperands(t *testing.T) {
	parsed, err := ParseFilePath(descriptorPath(t, "arm64.td"))
	if err != nil {
		t.Fatalf("ParseFilePath failed: %v", err)
	}
	sel, err := New(parsed)
	if err != nil {
		t.Fatalf("New selector failed: %v", err)
	}

	addr := mir.NewMemory(
		mir.NewRegister("v10", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		mir.NewImmediate(16, mir.NewScalarType("i64", 8)),
		mir.NewScalarType("ptr", 8),
	)

	load := &mir.LoadInstr{
		Dst:  mir.NewRegister("v11", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Addr: addr,
	}
	selected, err := sel.SelectInstr(load)
	if err != nil {
		t.Fatalf("SelectInstr(load[mem]) failed: %v", err)
	}
	if len(selected) != 1 {
		t.Fatalf("load[mem] selected len = %d, want 1", len(selected))
	}
	if mi, ok := selected[0].(*mir.MachineInstr); !ok || mi.Op != "load" || len(mi.Srcs) != 1 {
		t.Fatalf("load[mem] selected = %#v", selected[0])
	}

	store := &mir.StoreInstr{Addr: addr, Value: mir.NewImmediate(99, mir.NewScalarType("i64", 8))}
	selected, err = sel.SelectInstr(store)
	if err != nil {
		t.Fatalf("SelectInstr(store[mem]) failed: %v", err)
	}
	if len(selected) != 1 {
		t.Fatalf("store[mem] selected len = %d, want 1", len(selected))
	}
	if mi, ok := selected[0].(*mir.MachineInstr); !ok || mi.Op != "store" || len(mi.Srcs) != 2 {
		t.Fatalf("store[mem] selected = %#v", selected[0])
	}
}

func TestArm64DescriptorSelectsCoreInstructions(t *testing.T) {
	parsed, err := ParseFilePath(descriptorPath(t, "arm64.td"))
	if err != nil {
		t.Fatalf("ParseFilePath failed: %v", err)
	}
	sel, err := New(parsed)
	if err != nil {
		t.Fatalf("New selector failed: %v", err)
	}

	load := &mir.LoadInstr{
		Dst:  mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Addr: mir.NewSymbol("g0", mir.NewScalarType("i64", 8)),
	}
	selected, err := sel.SelectInstr(load)
	if err != nil {
		t.Fatalf("SelectInstr(load) failed: %v", err)
	}
	if len(selected) != 1 {
		t.Fatalf("load selected len = %d, want 1", len(selected))
	}
	if mi, ok := selected[0].(*mir.MachineInstr); !ok || mi.Op != "load" {
		t.Fatalf("load selected = %#v, want machine load", selected[0])
	}

	store := &mir.StoreInstr{
		Addr:  mir.NewSymbol("g1", mir.NewScalarType("i64", 8)),
		Value: mir.NewImmediate(7, mir.NewScalarType("i64", 8)),
	}
	selected, err = sel.SelectInstr(store)
	if err != nil {
		t.Fatalf("SelectInstr(store) failed: %v", err)
	}
	if len(selected) != 2 {
		t.Fatalf("store selected len = %d, want 2", len(selected))
	}
	if mi, ok := selected[0].(*mir.MachineInstr); !ok || mi.Op != "mov" {
		t.Fatalf("store selected[0] = %#v, want mov materialization", selected[0])
	}
	if mi, ok := selected[1].(*mir.MachineInstr); !ok || mi.Op != "store" {
		t.Fatalf("store selected[1] = %#v, want store", selected[1])
	}

	j := &mir.JumpInstr{Target: "L1"}
	term, err := sel.SelectTerminator(j)
	if err != nil {
		t.Fatalf("SelectTerminator(jmp) failed: %v", err)
	}
	if mt, ok := term.(*mir.MachineTerm); !ok || mt.Op != "j" || len(mt.Targets) != 1 || mt.Targets[0] != "L1" {
		t.Fatalf("jump term = %#v", term)
	}

	br := &mir.CondBrInstr{
		Cond:       mir.NewRegister("v1", mir.VirtualReg, mir.NewScalarType("i1", 1)),
		TrueLabel:  "Ltrue",
		FalseLabel: "Lfalse",
	}
	term, err = sel.SelectTerminator(br)
	if err != nil {
		t.Fatalf("SelectTerminator(br) failed: %v", err)
	}
	if mt, ok := term.(*mir.MachineTerm); !ok || mt.Op != "bne" || len(mt.Targets) != 2 {
		t.Fatalf("branch term = %#v", term)
	}

	retBlk := mir.NewBlock(1, "ret")
	retBlk.Term = &mir.ReturnInstr{Value: mir.NewImmediate(42, mir.NewScalarType("i64", 8))}
	outRet, err := sel.SelectBlock(retBlk)
	if err != nil {
		t.Fatalf("SelectBlock(ret) failed: %v", err)
	}
	if len(outRet.Instrs) != 0 {
		t.Fatalf("return block instrs = %d, want 0", len(outRet.Instrs))
	}
	if mt, ok := outRet.Term.(*mir.MachineTerm); !ok || mt.Op != "ret" || len(mt.Srcs) != 1 || mt.Srcs[0].String() != "42" {
		t.Fatalf("return term = %#v", outRet.Term)
	}
}

func TestArm64DescriptorSelectsCallDirect(t *testing.T) {
	parsed, err := ParseFilePath(descriptorPath(t, "arm64.td"))
	if err != nil {
		t.Fatalf("ParseFilePath failed: %v", err)
	}
	sel, err := New(parsed)
	if err != nil {
		t.Fatalf("New selector failed: %v", err)
	}

	// Simulate a bare call as produced by LowerCallsInProgram.
	call := &mir.CallInstr{
		Callee: mir.NewSymbol("foo", mir.NewScalarType("i64", 8)),
		Args:   nil,
		Dst:    nil,
	}
	selected, err := sel.SelectInstr(call)
	if err != nil {
		t.Fatalf("SelectInstr(CALL_direct) failed: %v", err)
	}
	if len(selected) != 1 {
		t.Fatalf("expected 1 machine instr, got %d", len(selected))
	}
	mi, ok := selected[0].(*mir.MachineInstr)
	if !ok || mi.Op != "bl" {
		t.Fatalf("expected MachineInstr{op:bl}, got %#v", selected[0])
	}
}

func TestArm64DescriptorSelectsCallIndirect(t *testing.T) {
	parsed, err := ParseFilePath(descriptorPath(t, "arm64.td"))
	if err != nil {
		t.Fatalf("ParseFilePath failed: %v", err)
	}
	sel, err := New(parsed)
	if err != nil {
		t.Fatalf("New selector failed: %v", err)
	}

	// Indirect call: callee is a virtual register (function pointer).
	call := &mir.CallInstr{
		Callee: mir.NewRegister("fp", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Args:   nil,
		Dst:    nil,
	}
	selected, err := sel.SelectInstr(call)
	if err != nil {
		t.Fatalf("SelectInstr(CALL_indirect) failed: %v", err)
	}
	if len(selected) != 1 {
		t.Fatalf("expected 1 machine instr, got %d", len(selected))
	}
	mi, ok := selected[0].(*mir.MachineInstr)
	if !ok || mi.Op != "blr" {
		t.Fatalf("expected MachineInstr{op:blr}, got %#v", selected[0])
	}
}
