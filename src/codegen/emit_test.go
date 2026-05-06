package codegen

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/codegen/asm"
	"github.com/anthonyabeo/obx/src/codegen/target"
	"github.com/anthonyabeo/obx/src/ir/obxir"
)

// fakeMachine is a minimal target.Machine implementation used for unit tests.
type fakeMachine struct{}

func (f *fakeMachine) Name() string                       { return "fake" }
func (f *fakeMachine) InstrInfo()                         {}
func (f *fakeMachine) RegisterInfo() *target.RegisterFile { return &target.RegisterFile{} }
func (f *fakeMachine) FrameInfo() *target.FrameInfo {
	return &target.FrameInfo{PointerSize: 8, WordSize: 8, FrameAlign: 16, StackGrowsDown: true}
}
func (f *fakeMachine) Alignment(t asm.Type) int             { return 8 }
func (f *fakeMachine) AssignParams(n int) []target.Location { return nil }
func (f *fakeMachine) Legalize(a *asm.Function)             {}
func (f *fakeMachine) Emit(m *asm.Module) string {
	if m == nil {
		return "<nil>"
	}
	return fmt.Sprintf("EMIT:%s", m.Name)
}
func (f *fakeMachine) EmitPrologueEpilogue(fn *asm.Function, fl target.FrameLayout) {}

// TestEmit verifies that emit() delegates to the target's Emit method and
// returns an empty string when the module has no asm attached.
func TestEmit(t *testing.T) {
	mach := &fakeMachine{}

	// 1) module without Asm should emit empty string
	m1 := &obxir.Module{Name: "mod1", Asm: nil}
	out := emit(m1, mach)
	if out != "" {
		t.Fatalf("expected empty emit for nil Asm, got %q", out)
	}

	// 2) module with an asm.Module should be forwarded to target.Emit
	asmMod := &asm.Module{Name: "mod2"}
	m2 := &obxir.Module{Name: "mod2", Asm: asmMod}
	out2 := emit(m2, mach)
	if out2 != "EMIT:mod2" {
		t.Fatalf("unexpected emit output: %q", out2)
	}
}
