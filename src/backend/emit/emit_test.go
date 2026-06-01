package emit

import (
	"reflect"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type stubTarget struct{ name string }

func (s stubTarget) Name() string { return s.name }
func (s stubTarget) ABIInfo() target.ABI { return target.ABI{} }
func (s stubTarget) SupportsIntegerScalar(*mir.Type) bool { return true }
func (s stubTarget) LowerPhiBlock(string, []*mir.PhiInstr) (*target.PhiPlan, error) {
	return &target.PhiPlan{}, nil
}
func (s stubTarget) LowerSwitch(*mir.SwitchInstr) (*target.SwitchPlan, error) {
	return &target.SwitchPlan{}, nil
}
func (s stubTarget) LowerCall(*mir.CallInstr) (*target.CallPlan, error) {
	return &target.CallPlan{}, nil
}
func (s stubTarget) Emit(*mir.Module) string { return "" }

func TestLinkFlagsDarwinClangDropsLibc(t *testing.T) {
	e := New(Config{
		Target: stubTarget{name: target.Arm64AppleMacosName},
		Toolchain: Toolchain{
			Linker: "clang",
		},
	})
	e.linkLibs["libc"] = struct{}{}
	e.linkLibs["libm"] = struct{}{}

	got := e.linkFlags()
	want := []string{"-lm"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("linkFlags() = %v, want %v", got, want)
	}
}

func TestLinkFlagsDarwinLDMapsLibcToSystem(t *testing.T) {
	e := New(Config{
		Target: stubTarget{name: target.AArch64AppleDarwinName},
		Toolchain: Toolchain{
			Linker: "ld",
		},
	})
	e.linkLibs["libc"] = struct{}{}

	got := e.linkFlags()
	want := []string{"-lSystem"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("linkFlags() = %v, want %v", got, want)
	}
}

func TestLinkFlagsNonDarwinKeepsLibc(t *testing.T) {
	e := New(Config{Target: stubTarget{name: target.RV64IMAFDName}})
	e.linkLibs["libc"] = struct{}{}

	got := e.linkFlags()
	want := []string{"-lc"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("linkFlags() = %v, want %v", got, want)
	}
}

