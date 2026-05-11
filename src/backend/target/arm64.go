package target

import "github.com/anthonyabeo/obx/src/backend/mir"

// ARM64Target is the first minir-first target implementation.
type ARM64Target struct {
	*BaseTarget
}

var arm64Default = &ARM64Target{
	BaseTarget: NewBaseTarget("arm64", ABI{
		WordSize:            8,
		Align:               16,
		IntArgRegs:          []string{"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"},
		IntRetRegs:          []string{"x0", "x1"},
		CallerSaved:         []string{"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18"},
		CalleeSaved:         []string{"x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28"},
		StackPointer:        "sp",
		FramePointer:        "x29",
		LinkRegister:        "x30",
		JumpTableMinDensity: 0.5,
	}),
}

func (t *ARM64Target) Emit(*mir.Module) string {
	panic("Emit not implemented for ARM64Target yet")
}

func init() {
	Register("arm64", func() Target { return arm64Default })
}
