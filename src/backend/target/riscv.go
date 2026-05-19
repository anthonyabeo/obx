package target

import "github.com/anthonyabeo/obx/src/backend/mir"

// RISCV64Target is the first minir-first target implementation.
type RISCV64Target struct {
	*BaseTarget
}

func NewRISCV64Target() *RISCV64Target {
	return &RISCV64Target{
		BaseTarget: NewBaseTarget(RV64IMAFDName, ABI{
			WordSize:            8,
			Align:               16,
			IntArgRegs:          []string{"a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"},
			IntRetRegs:          []string{"a0", "a1"},
			FloatArgRegs:        []string{"fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"},
			FloatRetRegs:        []string{"fa0", "fa1"},
			CallerSaved:         []string{"ra", "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"},
			CalleeSaved:         []string{"s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11"},
			StackPointer:        "sp",
			FramePointer:        "s0",
			LinkRegister:        "ra",
			JumpTableMinDensity: 0.5,
		}),
	}
}

func (t *RISCV64Target) Emit(*mir.Module) string {
	panic("Emit not implemented for RISCV64Target yet")
}

func init() {
	Register(RV64IMAFDName, func() Target { return NewRISCV64Target() })
	RegisterAlias(RISCV64AliasName, RV64IMAFDName)
}
