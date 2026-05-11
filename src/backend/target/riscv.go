package target

// RISCV64Target is the first minir-first target implementation.
type RISCV64Target struct {
	*BaseTarget
}

func NewRISCV64Target() *RISCV64Target {
	return &RISCV64Target{
		BaseTarget: NewBaseTarget("riscv64", ABI{
			WordSize:            8,
			Align:               16,
			IntArgRegs:          []string{"a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"},
			IntRetRegs:          []string{"a0", "a1"},
			CallerSaved:         []string{"ra", "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"},
			CalleeSaved:         []string{"s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11"},
			StackPointer:        "sp",
			FramePointer:        "s0",
			LinkRegister:        "ra",
			JumpTableMinDensity: 0.5,
		}),
	}
}
