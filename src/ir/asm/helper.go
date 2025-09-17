package asm

// ////////////////////////////////////////////////////////////////
// Register Operand Properties					                //
// ////////////////////////////////////////////////////////////////
type (
	RegKind int
	RegMode int
)

func (k RegKind) String() string {
	switch k {
	case GPR:
		return "GPR"
	case FPR:
		return "FPR"
	case SPR:
		return "SPR"
	case VEC:
		return "VEC"
	default:
		panic("invalid register kind")
	}
}

func (m RegMode) String() string {
	switch m {
	case Virt:
		return "virt"
	case Phys:
		return "phys"
	default:
		panic("unknown register mode")
	}
}

const (
	Unknown RegKind = iota
	GPR
	FPR
	SPR
	VEC

	Virt RegMode = iota
	Phys
)

// ////////////////////////////////////////////////////////////////
// Relocation Function Kinds for RISC-V assembly				//
// ////////////////////////////////////////////////////////////////

type RelocKind int

func (k RelocKind) String() string {
	switch k {
	case Hi:
		return "%hi"
	case Lo:
		return "%lo"
	case Pcrel_lo:
		return "%pcrel_lo"
	case Pcrel_hi:
		return "%pcrel_hi"
	case Tprel_lo:
		return "%tprel_lo"
	case Tprel_hi:
		return "%tprel_hi"
	case Tprel_add:
		return "%tprel_add"
	default:
		panic("unhandled reloc kind")
	}
}

const (
	Hi RelocKind = iota
	Lo
	Pcrel_hi
	Pcrel_lo
	Tprel_hi
	Tprel_lo
	Tprel_add
)
