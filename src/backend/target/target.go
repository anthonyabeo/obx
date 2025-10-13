package target

import (
	"github.com/anthonyabeo/obx/src/ir/asm"
)

type RegisterFile struct {
	// List of all registers
	AllRegs []string

	// List of all general purpose registers
	GeneralRegs []string

	// List of all floating-point registers
	FloatRegs []string

	// Map from register name to its index in AllRegs
	RegToIdx map[string]int

	// List of available general-purpose registers (in allocation order).
	Allocatable []string

	// Register classification (caller-saved vs callee-saved).
	CallerSaved map[string]bool
	CalleeSaved map[string]bool

	// Registers used for passing arguments (in order).
	ArgRegs []string

	// Registers used for returning values.
	ReturnRegs []string

	// Register used as the stack pointer.
	StackPointer string

	// Register used as the frame/base pointer (optional).
	FramePointer string

	Reserved []string // e.g., stack pointer, zero register, etc.

	// Number of arguments passed in registers before spilling to stack.
	MaxArgRegs int

	// Temporary registers
	Temporaries []string
}

type FrameInfo struct {
	// Pointer and word sizes in bytes (e.g., 8 for 64-bit).
	PointerSize int
	WordSize    int

	// Stack alignment (in bytes). For example, 16 on x86_64.
	FrameAlign int

	// Stack grows down (true) or up (false).
	StackGrowsDown bool
}

type Machine interface {
	Name() string
	InstrInfo()                  // describe the machine instructions supported by the target.
	RegisterInfo() *RegisterFile // describe the register file of the target and any interactions between the registers
	FrameInfo() *FrameInfo

	AssignParams(int) []Location
	Legalize(*asm.Function)
	Emit(*asm.Module) string
	EmitPrologueEpilogue(*asm.Function, FrameLayout)
}
