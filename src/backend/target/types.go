package target

import (
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

// ABI describes the call-convention and stack-shape metadata that all
// targets share in the first backend pass.
type ABI struct {
	WordSize int
	Align    int

	IntArgRegs   []string
	IntRetRegs   []string
	FloatArgRegs []string
	FloatRetRegs []string

	CallerSaved []string
	CalleeSaved []string

	StackPointer string
	FramePointer string
	LinkRegister string

	// JumpTableMinDensity controls when a switch plan prefers a jump table
	// over a compare chain. A zero value defaults to 0.5.
	JumpTableMinDensity float64
}

// ArgReg returns the register used for the i-th integer argument, if any.
func (abi ABI) ArgReg(i int) (string, bool) {
	if i < 0 || i >= len(abi.IntArgRegs) {
		return "", false
	}
	return abi.IntArgRegs[i], true
}

// FloatArgReg returns the register used for the i-th floating-point argument,
// if any.
func (abi ABI) FloatArgReg(i int) (string, bool) {
	if i < 0 || i >= len(abi.FloatArgRegs) {
		return "", false
	}
	return abi.FloatArgRegs[i], true
}

// RetReg returns the register used for the i-th integer result, if any.
func (abi ABI) RetReg(i int) (string, bool) {
	if i < 0 || i >= len(abi.IntRetRegs) {
		return "", false
	}
	return abi.IntRetRegs[i], true
}

// JumpTableDensity returns the density threshold used for switch lowering.
func (abi ABI) JumpTableDensity() float64 {
	if abi.JumpTableMinDensity > 0 {
		return abi.JumpTableMinDensity
	}
	return 0.5
}

// SupportsIntegerScalar reports whether ty is in the first-pass supported
// type set: scalar integers only.
func SupportsIntegerScalar(ty *mir.Type) bool {
	return ty != nil && ty.Elem == nil && ty.Len == 0 && ty.Size > 0
}

// IsFloatType reports whether ty is a floating-point scalar (f32 or f64).
func IsFloatType(ty *mir.Type) bool {
	if ty == nil {
		return false
	}
	n := strings.ToLower(ty.Name)
	return n == "f32" || n == "f64" || n == "float" || n == "double"
}
