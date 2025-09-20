package target

import (
	"github.com/anthonyabeo/obx/src/ir/asm"
	"github.com/anthonyabeo/obx/src/ir/mir"
)

type Machine interface {
	Name() string
	Triple() string
	InstrInfo()    // describe the machine instructions supported by the target.
	RegisterInfo() // describe the register file of the target and any interactions between the registers
	FrameInfo()

	SelectInstr(*mir.Function) *asm.Function
	Legalize(*asm.Function)
	AllocRegs(*asm.Function)
	Emit(*asm.Module) string
}
