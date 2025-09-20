package riscv

import (
	"github.com/anthonyabeo/obx/src/ir/asm"
	"github.com/anthonyabeo/obx/src/ir/mir"
	"strings"
)

type RV64IMAFD struct {
}

func NewRV64IMAFDTarget() *RV64IMAFD {
	return &RV64IMAFD{}
}

func (r RV64IMAFD) Name() string { return "rv64imafd" }

func (r RV64IMAFD) Triple() string {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) InstrInfo() {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) RegisterInfo() {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) FrameInfo() {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) SelectInstr(function *mir.Function) *asm.Function {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) Legalize(fn *asm.Function) {

}

func (r RV64IMAFD) AllocRegs(function *asm.Function) {
	//TODO implement me
	panic("implement me")
}

func (r RV64IMAFD) Emit(module *asm.Module) string {
	var buf strings.Builder

	buf.WriteString(".section .data\n")
	for _, global := range module.Globals {
		buf.WriteString(r.formatGlobal(global))
	}
	buf.WriteString("\n")

	var funcNames []string
	funcMap := make(map[string]*asm.Function)
	for _, function := range module.Funcs {
		funcNames = append(funcNames, function.Name)
		funcMap[function.Name] = function
	}
	// sort function names to have deterministic output
	// (map iteration order is random)
	for _, name := range funcNames {
		function := funcMap[name]
		buf.WriteString(r.formatFunc(function))
	}

	return buf.String()
}

func (r RV64IMAFD) formatFunc(fn *asm.Function) string {
	var buf strings.Builder

	buf.WriteString(".section .text\n")

	fnName := fn.Name
	if strings.HasPrefix(fnName, "__init_") {
		fnName = "start"
		fn.Exported = true
	}

	if fn.Exported {
		buf.WriteString(".globl " + fnName + "\n")
	}

	buf.WriteString(fnName + ":\n")
	for _, block := range fn.Blocks {
		if block.Label != "entry" {
			buf.WriteString(block.Label + ":\n")
		}

		for _, inst := range block.Instr {
			buf.WriteString("  " + inst.String() + "\n")
		}
		buf.WriteString("\n")
	}

	return buf.String()
}

func (r RV64IMAFD) formatGlobal(g *asm.Global) string {
	var buf strings.Builder

	buf.WriteString(g.Name + ":\n")
	buf.WriteString("  " + r.getType(g.Ty) + " " + "\n")

	return buf.String()
}

func (r RV64IMAFD) getType(ty asm.Type) string {
	switch ty {
	case asm.I32:
		return ".word"
	default:
		panic("unhandled type" + ty.String())
	}
}
