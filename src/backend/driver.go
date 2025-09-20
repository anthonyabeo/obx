package backend

import (
	"fmt"
	"os"

	"github.com/anthonyabeo/obx/src/backend/isel"
	"github.com/anthonyabeo/obx/src/backend/isel/bud"
	"github.com/anthonyabeo/obx/src/backend/isel/bud/parser"
	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/asm"
	"github.com/anthonyabeo/obx/src/ir/mir"
)

func Compile(module *mir.Module, mach target.Machine) string {
	asmModule := &asm.Module{Name: module.Name, Globals: make(map[string]*asm.Global)}

	for s, global := range module.Globals {
		asmModule.Globals[s] = &asm.Global{
			Name: s,
			Size: global.Size,
			Ty:   mir.MirTypeToAsmType(global.Typ)}
	}

	// Instruction Selection
	for _, fn := range module.Funcs {
		iSel(fn, mach)
		asmModule.Funcs = append(asmModule.Funcs, fn.Asm)
	}

	// Register Allocation

	module.Asm = asmModule

	return emit(module, mach)
}

func iSel(fn *mir.Function, target target.Machine) {
	tdFile := fmt.Sprintf("./target/desc/%s.td", target.Name())

	tdContent, err := os.ReadFile(tdFile)
	if err != nil {
		panic(err)
	}

	src := string(tdContent)

	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)
	machine := p.Parse()

	selector := isel.NewSelector(machine.Rules)

	asmFn := &asm.Function{Name: fn.Name}

	for id := fn.Entry.ID; id <= len(fn.Blocks); id++ {
		block := fn.Blocks[id]

		asmBlock := asm.NewBlock(block.Label)
		selectedInst := make([]*asm.Instr, 0)

		for _, inst := range block.Instrs {
			pattern := bud.PatMIRInst(inst)

			selectedInst = append(selectedInst, selector.Select(pattern)...)
		}
		asmBlock.Instr = selectedInst
		asmFn.Blocks = append(asmFn.Blocks, asmBlock)
	}

	// validate all the selected instructions ensuring tha they match the semantics
	// of the target ISA.
	target.Legalize(asmFn)

	fn.Asm = asmFn
}

func emit(module *mir.Module, mach target.Machine) string {
	if module.Asm == nil {
		return ""
	}
	return mach.Emit(module.Asm)
}
