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

func Compile(fn *mir.Function, mach target.Machine) {
	iSel(fn, mach)
}

func iSel(fn *mir.Function, target target.Machine) {
	tdFile := fmt.Sprintf("%s.td", target.Name())

	tdContent, err := os.ReadFile(tdFile)
	if err != nil {

	}
	src := string(tdContent)

	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)
	machine := p.Parse()

	selector := isel.NewSelector(machine.Rules)

	asmFn := &asm.Function{Name: fn.Name}

	for _, block := range fn.Blocks {
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

	for _, block := range asmFn.Blocks {
		fmt.Println(block.Label + ":\n")
		for _, instr := range block.Instr {
			fmt.Println("  " + instr.String() + "\n")
		}
	}
}
