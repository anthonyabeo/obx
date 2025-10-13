package backend

import (
	"fmt"
	"os"

	"github.com/anthonyabeo/obx/src/backend/isel"
	"github.com/anthonyabeo/obx/src/backend/isel/bud"
	"github.com/anthonyabeo/obx/src/backend/isel/bud/parser"
	"github.com/anthonyabeo/obx/src/backend/ralloc"
	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/asm"
	"github.com/anthonyabeo/obx/src/ir/mir"
)

func Compile(module *mir.Module, mach target.Machine) string {
	asmModule := &asm.Module{Name: module.Name, Globals: make(map[string]*asm.Symbol)}

	for name, global := range module.Globals {
		asmModule.Globals[name] = &asm.Symbol{
			Name: name,
			Size: global.Size,
			Ty:   mir.MirTypeToAsmType(global.Typ)}
	}

	// Instruction Selection
	for _, fn := range module.Funcs {
		iSel(fn, mach)
		asmModule.Funcs = append(asmModule.Funcs, fn.Asm)

		for _, local := range fn.Locals {
			if fn.Asm.Locals == nil {
				fn.Asm.Locals = make(map[string]asm.Symbol)
			}

			fn.Asm.Locals[local.Name()] = asm.Symbol{
				Name: local.Name(),
				Ty:   mir.MirTypeToAsmType(local.Type()),
			}
		}

		for _, value := range fn.Params {
			fn.Asm.Params = append(fn.Asm.Params, &asm.Symbol{
				Name: value.Name(),
				Kind: asm.ParamSK,
				Ty:   mir.MirTypeToAsmType(value.Type()),
			})
		}
	}

	// Register Allocation
	for _, fn := range asmModule.Funcs {
		ralloc.BuildCFG(fn)
		ralloc.Liveness(fn)
		intervals := ralloc.BuildIntervals(fn)
		alloc := ralloc.LinearScan(fn, intervals, mach)

		spillSlots := map[string]asm.SpillInfo{}
		for vreg, idx := range alloc.TempSpillMap {
			spillSlots[vreg] = asm.SpillInfo{
				Index: idx,
				Size:  mach.FrameInfo().WordSize,
				Align: mach.FrameInfo().WordSize,
			}
		}

		fn.Spills = spillSlots
		fn.CalleeRegsUed = alloc.CalleeSavedUsed

		frameLayout := target.ComputeFrameLayout(fn, mach)
		frameLayout.Output()

		ralloc.Rewrite(fn, alloc, mach, frameLayout)
	}

	module.Asm = asmModule

	for _, function := range asmModule.Funcs {
		function.OutputDOT()
	}

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

	asmFn := &asm.Function{Name: fn.Name, Exported: fn.Exported, IsLeaf: fn.IsLeaf}

	for id := fn.Entry.ID; id <= fn.Exit.ID; id++ {
		block := fn.Blocks[id]

		asmBlock := asm.NewBlock(id, block.Label)
		selectedInst := make([]*asm.Instr, 0)

		if id != fn.Exit.ID {
			for _, inst := range block.Instrs {
				pattern := bud.PatMIRInst(inst)

				selectedInst = append(selectedInst, selector.Select(pattern)...)
			}
			asmBlock.Instr = selectedInst
			asmBlock.Term = selectedInst[len(selectedInst)-1]
		}
		asmFn.Blocks = append(asmFn.Blocks, asmBlock)

		if id == fn.Entry.ID {
			asmFn.Entry = asmBlock
		}

		if id == fn.Exit.ID {
			asmFn.Exit = asmBlock
		}
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
