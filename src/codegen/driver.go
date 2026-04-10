package codegen

import (
	"fmt"
	"os"

	"github.com/anthonyabeo/obx/src/codegen/asm"
	"github.com/anthonyabeo/obx/src/codegen/bud"
	"github.com/anthonyabeo/obx/src/codegen/bud/parser"
	"github.com/anthonyabeo/obx/src/codegen/isel"
	"github.com/anthonyabeo/obx/src/codegen/ralloc"
	"github.com/anthonyabeo/obx/src/codegen/target"
	"github.com/anthonyabeo/obx/src/ir/mir"
)

// CompileOptions controls optional codegen behavior.
type CompileOptions struct {
	// Debug enables frame-layout dumps and CFG dot-file generation.
	Debug bool
}

func Compile(module *mir.Module, mach target.Machine, targetDescPath string, opts CompileOptions) (string, error) {
	// Parse the target description once for the whole module.
	tdFile := fmt.Sprintf("%s/%s.td", targetDescPath, mach.Name())
	tdContent, err := os.ReadFile(tdFile)
	if err != nil {
		return "", fmt.Errorf("reading target description %q: %w", tdFile, err)
	}
	machine := parser.NewParser(parser.NewLexer(string(tdContent))).Parse()
	selector := isel.NewSelector(machine.Rules)

	asmModule := &asm.Module{Name: module.Name, Globals: make(map[string]*asm.Symbol)}

	for name, global := range module.Globals {
		asmModule.Globals[name] = &asm.Symbol{
			Name: name,
			Kind: asm.GlobalSK,
			Size: global.Size,
			Ty:   mir.ToAsmType(global.Typ)}
	}

	// Instruction Selection
	for _, fn := range module.Funcs {
		iSel(fn, selector, mach)
		asmModule.Funcs = append(asmModule.Funcs, fn.Asm)
		mach.Legalize(fn.Asm)

		for _, local := range fn.Locals {
			if fn.Asm.Locals == nil {
				fn.Asm.Locals = make(map[string]asm.Symbol)
			}

			fn.Asm.Locals[local.Name()] = asm.Symbol{
				Name: local.Name(),
				Ty:   mir.ToAsmType(local.Type()),
			}
		}

		for _, value := range fn.Params {
			fn.Asm.Params = append(fn.Asm.Params, &asm.Symbol{
				Name: value.Name(),
				Kind: asm.ParamSK,
				Ty:   mir.ToAsmType(value.Type()),
			})
		}

		for _, c := range fn.Constants {
			fn.Asm.Constant = append(fn.Asm.Constant, asm.Constant{
				Name:  c.Name(),
				Value: c.Value(),
				Type:  mir.ToAsmType(c.Type()),
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
		if opts.Debug {
			frameLayout.Output()
		}

		ralloc.Rewrite(fn, alloc, mach, frameLayout)
	}

	module.Asm = asmModule

	if opts.Debug {
		for _, function := range asmModule.Funcs {
			function.OutputDOT()
		}
	}

	return emit(module, mach), nil
}

// iSel runs instruction selection for fn using a pre-built selector, avoiding
// repeated TD file reads per function.
func iSel(fn *mir.Function, selector *isel.Selector, mach target.Machine) {
	asmFn := &asm.Function{Name: fn.FnName, Exported: fn.Exported, IsLeaf: fn.IsLeaf}

	for _, block := range fn.SortedBlocks() {
		asmBlock := asm.NewBlock(block.ID, block.Label)
		if block.ID != fn.Exit.ID {
			for _, inst := range block.Instrs {
				asmBlock.Instr = append(asmBlock.Instr, selector.Select(bud.PatMIRInst(inst))...)
			}
			asmBlock.Term = asmBlock.Instr[len(asmBlock.Instr)-1]
		}
		asmFn.Blocks = append(asmFn.Blocks, asmBlock)
		if block.ID == fn.Entry.ID {
			asmFn.Entry = asmBlock
		}
		if block.ID == fn.Exit.ID {
			asmFn.Exit = asmBlock
		}
	}

	fn.Asm = asmFn
}

func emit(module *mir.Module, mach target.Machine) string {
	if module.Asm == nil {
		return ""
	}
	return mach.Emit(module.Asm)
}
