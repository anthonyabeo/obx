package regalloc

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// Run performs backend MIR register allocation for every function in prog.
func Run(prog *mir.Program, tgt target.Target) (*mir.Program, error) {
	if prog == nil {
		return nil, fmt.Errorf("backend register allocation: nil MIR program before register allocation")
	}
	if tgt == nil {
		return nil, fmt.Errorf("backend register allocation: nil target")
	}

	for _, mod := range prog.Modules {
		if mod == nil {
			continue
		}
		for _, fn := range mod.Functions {
			if fn == nil {
				continue
			}
			frame, err := allocateFunction(fn, tgt)
			if err != nil {
				return nil, fmt.Errorf("register allocation for %s.%s: %w", mod.Name, fn.Name, err)
			}
			fn.Frame = frame
		}
	}

	return prog, nil
}

func allocateFunction(fn *mir.Function, tgt target.Target) (*mir.FrameLayout, error) {
	if fn == nil {
		return nil, fmt.Errorf("nil function")
	}
	if tgt == nil {
		return nil, fmt.Errorf("nil target")
	}

	abi := tgt.ABIInfo()
	analysis := analyzeFunction(fn)
	if len(analysis.blocks) == 0 {
		return mir.NewFrameLayout(), nil
	}

	colors, scratch := abiColorPools(abi)
	if len(colors) == 0 {
		return nil, fmt.Errorf("target %s exposes no allocatable registers", tgt.Name())
	}
	if len(scratch) == 0 {
		return nil, fmt.Errorf("target %s exposes no scratch registers for spill rewriting", tgt.Name())
	}

	alloc, err := colorGraph(fn, analysis, colors, scratch, abi)
	if err != nil {
		return nil, err
	}

	frame := buildFrameLayout(alloc, fn, abi)
	if err := rewriteFunction(fn, alloc, analysis, frame, abi); err != nil {
		return nil, err
	}

	return frame, nil
}

