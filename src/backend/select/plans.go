package selector

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// FunctionPlans groups target-level lowering plans associated with one MIR
// function.
type FunctionPlans struct {
	Phi    []*target.PhiPlan
	Switch []*target.SwitchPlan
	Calls  []*target.CallPlan
}

// BuildPlans walks a lowered backend MIR program and derives target-specific
// lowering plans from it.
func BuildPlans(prog *mir.Program, tgt target.Target) (map[string]*FunctionPlans, error) {
	if prog == nil {
		return map[string]*FunctionPlans{}, nil
	}
	if tgt == nil {
		return nil, fmt.Errorf("build plans: nil target")
	}
	plans := make(map[string]*FunctionPlans)
	for _, mod := range prog.Modules {
		if mod == nil {
			continue
		}
		for _, fn := range mod.Functions {
			if fn == nil {
				continue
			}
			fp, err := BuildFunctionPlans(fn, tgt)
			if err != nil {
				return nil, fmt.Errorf("build plans for %s.%s: %w", mod.Name, fn.Name, err)
			}
			plans[functionPlanKey(mod.Name, fn.Name)] = fp
		}
	}
	return plans, nil
}

// BuildFunctionPlans derives per-block and per-call target plans for one
// backend MIR function.
func BuildFunctionPlans(fn *mir.Function, tgt target.Target) (*FunctionPlans, error) {
	if fn == nil {
		return nil, fmt.Errorf("build function plans: nil function")
	}
	if tgt == nil {
		return nil, fmt.Errorf("build function plans: nil target")
	}
	plans := &FunctionPlans{}
	for _, block := range fn.Blocks {
		if block == nil {
			continue
		}
		phis := collectPhiInstrs(block)
		if len(phis) > 0 {
			plan, err := tgt.LowerPhiBlock(block.Label, phis)
			if err != nil {
				return nil, fmt.Errorf("block %s phi lowering: %w", block.Label, err)
			}
			plans.Phi = append(plans.Phi, plan)
		}
		for _, instr := range block.Instrs {
			if call, ok := instr.(*mir.CallInstr); ok {
				plan, err := tgt.LowerCall(call)
				if err != nil {
					return nil, fmt.Errorf("block %s call lowering: %w", block.Label, err)
				}
				plans.Calls = append(plans.Calls, plan)
			}
		}
		if sw, ok := block.Term.(*mir.SwitchInstr); ok {
			plan, err := tgt.LowerSwitch(sw)
			if err != nil {
				return nil, fmt.Errorf("block %s switch lowering: %w", block.Label, err)
			}
			plans.Switch = append(plans.Switch, plan)
		}
	}
	return plans, nil
}

func collectPhiInstrs(block *mir.Block) []*mir.PhiInstr {
	phis := make([]*mir.PhiInstr, 0)
	for _, instr := range block.Instrs {
		phi, ok := instr.(*mir.PhiInstr)
		if !ok {
			continue
		}
		phis = append(phis, phi)
	}
	return phis
}

func functionPlanKey(moduleName, functionName string) string {
	if moduleName == "" {
		return functionName
	}
	return moduleName + "." + functionName
}

