package backend

import (
	"fmt"
	"path/filepath"
	"runtime"

	"github.com/anthonyabeo/obx/src/backend/legalize"
	"github.com/anthonyabeo/obx/src/backend/lower"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/regalloc"
	selector "github.com/anthonyabeo/obx/src/backend/select"
	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/minir"
)

// PipelineDriver orchestrates the backend pipeline stages.
//
// Current implementation:
//  1. Lower                 (minir -> backend/mir)
//  2. CallLowering          (ABI arg/result expansion; runs before instruction selection)
//  3. SwitchLowering        (switch → compare-chain or jump-table; runs before instruction selection)
//  4. InstructionSelection
//  5. Legalization          (target-aware post-pass)
//  6. InstructionScheduling (stub)
//  7. RegisterAllocation
//  8. PrologueEpilogue      (emit prologue/epilogue based on frame layout)
//  9. PhiRemoval            (remove phi nodes, insert move sequences on predecessor edges)
//  10. Assemble             (optional callback)
//  11. Link                 (optional callback)
//  12. BuildPlans           (compute phi/switch/call plans; sees zero phis after removal)
type PipelineDriver struct {
	Target   target.Target
	Selector *selector.Selector
	Stages   []string
	Assemble func(*mir.Program) error
	Link     func(*mir.Program) error
}

// NewPipelineDriver constructs a backend pipeline driver for tgt.
func NewPipelineDriver(tgt target.Target, stages ...string) *PipelineDriver {
	return &PipelineDriver{Target: tgt, Stages: append([]string(nil), stages...)}
}

// Run executes the backend pipeline from minir to target plans.
func (p *PipelineDriver) Run(prog *minir.Program) (*lower.LoweredProgram, error) {
	if p == nil {
		return nil, fmt.Errorf("backend pipeline: nil driver")
	}
	if p.Target == nil {
		return nil, fmt.Errorf("backend pipeline: nil target")
	}
	if p.Selector == nil {
		sel, err := p.loadSelector()
		if err != nil {
			return nil, err
		}
		p.Selector = sel
	}

	mprog, err := p.Lower(prog)
	if err != nil {
		return nil, err
	}

	for _, name := range p.stageNames() {
		stage, err := LookupStage(name)
		if err != nil {
			return nil, err
		}
		if !stage.Enabled(p.Target) {
			continue
		}
		mprog, err = stage.Run(p, mprog)
		if err != nil {
			return nil, fmt.Errorf("backend stage %s: %w", name, err)
		}
	}

	plans, err := selector.BuildPlans(mprog, p.Target)
	if err != nil {
		return nil, err
	}


	return &lower.LoweredProgram{MIR: mprog, Plans: plans}, nil
}

// Lower is the real front-end of the backend pipeline.
func (p *PipelineDriver) Lower(prog *minir.Program) (*mir.Program, error) {
	if p == nil {
		return nil, fmt.Errorf("backend pipeline: nil driver")
	}
	return lower.LowerProgram(prog)
}

func (p *PipelineDriver) InstructionSelection(prog *mir.Program) (*mir.Program, error) {
	if p == nil {
		return nil, fmt.Errorf("backend pipeline: nil driver")
	}
	if prog == nil {
		return nil, fmt.Errorf("backend pipeline: nil MIR program before instruction selection")
	}
	if p.Selector == nil {
		return p.passThrough("instruction selection", prog)
	}

	selected, err := p.Selector.SelectProgram(prog)
	if err != nil {
		return nil, fmt.Errorf("instruction selection: %w", err)
	}

	return selected, nil
}

func (p *PipelineDriver) Legalization(prog *mir.Program) (*mir.Program, error) {
	if p == nil {
		return nil, fmt.Errorf("backend pipeline: nil driver")
	}
	if prog == nil {
		return nil, fmt.Errorf("backend pipeline: nil MIR program before legalization")
	}
	if p.Target == nil {
		return nil, fmt.Errorf("backend pipeline: nil target before legalization")
	}

	return legalize.Run(prog, p.Target)
}

// InstructionScheduling is currently a stub.
func (p *PipelineDriver) InstructionScheduling(prog *mir.Program) (*mir.Program, error) {
	return p.passThrough("instruction scheduling", prog)
}

// CallLowering expands each *mir.CallInstr into ABI-explicit moves + a bare call.
func (p *PipelineDriver) CallLowering(prog *mir.Program) (*mir.Program, error) {
	if p == nil {
		return nil, fmt.Errorf("backend pipeline: nil driver")
	}
	if prog == nil {
		return nil, fmt.Errorf("backend pipeline: nil MIR program before call lowering")
	}
	if p.Target == nil {
		return nil, fmt.Errorf("backend pipeline: nil target before call lowering")
	}
	return lower.LowerCallsInProgram(prog, p.Target)
}

// SwitchLowering rewrites each *mir.SwitchInstr into a compare-chain
// (CompareInstr + CondBrInstr sequence, new blocks added) or a single
// MachineTerm{Op:"switch_table"} for dense jump-table cases.
func (p *PipelineDriver) SwitchLowering(prog *mir.Program) (*mir.Program, error) {
	if p == nil {
		return nil, fmt.Errorf("backend pipeline: nil driver")
	}
	if prog == nil {
		return nil, fmt.Errorf("backend pipeline: nil MIR program before switch lowering")
	}
	if p.Target == nil {
		return nil, fmt.Errorf("backend pipeline: nil target before switch lowering")
	}
	return lower.LowerSwitchesInProgram(prog, p.Target)
}

// PhiRemoval removes phi nodes from the MIR program after register allocation.
// At this point, phi arms have been renamed to physical registers by regalloc,
// and RemovePhisInProgram will replace phis with explicit move sequences.
func (p *PipelineDriver) PhiRemoval(prog *mir.Program) (*mir.Program, error) {
	if p == nil {
		return nil, fmt.Errorf("backend pipeline: nil driver")
	}
	if prog == nil {
		return nil, fmt.Errorf("backend pipeline: nil MIR program before phi removal")
	}
	if p.Target == nil {
		return nil, fmt.Errorf("backend pipeline: nil target before phi removal")
	}
	return lower.RemovePhisInProgram(prog, p.Target)
}

// RegisterAllocation ...
func (p *PipelineDriver) RegisterAllocation(prog *mir.Program) (*mir.Program, error) {
	if p == nil {
		return nil, fmt.Errorf("backend pipeline: nil driver")
	}
	if prog == nil {
		return nil, fmt.Errorf("backend pipeline: nil MIR program before register allocation")
	}

	return regalloc.Run(prog, p.Target)
}

// PrologueEpilogue emits prologue and epilogue instructions based on frame layout.
// This runs after register allocation when frame information is available.
func (p *PipelineDriver) PrologueEpilogue(prog *mir.Program) (*mir.Program, error) {
	if p == nil {
		return nil, fmt.Errorf("backend pipeline: nil driver")
	}
	if prog == nil {
		return nil, fmt.Errorf("backend pipeline: nil MIR program before prologue/epilogue")
	}
	if p.Target == nil {
		return nil, fmt.Errorf("backend pipeline: nil target before prologue/epilogue")
	}

	return prog, legalize.EmitPrologueEpilogue(prog, p.Target)
}

func (p *PipelineDriver) passThrough(stage string, prog *mir.Program) (*mir.Program, error) {
	if p == nil {
		return nil, fmt.Errorf("backend pipeline: nil driver")
	}
	if prog == nil {
		return nil, fmt.Errorf("backend pipeline: nil MIR program before %s", stage)
	}
	return prog, nil
}

func (p *PipelineDriver) stageNames() []string {
	if p == nil || len(p.Stages) == 0 {
		return append([]string(nil), DefaultStageOrder...)
	}
	return append([]string(nil), p.Stages...)
}

func (p *PipelineDriver) loadSelector() (*selector.Selector, error) {
	if p == nil || p.Target == nil {
		return nil, nil
	}

	_, file, _, ok := runtime.Caller(0)
	if !ok {
		return nil, fmt.Errorf("backend pipeline: unable to locate selector descriptors")
	}

	path := filepath.Join(filepath.Dir(file), "select", "desc", p.Target.Name()+".td")
	sel, err := selector.ParseSelectorFile(path)
	if err != nil {
		return nil, fmt.Errorf("backend pipeline: load selector for %s: %w", p.Target.Name(), err)
	}

	return sel, nil
}
