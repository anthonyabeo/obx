package backend

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/backend/lower"
	"github.com/anthonyabeo/obx/src/backend/mir"
	selector "github.com/anthonyabeo/obx/src/backend/select"
	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/minir"
)

// PipelineDriver orchestrates the backend pipeline stages.
//
// Current implementation:
//  1. Lower(minir -> backend/mir)
//  2. InstructionSelection (stub)
//  3. Legalization (stub)
//  4. InstructionScheduling (stub)
//  5. RegisterAllocation (stub)
//  6. Assemble (optional callback)
//  7. Link (optional callback)
//  8. BuildPlans(target lowering helpers)
type PipelineDriver struct {
	Target   target.Target
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

	mprog, err := lower.LowerProgram(prog)
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

// InstructionSelection is currently a stub.
func (p *PipelineDriver) InstructionSelection(prog *mir.Program) (*mir.Program, error) {
	return p.passThrough("instruction selection", prog)
}

// Legalization is currently a stub.
func (p *PipelineDriver) Legalization(prog *mir.Program) (*mir.Program, error) {
	return p.passThrough("legalization", prog)
}

// InstructionScheduling is currently a stub.
func (p *PipelineDriver) InstructionScheduling(prog *mir.Program) (*mir.Program, error) {
	return p.passThrough("instruction scheduling", prog)
}

// RegisterAllocation is currently a stub.
func (p *PipelineDriver) RegisterAllocation(prog *mir.Program) (*mir.Program, error) {
	return p.passThrough("register allocation", prog)
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
