package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type instructionSchedulingStage struct{}

func (s *instructionSchedulingStage) Name() string { return backend.DefaultStageOrder[2] }
func (s *instructionSchedulingStage) Enabled(tgt target.Target) bool {
	return tgt != nil && tgt.ABIInfo().WordSize > 0
}
func (s *instructionSchedulingStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	return p.InstructionScheduling(prog)
}

func init() {
	backend.RegisterStage(backend.DefaultStageOrder[2], func() backend.Stage { return &instructionSchedulingStage{} })
}
