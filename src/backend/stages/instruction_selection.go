package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type instructionSelectionStage struct{}

func (s *instructionSelectionStage) Name() string               { return backend.DefaultStageOrder[0] }
func (s *instructionSelectionStage) Enabled(target.Target) bool { return true }
func (s *instructionSelectionStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	return p.InstructionSelection(prog)
}

func init() {
	backend.RegisterStage(backend.DefaultStageOrder[0], func() backend.Stage { return &instructionSelectionStage{} })
}
