package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/lower"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type callLoweringStage struct{}

func (s *callLoweringStage) Name() string                   { return backend.DefaultStageOrder[0] }
func (s *callLoweringStage) Enabled(tgt target.Target) bool { return tgt != nil }
func (s *callLoweringStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	return lower.LowerCallsInProgram(prog, p.Target)
}

func init() {
	backend.RegisterStage(backend.DefaultStageOrder[0], func() backend.Stage { return &callLoweringStage{} })
}

