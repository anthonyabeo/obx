package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/lower"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type switchLoweringStage struct{}

func (s *switchLoweringStage) Name() string                   { return backend.DefaultStageOrder[1] }
func (s *switchLoweringStage) Enabled(tgt target.Target) bool { return tgt != nil }
func (s *switchLoweringStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	return lower.LowerSwitchesInProgram(prog, p.Target)
}

func init() {
	backend.RegisterStage(backend.DefaultStageOrder[1], func() backend.Stage { return &switchLoweringStage{} })
}

