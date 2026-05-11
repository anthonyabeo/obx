package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type legalizationStage struct{}

func (s *legalizationStage) Name() string                   { return backend.DefaultStageOrder[1] }
func (s *legalizationStage) Enabled(tgt target.Target) bool { return tgt != nil }
func (s *legalizationStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	return p.Legalization(prog)
}

func init() {
	backend.RegisterStage(backend.DefaultStageOrder[1], func() backend.Stage { return &legalizationStage{} })
}
