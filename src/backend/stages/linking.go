package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type linkingStage struct{}

func (s *linkingStage) Name() string                   { return backend.DefaultStageOrder[5] }
func (s *linkingStage) Enabled(tgt target.Target) bool { return tgt != nil }
func (s *linkingStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	if p == nil || p.Link == nil {
		return prog, nil
	}
	return prog, p.Link(prog)
}

func init() {
	backend.RegisterStage(backend.DefaultStageOrder[5], func() backend.Stage { return &linkingStage{} })
}
