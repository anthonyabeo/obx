package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type prologueEpilogueStage struct{}

func (s *prologueEpilogueStage) Name() string                   { return backend.DefaultStageOrder[6] }
func (s *prologueEpilogueStage) Enabled(tgt target.Target) bool { return tgt != nil }
func (s *prologueEpilogueStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	if p == nil {
		return prog, nil
	}
	return p.PrologueEpilogue(prog)
}

func init() {
	backend.RegisterStage(backend.DefaultStageOrder[6], func() backend.Stage { return &prologueEpilogueStage{} })
}

