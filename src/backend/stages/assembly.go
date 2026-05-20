package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type assemblyStage struct{}

func (s *assemblyStage) Name() string                   { return backend.DefaultStageOrder[7] }
func (s *assemblyStage) Enabled(tgt target.Target) bool { return tgt != nil }
func (s *assemblyStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	if p == nil || p.Assemble == nil {
		return prog, nil
	}
	return prog, p.Assemble(prog)
}

func init() {
	backend.RegisterStage(backend.DefaultStageOrder[7], func() backend.Stage { return &assemblyStage{} })
}
