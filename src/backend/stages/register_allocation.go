package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type registerAllocationStage struct{}

func (s *registerAllocationStage) Name() string { return backend.DefaultStageOrder[3] }
func (s *registerAllocationStage) Enabled(tgt target.Target) bool {
	return tgt != nil
}
func (s *registerAllocationStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	return p.RegisterAllocation(prog)
}

func init() {
	backend.RegisterStage(backend.DefaultStageOrder[3], func() backend.Stage { return &registerAllocationStage{} })
}
