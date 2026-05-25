package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/lower"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type phiRemovalStage struct{}

func (prs *phiRemovalStage) Name() string                   { return backend.DefaultStageOrder[7] }
func (prs *phiRemovalStage) Enabled(tgt target.Target) bool { return tgt != nil }
func (prs *phiRemovalStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	return lower.RemovePhisInProgram(prog, p.Target)
}

func init() {
	backend.RegisterStage(backend.DefaultStageOrder[7], func() backend.Stage { return &phiRemovalStage{} })
}
