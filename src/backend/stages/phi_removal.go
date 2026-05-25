package stages

import (
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/lower"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type phiRemovalStage struct{}

func (prs *phiRemovalStage) Name() string                   { return "phi-removal" }
func (prs *phiRemovalStage) Enabled(tgt target.Target) bool { return tgt != nil }

// Run executes phi removal. Note: this stage is NOT registered in DefaultStageOrder.
// Instead, phi removal runs as a post-BuildPlans callback in driver.Run().
func (prs *phiRemovalStage) Run(p *backend.PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	return lower.RemovePhisInProgram(prog, p.Target)
}

// NOTE: This stage is NOT registered. Phi removal runs post-BuildPlans via driver.Run() instead.
