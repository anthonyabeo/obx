package opt

import (
	"github.com/anthonyabeo/obx/src/opt/analysis"
	"github.com/anthonyabeo/obx/src/opt/pass"
)

type PassManager struct {
	passes   map[string]pass.Pass
	analyses map[string]analysis.Analysis
}

func (p *PassManager) RegisterPass(pass pass.Pass) {

}

func (p *PassManager) CreatePlan() {

}

func (p *PassManager) ExecPlan() {

}
