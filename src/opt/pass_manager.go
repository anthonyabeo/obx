package opt

import (
	"github.com/anthonyabeo/obx/src/opt/analysis"
	"github.com/anthonyabeo/obx/src/opt/pass"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type PassManager struct {
	passes   map[string]pass.Pass
	analyses map[string]analysis.Analysis
	alias    map[string][]string
}

func (pm *PassManager) RegisterPass(pass pass.Pass) {

}

func (pm *PassManager) RegisterAlias(name string, passes []string) {
	if _, ok := pm.alias[name]; ok {
		return
	}

	for _, pss := range passes {
		if _, ok := pm.alias[pss]; ok {
			pm.alias[name] = append(pm.alias[name], pm.alias[pss]...)
		}

		if _, ok := pm.passes[pss]; ok {
			pm.alias[name] = append(pm.alias[name], pss)
		}
	}
}

func (pm *PassManager) CreatePlan() {

}

func (pm *PassManager) ExecPlan() {

}

func (pm *PassManager) Init() {
	pm.RegisterAlias("none", nil)
	pm.RegisterAlias("default", []string{"PromoteMemToReg", "GVN"})
}

func (pm *PassManager) Run(f *ir.Function) {
	entry := f.EntryBlock()
	for _, p := range pm.passes {
		p.Init(entry)
		p.Run()
	}
}
