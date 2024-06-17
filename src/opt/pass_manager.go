package opt

import (
	"github.com/anthonyabeo/obx/src/opt/pass"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type Pass interface {
	Name() string
	Run(program *ir.Program) bool
}

type PassManager struct {
	passes map[string]Pass
}

func NewPassManager() *PassManager {
	return &PassManager{passes: map[string]Pass{}}
}

func (pm *PassManager) AddPass(pass Pass) {
	if _, exits := pm.passes[pass.Name()]; !exits {
		pm.passes[pass.Name()] = pass
	}
}

func (pm *PassManager) Run(program *ir.Program) {
	for _, p := range pm.passes {
		p.Run(program)
	}
}

func (pm *PassManager) AddPasses(passNames ...string) {
	for _, name := range passNames {
		if p, exist := declPasses[name]; exist {
			pm.AddPass(p)
		} else {
			// return error unknown pass
		}
	}
}

// Declared Passes
// ---------------------------------------------------------------
var declPasses map[string]Pass

func init() {
	declPasses = map[string]Pass{
		"mem2reg":    &pass.Mem2Reg{},
		"dce":        &pass.DeadCodeElimination{},
		"const_prop": &pass.ConstantPropagation{},
	}
}
