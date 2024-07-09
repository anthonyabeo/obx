package opt

import (
	"github.com/anthonyabeo/obx/src/opt/pass"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

type Pass interface {
	Name() string
	Run(program *tacil.Program)
}

type PassManager struct {
	passes []Pass
	set    map[string]bool
}

func NewPassManager() *PassManager {
	return &PassManager{set: map[string]bool{}}
}

func (pm *PassManager) AddPass(pass Pass) {
	if _, exits := pm.set[pass.Name()]; !exits {
		pm.set[pass.Name()] = true
		pm.passes = append(pm.passes, pass)
	}
}

func (pm *PassManager) dequeue() Pass {
	p := pm.passes[0]
	pm.passes = pm.passes[1:]

	return p
}

func (pm *PassManager) Run(program *tacil.Program) {
	for len(pm.passes) > 0 {
		p := pm.dequeue()
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
		"mem2reg": &pass.Mem2Reg{Nom: "mem2reg"},
		//"dce":        &pass.DeadCodeElimination{Nom: "dce"},
		"const_prop": &pass.ConstantPropagation{},
	}
}
