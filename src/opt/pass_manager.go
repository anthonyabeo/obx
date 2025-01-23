package opt

import (
	"github.com/anthonyabeo/obx/src/meer"
	"github.com/anthonyabeo/obx/src/opt/pass"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

type Pass interface {
	Name() string
	Run(*meer.Program /*, *tacil.SymbolTable*/)
}

type PassManager struct {
	passes  []Pass
	set     map[string]bool
	symbols *tacil.SymbolTable
}

func NewPassManager( /*table *tacil.SymbolTable*/ ) *PassManager {
	return &PassManager{set: map[string]bool{} /*symbols: table*/}
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

func (pm *PassManager) Run(program *meer.Program) {
	for len(pm.passes) > 0 {
		p := pm.dequeue()
		p.Run(program /*, pm.symbols*/)
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
		"ssa": &pass.SSA{Nom: "ssa"},
		//"dce":        &pass.DeadCodeElimination{Nom: "dce"},
		//"const_prop": &pass.ConstantPropagation{},
	}
}
