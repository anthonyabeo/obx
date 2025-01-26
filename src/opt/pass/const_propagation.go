package pass

import (
	"github.com/anthonyabeo/obx/src/meer"
)

type ConstantPropagation struct {
	Nom string
}

func (c ConstantPropagation) Name() string { return c.Nom }

func (c ConstantPropagation) Run(program *meer.Program) {
	//TODO implement me
	panic("implement me")
}
