package pass

import "github.com/anthonyabeo/obx/src/translate/ir"

type ConstantPropagation struct {
	Nom string
}

func (c ConstantPropagation) Name() string { return c.Nom }

func (c ConstantPropagation) Run(program *ir.Program) {
	//TODO implement me
	panic("implement me")
}
