package pass

import "github.com/anthonyabeo/obx/src/translate/tacil"

type ConstantPropagation struct {
	Nom string
}

func (c ConstantPropagation) Name() string { return c.Nom }

func (c ConstantPropagation) Run(program *tacil.Program) {
	//TODO implement me
	panic("implement me")
}
