package pass

import "github.com/anthonyabeo/obx/src/translate/ir"

type Pass interface {
	Init(block *ir.BasicBlock)
	Run()
}
