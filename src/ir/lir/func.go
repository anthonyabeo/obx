package lir

import "github.com/anthonyabeo/obx/src/ir/mir"

// Function : A procedure/function in LIR.
type Function struct {
	Name         string   // e.g. "main", "Insert"
	Ret          mir.Type // void if procedure is a command
	Params       []*Param // input registers (may include return reg)
	Locals       []*Local // all locals (excluding params); for allocation info
	IsExported   bool     // for modules with export rules
	Instructions []Inst
}

type Param struct {
	Name string
	Type Type
	Kind string // "value", "VAR", "IN"
}

type Local struct {
	Name string
	Type Type
}
