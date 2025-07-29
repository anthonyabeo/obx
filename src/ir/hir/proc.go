package hir

import "github.com/anthonyabeo/obx/src/types"

type Param struct {
	Name   string
	Kind   ParamKind // Value, Var, In
	Type   types.Type
	Size   int
	Offset int
}

type ParamKind int

const (
	ValueParam ParamKind = iota
	VarParam
	InParam
)
