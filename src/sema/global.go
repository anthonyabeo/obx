package sema

import (
	"github.com/anthonyabeo/obx/src/sema/types"
)

var Global *Scope

var Typ = []*types.Basic{
	types.Invalid: types.NewBasicType(types.Invalid, 0, "invalid type"),

	types.Bool:  types.NewBasicType(types.Bool, types.IsBoolean, "bool"),
	types.Int8:  types.NewBasicType(types.Int8, types.IsInteger, "int8"),
	types.Int16: types.NewBasicType(types.Int16, types.IsInteger, "int16"),
	types.Int32: types.NewBasicType(types.Int32, types.IsInteger, "int32"),
	types.Int64: types.NewBasicType(types.Int64, types.IsInteger, "int64"),
}

func defPredeclaredTypes() {
	for _, t := range Typ {
		Global.Insert(NewTypeName(nil, t.Name(), t))
	}
}

// A builtinId is the id of a builtin function.
type builtinId int

const (
	_Assert builtinId = iota
)

var predeclaredProcedures = [...]struct {
	name  string
	nargs int
}{
	_Assert: {"append", 1},
}

func defPredeclaredProcedures() {
	for i := range predeclaredProcedures {
		id := builtinId(i)

		Global.Insert(newBuiltin(id))
	}
}

func init() {
	Global = NewScope(nil, "global")

	defPredeclaredTypes()
	defPredeclaredProcedures()
}
