package sema

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var Global *Scope

var Typ = []*types.Basic{
	types.Invalid: types.NewBasicType(types.Invalid, 0, "invalid type"),

	types.Bool:  types.NewBasicType(types.Bool, types.IsBoolean, "bool"),
	types.Int:   types.NewBasicType(types.Int, types.IsInteger|types.IsNumeric, "integer"),
	types.Int8:  types.NewBasicType(types.Int8, types.IsInteger|types.IsNumeric, "int8"),
	types.Int16: types.NewBasicType(types.Int16, types.IsInteger|types.IsNumeric, "int16"),
	types.Int32: types.NewBasicType(types.Int32, types.IsInteger|types.IsNumeric, "int32"),
	types.Int64: types.NewBasicType(types.Int64, types.IsInteger|types.IsNumeric, "int64"),
	types.LInt:  types.NewBasicType(types.LInt, types.IsInteger|types.IsNumeric, "longint"),
	types.SInt:  types.NewBasicType(types.SInt, types.IsInteger|types.IsNumeric, "shortint"),
	types.Byte:  types.NewBasicType(types.Byte, types.IsInteger|types.IsNumeric, "byte"),

	types.Real:  types.NewBasicType(types.Real, types.IsReal|types.IsNumeric, "real"),
	types.LReal: types.NewBasicType(types.LReal, types.IsReal|types.IsNumeric, "longreal"),
}

func defPredeclaredTypes() {
	for _, t := range Typ {
		Global.Insert(NewTypeName(nil, t.Name(), t, ast.IsPredeclared))
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
	_Assert: {"assert", 1},
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
