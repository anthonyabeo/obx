package scope

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var offset = 0
var Global *Scope

var Typ = []*types.Basic{
	types.Invalid: types.NewBasicType(types.Invalid, 0, "invalid type"),

	types.Bool:  types.NewBasicType(types.Bool, types.IsBoolean, "bool"),
	types.Byte:  types.NewBasicType(types.Byte, types.IsInteger, "byte"),
	types.Int8:  types.NewBasicType(types.Int8, types.IsInteger, "int8"),
	types.Int16: types.NewBasicType(types.Int16, types.IsInteger, "int16"),
	types.Int32: types.NewBasicType(types.Int32, types.IsInteger, "int32"),
	types.Int64: types.NewBasicType(types.Int64, types.IsInteger, "int64"),
	types.Real:  types.NewBasicType(types.Real, types.IsReal, "real"),
	types.LReal: types.NewBasicType(types.LReal, types.IsReal, "longreal"),
	types.Char:  types.NewBasicType(types.Char, types.IsChar, "char"),
	types.WChar: types.NewBasicType(types.WChar, types.IsWChar, "wchar"),
}

var AliasTypes = []*types.Basic{
	types.NewBasicType(types.Int, types.IsInteger, "integer"),
	types.NewBasicType(types.SInt, types.IsInteger, "shortint"),
	types.NewBasicType(types.LInt, types.IsInteger, "longtint"),
}

func defPredeclaredTypes() {
	for _, t := range Typ {
		Global.Insert(NewTypeName(nil, t.Name(), t, ast.Predeclared, offset))
	}

	for _, t := range AliasTypes {
		Global.Insert(NewTypeName(nil, t.Name(), t, ast.Predeclared, offset))
	}
}

// A BuiltinId is the id of a builtin function.
type BuiltinId int

const (
	Assert_ BuiltinId = iota
	Inc_
)

var PredeclaredProcedures = [...]struct {
	Name  string
	Nargs int
}{
	Assert_: {"assert", 1},
	Inc_:    {"inc", 1},
}

func defPredeclaredProcedures() {
	for i := range PredeclaredProcedures {
		id := BuiltinId(i)

		Global.Insert(NewBuiltin(id))
	}
}

func init() {
	Global = NewScope(nil, "global")

	defPredeclaredTypes()
	defPredeclaredProcedures()
}
