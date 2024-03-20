package scope

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var offset = 0
var Global Scope

var Typ = []*types.Basic{
	types.Invalid: types.NewBasicType(types.Invalid, 0, "invalid type"),

	types.Bool:   types.NewBasicType(types.Bool, types.IsBoolean, "bool"),
	types.Byte:   types.NewBasicType(types.Byte, types.IsInteger, "byte"),
	types.Int8:   types.NewBasicType(types.Int8, types.IsInteger, "int8"),
	types.Int16:  types.NewBasicType(types.Int16, types.IsInteger, "int16"),
	types.Int32:  types.NewBasicType(types.Int32, types.IsInteger, "int32"),
	types.Int64:  types.NewBasicType(types.Int64, types.IsInteger, "int64"),
	types.Real:   types.NewBasicType(types.Real, types.IsReal, "real"),
	types.LReal:  types.NewBasicType(types.LReal, types.IsReal, "longreal"),
	types.Char:   types.NewBasicType(types.Char, types.IsChar, "char"),
	types.WChar:  types.NewBasicType(types.WChar, types.IsWChar, "wchar"),
	types.Set:    types.NewBasicType(types.Set, types.IsSet, "set"),
	types.String: types.NewBasicType(types.String, types.IsString, "string"),
	types.Nil:    types.NewBasicType(types.Nil, types.IsNil, "nil"),
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
	Bytes_
	Dec_
	Excl_
	Halt_
	Inc_
	Incl_
	New_
	Number_
	PCall_
	Raise_
	Copy_
	Pack_
	UnPk_

	// Predeclared function procedures
	Abs_
	Cap_
	BitAnd_
	BitAsr_
	BitNot_
	BitOr_
	Bits_
	BitShl_
	BitShr_
	BitXor_
	Cast_
	Chr_
	Default_
	Floor_
	Flt_
	LdCmd_
	LdMod_
	Len_
	Long_
	Max_
	Min_
	Odd_
	Ord_
	Short_
	Size_
	StrLen_
	WChr_

	// Deprecated predeclared functions for backward compatibility
	ASh_
	ASr_
	Entier_
	Lsl_
	Ror_
)

var PredeclaredProcedures = [...]struct {
	Name string
}{
	// Predeclared proper procedures
	Assert_: {"assert"},
	Bytes_:  {"bytes"},
	Dec_:    {"dec"},
	Excl_:   {"excl"},
	Halt_:   {"halt"},
	Inc_:    {"inc"},
	Incl_:   {"incl"},
	New_:    {"new"},
	Number_: {"number"},
	PCall_:  {"pcall"},
	Raise_:  {"raise"},

	// Deprecated predeclared proper procedures for backward compatibility
	Copy_: {"copy"},
	Pack_: {"pack"},
	UnPk_: {"unpk"},

	Abs_:     {"abs"},
	Cap_:     {"cap"},
	BitAnd_:  {"bitand"},
	BitAsr_:  {"bitasr"},
	BitNot_:  {"bitnot"},
	BitOr_:   {"bitor"},
	Bits_:    {"bits"},
	BitShl_:  {"bitshl"},
	BitShr_:  {"bitshr"},
	BitXor_:  {"bitxor"},
	Cast_:    {"cast"},
	Chr_:     {"chr"},
	Default_: {"default"},
	Floor_:   {"floor"},
	Flt_:     {"flt"},
	LdCmd_:   {"ldcmd"},
	LdMod_:   {"ldmod"},
	Len_:     {"len"},
	Long_:    {"long"},
	Max_:     {"max"},
	Min_:     {"min"},
	Odd_:     {"odd"},
	Ord_:     {"ord"},
	Short_:   {"short"},
	Size_:    {"size"},
	StrLen_:  {"strlen"},
	WChr_:    {"wchr"},

	// Deprecated predeclared functions for backward compatibility
	ASh_:    {"ash"},
	ASr_:    {"asr"},
	Entier_: {"entier"},
	Lsl_:    {"lsl"},
	Ror_:    {"ror"},
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
