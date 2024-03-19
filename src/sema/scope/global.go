package scope

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type exprKind int

const (
	stmt exprKind = iota
	expr
)

var offset = 0
var Global *Scope

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
	Assertn_
	Bytes_
	Dec_
	Decn_
	Excl_
	Halt_
	Inc_
	Incn_
	Incl_
	New_
	Newn_
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
	LenN_
	Long_
	Max_
	MaxN_
	Min_
	MinN_
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
	Name     string
	Nargs    int
	Variadic bool
	Kind     exprKind
}{
	// Predeclared proper procedures
	Assert_:  {"assert", 1, false, stmt},
	Assertn_: {"assert", 2, false, stmt},
	Bytes_:   {"bytes", 2, false, stmt},
	Dec_:     {"dec", 1, false, stmt},
	Decn_:    {"dec", 2, false, stmt},
	Excl_:    {"excl", 2, false, stmt},
	Halt_:    {"halt", 1, false, stmt},
	Inc_:     {"inc", 1, false, stmt},
	Incn_:    {"inc", 2, false, stmt},
	Incl_:    {"incl", 2, false, stmt},
	New_:     {"new", 1, false, stmt},
	Newn_:    {"newn", 2, true, stmt},
	Number_:  {"number", 2, false, stmt},
	PCall_:   {"pcall", 3, true, stmt},
	Raise_:   {"raise", 1, false, stmt},

	// Deprecated predeclared proper procedures for backward compatibility
	Copy_: {"copy", 2, false, stmt},
	Pack_: {"pack", 2, false, stmt},
	UnPk_: {"unpk", 2, false, stmt},

	Abs_:     {"abs", 1, false, expr},
	Cap_:     {"cap", 1, false, expr},
	BitAnd_:  {"bitand", 2, false, expr},
	BitAsr_:  {"bitasr", 2, false, expr},
	BitNot_:  {"bitnot", 1, false, expr},
	BitOr_:   {"bitor", 2, false, expr},
	Bits_:    {"bits", 1, false, expr},
	BitShl_:  {"bitshl", 2, false, expr},
	BitShr_:  {"bitshr", 2, false, expr},
	BitXor_:  {"bitxor", 2, false, expr},
	Cast_:    {"cast", 2, false, expr},
	Chr_:     {"chr", 1, false, expr},
	Default_: {"default", 1, false, expr},
	Floor_:   {"floor", 1, false, expr},
	Flt_:     {"flt", 1, false, expr},
	LdCmd_:   {"ldcmd", 2, false, expr},
	LdMod_:   {"ldmod", 1, false, expr},
	Len_:     {"len", 1, false, expr},
	LenN_:    {"len", 2, false, expr},
	Long_:    {"long", 1, false, expr},
	Max_:     {"max", 1, false, expr},
	MaxN_:    {"max", 2, false, expr},
	Min_:     {"min", 1, false, expr},
	MinN_:    {"min", 2, false, expr},
	Odd_:     {"odd", 1, false, expr},
	Ord_:     {"ord", 1, false, expr},
	Short_:   {"short", 1, false, expr},
	Size_:    {"size", 1, false, expr},
	StrLen_:  {"strlen", 1, false, expr},
	WChr_:    {"wchr", 1, false, expr},

	// Deprecated predeclared functions for backward compatibility
	ASh_:    {"ash", 2, false, expr},
	ASr_:    {"asr", 2, false, expr},
	Entier_: {"entier", 1, false, expr},
	Lsl_:    {"lsl", 2, false, expr},
	Ror_:    {"ror", 2, false, expr},
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
