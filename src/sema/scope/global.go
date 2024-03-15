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
	_Assertn
	_Bytes
	_Dec
	_Decn
	_Excl
	_Halt
	Inc_
	_Incn
	_Incl
	_New
	_Newn
	_Number
	_PCall
	_Raise
	_Copy
	_Pack
	_UnPk

	// Predeclared function procedures
	_Abs
	_Cap
	_BitAnd
	_BitAsr
	_BitNot
	_BitOr
	_Bits
	_BitShl
	_BitShr
	_BitXor
	_Cast
	_Chr
	_Default
	_Floor
	_Flt
	_LdCmd
	_LdMod
	_Len
	_LenN
	_Long
	_Max
	_MaxN
	_Min
	_MinN
	_Odd
	_Ord
	_Short
	_Size
	_StrLen
	_WChr

	// Deprecated predeclared functions for backward compatibility
	_ASh
	_ASr
	_Entier
	_Lsl
	_Ror
)

var PredeclaredProcedures = [...]struct {
	Name     string
	Nargs    int
	Variadic bool
	Kind     exprKind
}{
	// Predeclared proper procedures
	Assert_:  {"assert", 1, false, stmt},
	_Assertn: {"assert", 2, false, stmt},
	_Bytes:   {"bytes", 2, false, stmt},
	_Dec:     {"dec", 1, false, stmt},
	_Decn:    {"dec", 2, false, stmt},
	_Excl:    {"excl", 2, false, stmt},
	_Halt:    {"halt", 1, false, stmt},
	Inc_:     {"inc", 1, false, stmt},
	_Incn:    {"inc", 2, false, stmt},
	_Incl:    {"incl", 2, false, stmt},
	_New:     {"new", 1, false, stmt},
	_Newn:    {"new", 2, true, stmt},
	_Number:  {"number", 2, false, stmt},
	_PCall:   {"pcall", 3, true, stmt},
	_Raise:   {"raise", 1, false, stmt},

	// Deprecated predeclared proper procedures for backward compatibility
	_Copy: {"copy", 2, false, stmt},
	_Pack: {"pack", 2, false, stmt},
	_UnPk: {"unpk", 2, false, stmt},

	_Abs:     {"abs", 1, false, expr},
	_Cap:     {"cap", 1, false, expr},
	_BitAnd:  {"bitand", 2, false, expr},
	_BitAsr:  {"bitasr", 2, false, expr},
	_BitNot:  {"bitnot", 1, false, expr},
	_BitOr:   {"bitor", 2, false, expr},
	_Bits:    {"bits", 1, false, expr},
	_BitShl:  {"bitshl", 2, false, expr},
	_BitShr:  {"bitshr", 2, false, expr},
	_BitXor:  {"bitxor", 2, false, expr},
	_Cast:    {"cast", 2, false, expr},
	_Chr:     {"chr", 1, false, expr},
	_Default: {"default", 1, false, expr},
	_Floor:   {"floor", 1, false, expr},
	_Flt:     {"flt", 1, false, expr},
	_LdCmd:   {"ldcmd", 2, false, expr},
	_LdMod:   {"ldmod", 1, false, expr},
	_Len:     {"len", 1, false, expr},
	_LenN:    {"len", 2, false, expr},
	_Long:    {"long", 1, false, expr},
	_Max:     {"max", 1, false, expr},
	_MaxN:    {"max", 2, false, expr},
	_Min:     {"min", 1, false, expr},
	_MinN:    {"min", 2, false, expr},
	_Odd:     {"odd", 1, false, expr},
	_Ord:     {"ord", 1, false, expr},
	_Short:   {"short", 1, false, expr},
	_Size:    {"size", 1, false, expr},
	_StrLen:  {"strlen", 1, false, expr},
	_WChr:    {"wchr", 1, false, expr},

	// Deprecated predeclared functions for backward compatibility
	_ASh:    {"ash", 2, false, expr},
	_ASr:    {"asr", 2, false, expr},
	_Entier: {"entier", 1, false, expr},
	_Lsl:    {"lsl", 2, false, expr},
	_Ror:    {"ror", 2, false, expr},
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
