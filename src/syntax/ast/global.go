package ast

var GlobalEnviron *Environment

// A PreDeclFuncProc is the id of a predeclared function or procedure.
type PreDeclFuncProc int

const (
	Assert_ PreDeclFuncProc = iota
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
		id := PreDeclFuncProc(i)

		GlobalEnviron.Insert(NewProcedureSymbol(PredeclaredProcedures[id].Name, Predeclared, nil))
	}
}

func init() {
	GlobalEnviron = NewEnvironment(nil, "global")

	defPredeclaredProcedures()
}
