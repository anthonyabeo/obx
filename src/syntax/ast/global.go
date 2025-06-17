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
	Name []string
}{
	// Predeclared proper procedures
	Assert_: {[]string{"assert", "ASSERT"}},
	Bytes_:  {[]string{"bytes", "BYTES"}},
	Dec_:    {[]string{"dec", "DEC"}},
	Excl_:   {[]string{"excl", "EXCL"}},
	Halt_:   {[]string{"halt", "HALT"}},
	Inc_:    {[]string{"inc", "INC"}},
	Incl_:   {[]string{"incl", "INCL"}},
	New_:    {[]string{"new", "NEW"}},
	Number_: {[]string{"number", "NUMBER"}},
	PCall_:  {[]string{"pcall", "PCALL"}},
	Raise_:  {[]string{"raise", "RAISE"}},

	// Deprecated predeclared proper procedures for backward compatibility
	Copy_: {[]string{"copy", "COPY"}},
	Pack_: {[]string{"pack", "PACK"}},
	UnPk_: {[]string{"unpk", "UNPK"}},

	Abs_:     {[]string{"abs", "ABS"}},
	Cap_:     {[]string{"cap", "CAP"}},
	BitAnd_:  {[]string{"bitand", "BITAND"}},
	BitAsr_:  {[]string{"bitasr", "BITASR"}},
	BitNot_:  {[]string{"bitnot", "BITNOT"}},
	BitOr_:   {[]string{"bitor", "BITOR"}},
	Bits_:    {[]string{"bits", "BITS"}},
	BitShl_:  {[]string{"bitshl", "BITSHL"}},
	BitShr_:  {[]string{"bitshr", "BITSHR"}},
	BitXor_:  {[]string{"bitxor", "BITXOR"}},
	Cast_:    {[]string{"cast", "CAST"}},
	Chr_:     {[]string{"chr", "CHR"}},
	Default_: {[]string{"default", "DEFAULT"}},
	Floor_:   {[]string{"floor", "FLOOR"}},
	Flt_:     {[]string{"flt", "FLT"}},
	LdCmd_:   {[]string{"ldcmd", "LDCMD"}},
	LdMod_:   {[]string{"ldmod", "LDMOD"}},
	Len_:     {[]string{"len", "LEN"}},
	Long_:    {[]string{"long", "LONG"}},
	Max_:     {[]string{"max", "MAX"}},
	Min_:     {[]string{"min", "MIN"}},
	Odd_:     {[]string{"odd", "ODD"}},
	Ord_:     {[]string{"ord", "ORD"}},
	Short_:   {[]string{"short", "SHORT"}},
	Size_:    {[]string{"size", "SIZE"}},
	StrLen_:  {[]string{"strlen", "STRLEN"}},
	WChr_:    {[]string{"wchr", "WCHR"}},

	ASh_:    {[]string{"ash", "ASH"}},
	ASr_:    {[]string{"asr", "ASR"}},
	Entier_: {[]string{"entier", "ENTIER"}},
	Lsl_:    {[]string{"lsl", "LSL"}},
	Ror_:    {[]string{"ror", "ROR"}},
}

func defPredeclaredProcedures() {
	for i := range PredeclaredProcedures {
		id := PreDeclFuncProc(i)

		for _, name := range PredeclaredProcedures[id].Name {
			GlobalEnviron.Insert(NewProcedureSymbol(name, Predeclared, nil, nil))
		}
	}
}

func init() {
	GlobalEnviron = NewEnvironment(nil, "global")

	defPredeclaredProcedures()
}
