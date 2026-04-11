package obxir

// GlobalEnv is the top-level symbol table that pre-populates all predeclared
// functions and procedures from OberonX.
var GlobalEnv = &SymbolTable{
	Parent:  nil,
	Symbols: make(map[string]Value),
}

// simpleBuiltin creates a minimal builtin Function entry for GlobalEnv lookup.
func simpleBuiltin(name string, ret Type) *Function {
	return &Function{
		FnName:    name,
		Result:    ret,
		Exported:  true,
		Variadic:  false,
		IsBuiltin: true,
		Params:    []Value{},
	}
}

func init() {
	// printf needs an explicit Variadic + Params signature.
	GlobalEnv.Define("printf", &Function{
		FnName:    "printf",
		Result:    Int32Type,
		Exported:  true,
		Variadic:  true,
		IsBuiltin: true,
		Params:    []Value{&Param{Ident: "format", Typ: &ArrayType{Len: -1, Elem: UInt8Type}}},
	})

	// Predeclared functions
	for _, name := range []string{
		"abs", "cap", "bitand", "bitasr", "bitor", "bitxor", "bitnot",
		"bits", "bitshl", "bitshr", "cast", "chr", "default", "floor",
		"flt", "ldcmd", "ldmod", "len", "long", "max", "min", "odd",
		"ord", "short", "size", "strlen", "wchar", "ash", "asr",
		"entier", "lsl", "ror",
	} {
		GlobalEnv.Define(name, simpleBuiltin(name, Void))
	}

	// Predeclared procedures
	for _, name := range []string{
		"assert", "bytes", "dec", "excl", "halt", "inc", "incl",
		"new", "number", "pcall", "raise", "copy", "pack", "unpk",
	} {
		GlobalEnv.Define(name, simpleBuiltin(name, Void))
	}
}

