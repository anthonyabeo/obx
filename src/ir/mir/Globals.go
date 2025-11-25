package mir

var GlobalEnv = &SymbolTable{
	Parent:  nil,
	Symbols: make(map[string]Value),
}

func init() {
	// Initialize built-in functions
	GlobalEnv.Define("cap", &Function{
		FnName:    "cap",
		Result:    UInt8Type,
		Exported:  true,
		Variadic:  false,
		IsBuiltin: true,
		Params:    []Value{&Param{Ident: "x", Typ: UInt8Type}},
	})

	GlobalEnv.Define("printf", &Function{
		FnName:    "printf",
		Result:    Int32Type,
		Exported:  true,
		Variadic:  true,
		IsBuiltin: true,
		Params:    []Value{&Param{Ident: "format", Typ: ArrayType{Len: -1, Elem: UInt8Type}}},
	})

	GlobalEnv.Define("new", &Function{
		FnName:    "new",
		Result:    PointerTo(UInt8Type),
		Exported:  true,
		Variadic:  false,
		IsBuiltin: true,
		Params:    []Value{&Param{Ident: "size", Typ: Int64Type}},
	})
}
