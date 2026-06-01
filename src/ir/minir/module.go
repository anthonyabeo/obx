package minir

// Module is the top-level container for a single translation unit.
// It mirrors LLVM's Module: it aggregates global variables, read-only
// constants, external function declarations, and function definitions,
// all indexed by a unified symbol table.
type Module struct {
	Name      string
	IsEntry   bool
	DLLName   string         // non-empty for DEFINITION modules: the external library name (e.g. "libc")
	Globals   []*GlobalVar    // mutable module-scope variables
	Constants []*GlobalConst  // read-only module-scope constants
	Externals []*ExternalFunc // imported / FFI function declarations
	Functions []*Function     // function definitions (with bodies)
	SymTab    SymbolTable     // module-scope symbol table
}

// Program is a collection of minir Modules produced by lowering a
// desugar.Program. Each source module maps to exactly one minir.Module.
// This mirrors the relationship between desugar.Program → []*desugar.Module
// at the level of the minir IR.
type Program struct {
	Modules []*Module
}
