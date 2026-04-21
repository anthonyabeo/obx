package obxir

import "strings"

// GlobalEnv is the top-level symbol table that pre-populates all predeclared
// functions and procedures from OberonX.
var GlobalEnv = &SymbolTable{
	Name:    "Global",
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

type Environment struct {
	scope   *SymbolTable
	modules map[string]*SymbolTable
}

func CreateEnv() *Environment {
	env := &Environment{
		scope:   GlobalEnv,
		modules: make(map[string]*SymbolTable),
	}

	return env
}
func (e *Environment) PushScope(scopeName string) {
	scope := NewSymbolTable(scopeName, e.scope)
	if e.modules[scopeName] == nil {
		e.modules[scopeName] = scope
	}
	e.scope = scope
}
func (e *Environment) PopScope()                   { e.scope = e.scope.Parent }
func (e *Environment) Define(name string, v Value) { e.scope.Define(name, v) }
func (e *Environment) Lookup(name string) Value {
	if v := e.scope.Lookup(name); v != nil {
		return v
	}

	return nil
}
func (e *Environment) LookupQualified(moduleName, name string) Value {
	if mod := e.modules[moduleName]; mod != nil {
		if v := mod.Lookup(name); v != nil {
			return v
		}
	}
	return nil
}
func (e *Environment) CurrentScope() *SymbolTable {
	return e.scope
}
func (e *Environment) SetCurrentScope(scope *SymbolTable)             { e.scope = scope }
func (e *Environment) AddModuleScope(name string, scope *SymbolTable) { e.modules[name] = scope }
func (e *Environment) ModuleScope(name string) *SymbolTable {
	return e.modules[name]
}
func (e *Environment) Find(mangledName string) Value {
	if mangledName == "" {
		return nil
	}

	parts := strings.Split(mangledName, "$")
	// If not a qualified (dollar-separated) name, fall back to current scope lookup.
	if len(parts) == 1 {
		return e.Lookup(mangledName)
	}

	// Start lookup at the named module scope.
	modName := parts[0]
	mod := e.ModuleScope(modName)
	if mod == nil {
		return nil
	}

	cur := mod
	var v Value

	// Walk components after the module name, descending into nested
	// symbol tables when we encounter a *Function (which carries an Env).
	for i := 1; i < len(parts); i++ {
		name := parts[i]

		// Prefer an exact entry in the current table, otherwise use Lookup
		// which searches parent scopes as a fallback.
		if val, ok := cur.Symbols[name]; ok {
			v = val
		} else {
			v = cur.Lookup(name)
		}

		if v == nil {
			return nil
		}

		// If this is the final component, return the found value.
		if i == len(parts)-1 {
			return v
		}

		// Otherwise we must descend: only *Function carries an Env currently.
		if fn, ok := v.(*Function); ok && fn.Env != nil {
			cur = fn.Env
			continue
		}

		// Cannot descend further but more components remain.
		return nil
	}

	return nil
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
