package ast

func NewEnv() *Environment {
	return &Environment{
		scope:   Global,
		modules: make(map[string]*LexicalScope),
	}
}

type Environment struct {
	scope   *LexicalScope
	modules map[string]*LexicalScope
}

func (e *Environment) PushScope() {
	e.scope = NewLexicalScope(e.scope, "")
}

func (e *Environment) PopScope() {
	e.scope = e.scope.parent
}

func (e *Environment) Define(sym Symbol) Symbol {
	return e.scope.Insert(sym)
}

func (e *Environment) Lookup(name string) Symbol {
	return e.scope.Lookup(name)
}

func (e *Environment) LookupQualified(moduleName, name string) Symbol {
	if mod := e.modules[moduleName]; mod != nil {
		return mod.Lookup(name)
	}
	return nil
}

func (e *Environment) CurrentScope() *LexicalScope {
	return e.scope
}

func (e *Environment) SetCurrentScope(scope *LexicalScope) {
	e.scope = scope
}

func (e *Environment) AddModuleScope(name string, scope *LexicalScope) {
	e.modules[name] = scope
}

func (e *Environment) ModuleScope(name string) *LexicalScope {
	return e.modules[name]
}
