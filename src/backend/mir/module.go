package mir

// Program is the top-level container for backend MIR modules.
type Program struct {
	Modules []*Module
}

func NewProgram(mods ...*Module) *Program {
	p := &Program{Modules: make([]*Module, 0, len(mods))}
	for _, mod := range mods {
		p.AddModule(mod)
	}
	return p
}

func (p *Program) AddModule(mod *Module) {
	if p == nil || mod == nil {
		return
	}
	p.Modules = append(p.Modules, mod)
}

func (p *Program) ModuleByName(name string) *Module {
	if p == nil {
		return nil
	}
	for _, mod := range p.Modules {
		if mod != nil && mod.Name == name {
			return mod
		}
	}
	return nil
}

// Module groups functions and module-scope data.
type Module struct {
	Name      string
	Globals   []*GlobalDecl
	Externals []*ExternDecl
	Constants []*ConstDecl
	Functions []*Function

	globalsByName   map[string]*GlobalDecl
	externsByName   map[string]*ExternDecl
	constantsByName map[string]*ConstDecl
	functionsByName map[string]*Function
}

func NewModule(name string) *Module {
	return &Module{
		Name:            name,
		Globals:         make([]*GlobalDecl, 0),
		Externals:       make([]*ExternDecl, 0),
		Constants:       make([]*ConstDecl, 0),
		Functions:       make([]*Function, 0),
		globalsByName:   make(map[string]*GlobalDecl),
		externsByName:   make(map[string]*ExternDecl),
		constantsByName: make(map[string]*ConstDecl),
		functionsByName: make(map[string]*Function),
	}
}

func (m *Module) AddGlobal(g *GlobalDecl) {
	if m == nil || g == nil {
		return
	}
	m.Globals = append(m.Globals, g)
	m.globalsByName[g.Name] = g
}

func (m *Module) AddExtern(e *ExternDecl) {
	if m == nil || e == nil {
		return
	}
	m.Externals = append(m.Externals, e)
	m.externsByName[e.Name] = e
}

func (m *Module) AddConst(c *ConstDecl) {
	if m == nil || c == nil {
		return
	}
	m.Constants = append(m.Constants, c)
	m.constantsByName[c.Name] = c
}

func (m *Module) AddFunction(fn *Function) {
	if m == nil || fn == nil {
		return
	}
	m.Functions = append(m.Functions, fn)
	m.functionsByName[fn.Name] = fn
}

func (m *Module) GlobalByName(name string) *GlobalDecl {
	if m == nil {
		return nil
	}
	return m.globalsByName[name]
}

func (m *Module) ExternByName(name string) *ExternDecl {
	if m == nil {
		return nil
	}
	return m.externsByName[name]
}

func (m *Module) ConstByName(name string) *ConstDecl {
	if m == nil {
		return nil
	}
	return m.constantsByName[name]
}

func (m *Module) FunctionByName(name string) *Function {
	if m == nil {
		return nil
	}
	return m.functionsByName[name]
}

// GlobalDecl models a module-scope mutable global.
type GlobalDecl struct {
	Name    string
	Type    *Type
	Linkage Linkage
	Init    Operand
}

func NewGlobalDecl(name string, ty *Type, linkage Linkage, init Operand) *GlobalDecl {
	return &GlobalDecl{Name: name, Type: ty, Linkage: linkage, Init: init}
}

// ExternDecl models an imported / foreign function or symbol.
type ExternDecl struct {
	Name     string
	Type     *Type
	DLLName  string
	Variadic bool
	CallConv string
	Linkage  Linkage
}

func NewExternDecl(name string, ty *Type) *ExternDecl {
	return &ExternDecl{Name: name, Type: ty, Linkage: ExternalLinkage}
}

// ConstDecl models a module-scope read-only constant.
type ConstDecl struct {
	Name    string
	Type    *Type
	Value   Operand
	Linkage Linkage
}

func NewConstDecl(name string, ty *Type, value Operand, linkage Linkage) *ConstDecl {
	return &ConstDecl{Name: name, Type: ty, Value: value, Linkage: linkage}
}
