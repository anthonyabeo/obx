package ir

import "container/list"

type ValueSymbolTable map[string]Value

type Module struct {
	name  string
	funcs []*Function
	env   ValueSymbolTable
}

func NewModule(name string) *Module {
	return &Module{name, make([]*Function, 0), ValueSymbolTable{}}
}

func (m *Module) GetFunctionList() []*Function {
	return m.funcs
}

func (m *Module) GetOrInsertFunction(name string, ty *FunctionType, link LinkageKind) *Function {
	if Func, found := m.env[name]; found {
		F, _ := Func.(*Function)
		return F
	}

	uses := list.New()
	uses.Init()

	f := &Function{
		name,
		link,
		ty,
		NewCFG(),
		m,
		uses,
	}

	m.funcs = append(m.funcs, f)
	m.env[name] = f

	return f
}

func (m *Module) GetFunction(name string) (F *Function) {
	// TODO remove hardcoded implementation

	switch name {
	case "main":
		F = m.env[name].(*Function)

	default:
		F = CreateFunction(
			CreateFunctionType([]Type{Int1Type}, VoidType, false),
			External,
			"assert",
			m,
		)

	}

	return
}
