package ir

type ValueSymbolTable map[string]Value

type Module struct {
	name string
	env  ValueSymbolTable
}

func NewModule(name string) *Module {
	return &Module{name, ValueSymbolTable{}}
}

func (m *Module) GetOrInsertFunction(name string, ty *FunctionType, link LinkageKind) *Function {
	if Func, found := m.env[name]; found {
		F, _ := Func.(*Function)
		return F
	}

	f := &Function{
		name,
		link,
		ty,
		BasicBlockListType{},
		m,
	}

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
