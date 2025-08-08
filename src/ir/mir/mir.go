package mir

// Program ...
type Program struct {
	Modules []*Module
}

// Module is the high level compilation unit
type Module struct {
	Name    string
	IsEntry bool
	Globals map[string]*Global
	Funcs   []*Function
}

type Function struct {
	Name   string
	Result Type
	Params map[string]Value // formal parameters
	Locals []Value          // local variable, constant, procedure declarations

	Blocks map[int]*Block // basic blocks (in order, but can build CFG)
	Entry  *Block         // pointer to entry block

	TempMap map[string]*Temp // optional: for SSA or debug
	SSAInfo *SSAInfo
	DomTree *DominatorTree

	Constants map[string]*Const
}

func NewFunction(name string, ret Type) *Function {
	return &Function{
		Name:   name,
		Result: ret,
		Blocks: make(map[int]*Block),
		Params: make(map[string]Value),
	}
}
