package mir

// Param is a function formal parameter.
type Param struct {
	Name string
	Type *Type
	Kind string
}

// Local is a function-local named storage location.
type Local struct {
	Name string
	Type *Type
}

// Function is a machine-IR function with explicit blocks and CFG.
type Function struct {
	Name     string
	Result   *Type
	Params   []*Param
	Locals   []*Local
	Blocks   []*Block
	Entry    *Block
	Exit     *Block
	IsLeaf   bool
	HasCalls bool

	blockByLabel map[string]*Block
}

func NewFunction(name string, result *Type) *Function {
	return &Function{
		Name:         name,
		Result:       result,
		Params:       make([]*Param, 0),
		Locals:       make([]*Local, 0),
		Blocks:       make([]*Block, 0),
		blockByLabel: make(map[string]*Block),
	}
}

func (fn *Function) AddParam(p *Param) {
	if fn == nil || p == nil {
		return
	}
	fn.Params = append(fn.Params, p)
}

func (fn *Function) AddLocal(l *Local) {
	if fn == nil || l == nil {
		return
	}
	fn.Locals = append(fn.Locals, l)
}

func (fn *Function) AddBlock(b *Block) {
	if fn == nil || b == nil {
		return
	}
	if _, exists := fn.blockByLabel[b.Label]; exists {
		return
	}
	fn.Blocks = append(fn.Blocks, b)
	fn.blockByLabel[b.Label] = b
}

func (fn *Function) BlockByLabel(label string) *Block {
	if fn == nil {
		return nil
	}
	return fn.blockByLabel[label]
}

func (fn *Function) SetEntry(b *Block) { fn.Entry = b }

func (fn *Function) SetExit(b *Block) { fn.Exit = b }
