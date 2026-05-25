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

// FrameLayout describes the stack frame assigned by register allocation.
// It is nil on a Function until the register-allocation stage runs.
type FrameLayout struct {
	// TotalSize is the total number of bytes allocated on the stack for this
	// frame, including allocas, saved registers, and spill slots.
	TotalSize int

	// SavedRegs lists callee-saved physical register names that this function
	// must preserve (i.e. those that are live across calls or clobbered).
	SavedRegs []string

	// AllocaSlots maps surviving alloca register names to their frame-relative
	// byte offsets (measured from the frame pointer or stack pointer base).
	AllocaSlots map[string]int

	// SpillSlots maps virtual-register names to their frame-relative byte
	// offsets (measured from the frame pointer or stack pointer base).
	SpillSlots map[string]int
}

// NewFrameLayout returns an empty FrameLayout ready for population.
func NewFrameLayout() *FrameLayout {
	return &FrameLayout{
		AllocaSlots: make(map[string]int),
		SpillSlots:  make(map[string]int),
	}
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

	// Frame is populated by the register-allocation stage.  It is nil before
	// that stage has run.
	Frame *FrameLayout

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
