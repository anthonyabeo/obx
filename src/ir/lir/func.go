package lir

// Function : A procedure/function in LIR.
type Function struct {
	Name       string   // e.g. "main", "Insert"
	Ret        Type     // void if procedure is a command
	Params     []*Param // input registers (may include return reg)
	Locals     []*Local // all locals (excluding params); for allocation info
	Blocks     []*Block // list of blocks; usually starts with "entry"
	IsExported bool     // for modules with export rules
}

type Param struct {
	Name string
	Type Type
	Kind string // "value", "VAR", "IN"
}

type Local struct {
	Name   string
	Offset int // offset in stack frame
	Type   Type
}
