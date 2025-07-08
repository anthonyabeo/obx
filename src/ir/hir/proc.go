package hir

type Procedure struct {
	Name       string
	Params     []*Param
	Result     Type // nil if procedure has no return
	Locals     []Decl
	Body       *CompoundStmt
	IsExported bool
}

type Param struct {
	Name string
	Kind ParamKind // Value, Var, In
	Type Type
}

type ParamKind int

const (
	ValueParam ParamKind = iota
	VarParam
	InParam
)
