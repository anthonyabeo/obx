package tacil

type ObjKind int

const (
	Var ObjKind = iota
	Const
	Func
)

type Object interface {
	Name() string
	Type() Type
	Kind() ObjKind
	Offset() int
	Parent() Scope
	SetParent(Scope)
}

// Variable
// ----------------
type Variable struct {
	name   string
	ty     Type
	offset int
	kind   ObjKind
	parent Scope
}

func CreateVariableObject(name string, ty Type, offset int, kind ObjKind) *Variable {
	return &Variable{
		name:   name,
		ty:     ty,
		kind:   kind,
		offset: offset,
	}
}
func (v *Variable) Name() string      { return v.name }
func (v *Variable) Type() Type        { return v.ty }
func (v *Variable) Kind() ObjKind     { return v.kind }
func (v *Variable) Offset() int       { return v.offset }
func (v *Variable) Parent() Scope     { return v.parent }
func (v *Variable) SetParent(s Scope) { v.parent = s }
