package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type SymbolKind int

const (
	Unknown SymbolKind = iota
	VariableSymbolKind
	ProcedureSymbolKind
	TypeSymbolKind
	ConstantSymbolKind
	ModuleSymbolKind
	DefinitionSymbolKind
	FieldSymbolKind
	ParamSymbolKind
	TypeBoundProcedureSymbolKind
)

func (s SymbolKind) String() string {
	switch s {
	case VariableSymbolKind:
		return "VariableSymbolKind"
	case ProcedureSymbolKind:
		return "ProcedureSymbolKind"
	case TypeSymbolKind:
		return "Type"
	case ConstantSymbolKind:
		return "ConstantSymbolKind"
	case ModuleSymbolKind:
		return "Module"
	case DefinitionSymbolKind:
		return "Definition"
	case FieldSymbolKind:
		return "FieldSymbolKind"
	case TypeBoundProcedureSymbolKind:
		return "TypeBoundProcedureSymbolKind"
	default:
		return "Unknown"
	}
}

func (s SymbolKind) IsProcedure() bool {
	switch s {
	case ProcedureSymbolKind, TypeBoundProcedureSymbolKind:
		return true
	default:
		return false
	}
}

type Symbol interface {
	Name() string
	Kind() SymbolKind
	Parent() *Environment
	SetParent(*Environment)
	Type() Type
	Props() IdentProps
}

type TypeSymbol struct {
	name   string
	kind   SymbolKind
	props  IdentProps
	typ    Type
	parent *Environment
}

func (t *TypeSymbol) Name() string                  { return t.name }
func (t *TypeSymbol) Kind() SymbolKind              { return t.kind }
func (t *TypeSymbol) Type() Type                    { return t.typ }
func (t *TypeSymbol) Parent() *Environment          { return t.parent }
func (t *TypeSymbol) SetParent(parent *Environment) { t.parent = parent }
func (t *TypeSymbol) Props() IdentProps             { return t.props }

func NewTypeSymbol(name string, props IdentProps, typ Type) *TypeSymbol {
	return &TypeSymbol{name: name, kind: TypeSymbolKind, props: props, typ: typ}
}

// ProcedureSymbol ...
type ProcedureSymbol struct {
	name   string
	kind   SymbolKind
	props  IdentProps
	parent *Environment
	typ    Type
}

func (p *ProcedureSymbol) Name() string                  { return p.name }
func (p *ProcedureSymbol) Kind() SymbolKind              { return p.kind }
func (p *ProcedureSymbol) Props() IdentProps             { return p.props }
func (p *ProcedureSymbol) Parent() *Environment          { return p.parent }
func (p *ProcedureSymbol) SetParent(parent *Environment) { p.parent = parent }
func (p *ProcedureSymbol) Type() Type                    { return p.typ }

func NewProcedureSymbol(name string, props IdentProps) *ProcedureSymbol {
	return &ProcedureSymbol{name: name, kind: ProcedureSymbolKind, props: props}
}

type ModuleSymbol struct {
	name   string
	kind   SymbolKind
	parent *Environment
	typ    Type
}

func NewModuleSymbol(name string) *ModuleSymbol {
	return &ModuleSymbol{name: name, kind: ModuleSymbolKind}
}
func (m *ModuleSymbol) Name() string                  { return m.name }
func (m *ModuleSymbol) Kind() SymbolKind              { return m.kind }
func (m *ModuleSymbol) Parent() *Environment          { return m.parent }
func (m *ModuleSymbol) SetParent(parent *Environment) { m.parent = parent }
func (m *ModuleSymbol) Type() Type                    { return m.typ }
func (m *ModuleSymbol) Props() IdentProps             { panic("not implemented") }

type VariableSymbol struct {
	name   string
	kind   SymbolKind
	props  IdentProps
	typ    Type
	parent *Environment
}

func NewVariableSymbol(name string, props IdentProps, typ Type) *VariableSymbol {
	return &VariableSymbol{name: name, kind: VariableSymbolKind, props: props, typ: typ}
}
func (v *VariableSymbol) Name() string                  { return v.name }
func (v *VariableSymbol) Kind() SymbolKind              { return v.kind }
func (v *VariableSymbol) Parent() *Environment          { return v.parent }
func (v *VariableSymbol) SetParent(parent *Environment) { v.parent = parent }
func (v *VariableSymbol) Type() Type                    { return v.typ }
func (v *VariableSymbol) Props() IdentProps             { return v.props }

type ConstantSymbol struct {
	name   string
	kind   SymbolKind
	props  IdentProps
	value  Expression
	parent *Environment
	typ    Type
}

func NewConstantSymbol(name string, props IdentProps, value Expression) *ConstantSymbol {
	return &ConstantSymbol{name: name, kind: ConstantSymbolKind, props: props, value: value}
}
func (c *ConstantSymbol) Name() string                  { return c.name }
func (c *ConstantSymbol) Kind() SymbolKind              { return c.kind }
func (c *ConstantSymbol) Parent() *Environment          { return c.parent }
func (c *ConstantSymbol) SetParent(parent *Environment) { c.parent = parent }
func (c *ConstantSymbol) Type() Type                    { return c.typ }
func (c *ConstantSymbol) Props() IdentProps             { return c.props }

type FieldSymbol struct {
	name   string
	kind   SymbolKind
	props  IdentProps
	typ    Type
	parent *Environment
}

func NewFieldSymbol(name string, props IdentProps, typ Type) *FieldSymbol {
	return &FieldSymbol{name: name, kind: FieldSymbolKind, props: props, typ: typ}
}
func (f *FieldSymbol) Name() string                 { return f.name }
func (f *FieldSymbol) Kind() SymbolKind             { return f.kind }
func (f *FieldSymbol) Parent() *Environment         { return f.parent }
func (f *FieldSymbol) SetParent(table *Environment) { f.parent = table }
func (f *FieldSymbol) Type() Type                   { return f.typ }
func (f *FieldSymbol) Props() IdentProps            { return f.props }

type ParamSymbol struct {
	name   string
	kind   SymbolKind
	mod    token.Kind
	typ    Type
	parent *Environment
}

func NewParamSymbol(name string, mod token.Kind, typ Type) *ParamSymbol {
	return &ParamSymbol{name: name, kind: ParamSymbolKind, mod: mod, typ: typ}
}

func (p *ParamSymbol) Name() string                  { return p.name }
func (p *ParamSymbol) Kind() SymbolKind              { return p.kind }
func (p *ParamSymbol) Parent() *Environment          { return p.parent }
func (p *ParamSymbol) SetParent(parent *Environment) { p.parent = parent }
func (p *ParamSymbol) Type() Type                    { return p.typ }
func (p *ParamSymbol) Props() IdentProps             { panic("not implemented") }
