package ast

import (
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
	"strings"
)

type SymbolKind int

const (
	Unknown SymbolKind = iota
	VariableSymbolKind
	ProcedureSymbolKind
	TypeSymbolKind
	ConstantSymbolKind
	ImportSymbolKind
	DefinitionSymbolKind
	FieldSymbolKind
	ParamSymbolKind
	TypeBoundProcedureSymbolKind
	PredeclaredProcedureSymbolKind
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
	case ImportSymbolKind:
		return "ImportSymbolKind"
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
	TypeNode() Type
	Type() types.Type
	SetType(types.Type)
	Props() IdentProps
	SetMangledName(string)
	MangledName() string
}

type TypeSymbol struct {
	name        string
	kind        SymbolKind
	props       IdentProps
	typ         types.Type
	typeNode    Type
	parent      *Environment
	mangledName string
}

func (t *TypeSymbol) Name() string                  { return t.name }
func (t *TypeSymbol) Kind() SymbolKind              { return t.kind }
func (t *TypeSymbol) TypeNode() Type                { return t.typeNode }
func (t *TypeSymbol) Type() types.Type              { return t.typ }
func (t *TypeSymbol) SetType(ty types.Type)         { t.typ = ty }
func (t *TypeSymbol) Parent() *Environment          { return t.parent }
func (t *TypeSymbol) SetParent(parent *Environment) { t.parent = parent }
func (t *TypeSymbol) Props() IdentProps             { return t.props }
func (t *TypeSymbol) MangledName() string           { return t.mangledName }
func (t *TypeSymbol) SetMangledName(name string)    { t.mangledName = name }

func NewTypeSymbol(name string, props IdentProps, typ Type) *TypeSymbol {
	return &TypeSymbol{name: name, kind: TypeSymbolKind, props: props, typeNode: typ}
}

// ProcedureSymbol ...
type ProcedureSymbol struct {
	name        string
	kind        SymbolKind
	props       IdentProps
	Env         *Environment
	parent      *Environment
	typeNode    Type
	typ         types.Type
	mangledName string
}

func NewProcedureSymbol(name string, props IdentProps, ty Type, env *Environment) *ProcedureSymbol {
	return &ProcedureSymbol{
		Env:      env,
		name:     name,
		kind:     ProcedureSymbolKind,
		props:    props,
		typeNode: ty,
	}
}

func (p *ProcedureSymbol) Name() string                  { return p.name }
func (p *ProcedureSymbol) Kind() SymbolKind              { return p.kind }
func (p *ProcedureSymbol) Props() IdentProps             { return p.props }
func (p *ProcedureSymbol) Parent() *Environment          { return p.parent }
func (p *ProcedureSymbol) SetParent(parent *Environment) { p.parent = parent }
func (p *ProcedureSymbol) MangledName() string           { return p.mangledName }
func (p *ProcedureSymbol) SetMangledName(name string)    { p.mangledName = name }
func (p *ProcedureSymbol) Type() types.Type              { return p.typ }
func (p *ProcedureSymbol) SetType(ty types.Type)         { p.typ = ty }
func (p *ProcedureSymbol) TypeNode() Type                { return p.typeNode }

type ImportSymbol struct {
	name        string
	kind        SymbolKind
	unit        CompilationUnit
	Env         *Environment
	parent      *Environment
	typ         types.Type
	mangledName string
}

func NewImportSymbol(name string) *ImportSymbol {
	return &ImportSymbol{name: name, kind: ImportSymbolKind}
}

func (m *ImportSymbol) Name() string                  { return m.name }
func (m *ImportSymbol) Kind() SymbolKind              { return m.kind }
func (m *ImportSymbol) Parent() *Environment          { return m.parent }
func (m *ImportSymbol) SetParent(parent *Environment) { m.parent = parent }
func (m *ImportSymbol) Type() types.Type              { return m.typ }
func (m *ImportSymbol) SetType(ty types.Type)         { m.typ = ty }
func (m *ImportSymbol) Props() IdentProps             { panic("not implemented") }
func (m *ImportSymbol) MangledName() string           { return m.mangledName }
func (m *ImportSymbol) SetMangledName(name string)    { m.mangledName = name }
func (m *ImportSymbol) TypeNode() Type                { return nil }

type VariableSymbol struct {
	name        string
	kind        SymbolKind
	props       IdentProps
	typeNode    Type
	typ         types.Type
	parent      *Environment
	mangledName string
}

func NewVariableSymbol(name string, props IdentProps, typ Type) *VariableSymbol {
	return &VariableSymbol{name: name, kind: VariableSymbolKind, props: props, typeNode: typ}
}
func (v *VariableSymbol) Name() string                  { return v.name }
func (v *VariableSymbol) Kind() SymbolKind              { return v.kind }
func (v *VariableSymbol) Parent() *Environment          { return v.parent }
func (v *VariableSymbol) SetParent(parent *Environment) { v.parent = parent }
func (v *VariableSymbol) TypeNode() Type                { return v.typeNode }
func (v *VariableSymbol) Type() types.Type              { return v.typ }
func (v *VariableSymbol) SetType(ty types.Type)         { v.typ = ty }
func (v *VariableSymbol) Props() IdentProps             { return v.props }
func (v *VariableSymbol) MangledName() string           { return v.mangledName }
func (v *VariableSymbol) SetMangledName(name string)    { v.mangledName = name }

type ConstantSymbol struct {
	name        string
	kind        SymbolKind
	props       IdentProps
	Value       Expression
	parent      *Environment
	typ         types.Type
	mangledName string
}

func NewConstantSymbol(name string, props IdentProps, value Expression) *ConstantSymbol {
	return &ConstantSymbol{name: name, kind: ConstantSymbolKind, props: props, Value: value}
}
func (c *ConstantSymbol) Name() string                  { return c.name }
func (c *ConstantSymbol) Kind() SymbolKind              { return c.kind }
func (c *ConstantSymbol) Parent() *Environment          { return c.parent }
func (c *ConstantSymbol) SetParent(parent *Environment) { c.parent = parent }
func (c *ConstantSymbol) Type() types.Type              { return c.typ }
func (c *ConstantSymbol) SetType(ty types.Type)         { c.typ = ty }
func (c *ConstantSymbol) Props() IdentProps             { return c.props }
func (c *ConstantSymbol) MangledName() string           { return c.mangledName }
func (c *ConstantSymbol) SetMangledName(name string)    { c.mangledName = name }
func (c *ConstantSymbol) TypeNode() Type                { return nil }

type FieldSymbol struct {
	name        string
	kind        SymbolKind
	props       IdentProps
	typeNode    Type
	typ         types.Type
	parent      *Environment
	mangledName string
}

func NewFieldSymbol(name string, props IdentProps, typ Type) *FieldSymbol {
	return &FieldSymbol{name: name, kind: FieldSymbolKind, props: props, typeNode: typ}
}
func (f *FieldSymbol) Name() string                 { return f.name }
func (f *FieldSymbol) Kind() SymbolKind             { return f.kind }
func (f *FieldSymbol) Parent() *Environment         { return f.parent }
func (f *FieldSymbol) SetParent(table *Environment) { f.parent = table }
func (f *FieldSymbol) TypeNode() Type               { return f.typeNode }
func (f *FieldSymbol) Type() types.Type             { return f.typ }
func (f *FieldSymbol) SetType(ty types.Type)        { f.typ = ty }
func (f *FieldSymbol) Props() IdentProps            { return f.props }
func (f *FieldSymbol) MangledName() string          { return f.mangledName }
func (f *FieldSymbol) SetMangledName(name string)   { f.mangledName = name }

type ParamSymbol struct {
	name        string
	kind        SymbolKind
	Mod         token.Kind
	typeNode    Type
	typeSema    types.Type
	parent      *Environment
	mangledName string
}

func NewParamSymbol(name string, mod token.Kind, typ Type) *ParamSymbol {
	return &ParamSymbol{name: name, kind: ParamSymbolKind, Mod: mod, typeNode: typ}
}

func (p *ParamSymbol) Name() string                  { return p.name }
func (p *ParamSymbol) Kind() SymbolKind              { return p.kind }
func (p *ParamSymbol) Parent() *Environment          { return p.parent }
func (p *ParamSymbol) SetParent(parent *Environment) { p.parent = parent }
func (p *ParamSymbol) Type() types.Type              { return p.typeSema }
func (p *ParamSymbol) SetType(ty types.Type)         { p.typeSema = ty }
func (p *ParamSymbol) Props() IdentProps             { panic("not implemented") }
func (p *ParamSymbol) MangledName() string           { return p.mangledName }
func (p *ParamSymbol) SetMangledName(name string)    { p.mangledName = name }
func (p *ParamSymbol) TypeNode() Type                { return p.typeNode }

func Mangle(sym Symbol) string {
	var parts []string
	for env := sym.Parent(); env != nil && env.name != "global"; env = env.Parent() {
		if env.Name() != "" {
			parts = append([]string{env.Name()}, parts...)
		}
	}
	parts = append(parts, sym.Name())
	return strings.Join(parts, "$")
}

func IsVarParam(dsg *Designator) bool {
	if dsg.QIdent == nil || dsg.QIdent.Symbol == nil {
		return false
	}

	paramSym, ok := dsg.QIdent.Symbol.(*ParamSymbol)
	if !ok {
		return false
	}

	return paramSym.kind == ParamSymbolKind && paramSym.Mod == token.VAR
}
