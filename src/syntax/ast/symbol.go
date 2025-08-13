package ast

import (
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
)

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
		return "ModuleSymbolKind"
	case DefinitionSymbolKind:
		return "Definition"
	case FieldSymbolKind:
		return "FieldSymbolKind"
	default:
		return "Unknown"
	}
}

func (s SymbolKind) IsProcedure() bool {
	switch s {
	case ProcedureSymbolKind:
		return true
	default:
		return false
	}
}

type Symbol interface {
	Name() string
	Kind() SymbolKind
	Parent() *LexicalScope
	SetParent(*LexicalScope)
	AstType() Type
	Type() types.Type
	SetType(types.Type)
	Props() IdentProps
	SetMangledName(string)
	MangledName() string
	Offset() int
	SetOffset(int)
}

type TypeSymbol struct {
	name        string
	kind        SymbolKind
	props       IdentProps
	semaType    types.Type
	astType     Type
	parent      *LexicalScope
	mangledName string
	Displace    int
}

func (t *TypeSymbol) Name() string                   { return t.name }
func (t *TypeSymbol) Kind() SymbolKind               { return t.kind }
func (t *TypeSymbol) AstType() Type                  { return t.astType }
func (t *TypeSymbol) Type() types.Type               { return t.semaType }
func (t *TypeSymbol) SetType(ty types.Type)          { t.semaType = ty }
func (t *TypeSymbol) Parent() *LexicalScope          { return t.parent }
func (t *TypeSymbol) SetParent(parent *LexicalScope) { t.parent = parent }
func (t *TypeSymbol) Props() IdentProps              { return t.props }
func (t *TypeSymbol) MangledName() string            { return t.mangledName }
func (t *TypeSymbol) SetMangledName(name string)     { t.mangledName = name }
func (t *TypeSymbol) Offset() int                    { return t.Displace }
func (t *TypeSymbol) SetOffset(disp int)             { t.Displace = disp }

func NewTypeSymbol(name string, props IdentProps, typ Type) *TypeSymbol {
	return &TypeSymbol{name: name, kind: TypeSymbolKind, props: props, astType: typ}
}

// ProcedureSymbol ...
type ProcedureSymbol struct {
	name        string
	kind        SymbolKind
	props       IdentProps
	Env         *LexicalScope
	parent      *LexicalScope
	astType     Type
	semaType    types.Type
	mangledName string
	ProcKind    ProcedureKind
	Displace    int
}

func NewProcedureSymbol(name string, props IdentProps, ty Type, env *LexicalScope, procKind ProcedureKind) *ProcedureSymbol {
	return &ProcedureSymbol{
		Env:      env,
		name:     name,
		kind:     ProcedureSymbolKind,
		props:    props,
		astType:  ty,
		ProcKind: procKind,
	}
}

func (p *ProcedureSymbol) Name() string                   { return p.name }
func (p *ProcedureSymbol) Kind() SymbolKind               { return p.kind }
func (p *ProcedureSymbol) Props() IdentProps              { return p.props }
func (p *ProcedureSymbol) Parent() *LexicalScope          { return p.parent }
func (p *ProcedureSymbol) SetParent(parent *LexicalScope) { p.parent = parent }
func (p *ProcedureSymbol) MangledName() string            { return p.mangledName }
func (p *ProcedureSymbol) SetMangledName(name string)     { p.mangledName = name }
func (p *ProcedureSymbol) Type() types.Type               { return p.semaType }
func (p *ProcedureSymbol) SetType(ty types.Type)          { p.semaType = ty }
func (p *ProcedureSymbol) AstType() Type                  { return p.astType }
func (p *ProcedureSymbol) Offset() int                    { return p.Displace }
func (p *ProcedureSymbol) SetOffset(disp int)             { p.Displace = disp }

type ModuleSymbol struct {
	name        string
	kind        SymbolKind
	unit        CompilationUnit
	Env         *LexicalScope
	parent      *LexicalScope
	semaType    types.Type
	mangledName string
	Displace    int
}

func NewImportSymbol(name string) *ModuleSymbol {
	return &ModuleSymbol{name: name, kind: ModuleSymbolKind}
}

func (m *ModuleSymbol) Name() string                   { return m.name }
func (m *ModuleSymbol) Kind() SymbolKind               { return m.kind }
func (m *ModuleSymbol) Parent() *LexicalScope          { return m.parent }
func (m *ModuleSymbol) SetParent(parent *LexicalScope) { m.parent = parent }
func (m *ModuleSymbol) Type() types.Type               { return m.semaType }
func (m *ModuleSymbol) SetType(ty types.Type)          { m.semaType = ty }
func (m *ModuleSymbol) Props() IdentProps              { panic("not implemented") }
func (m *ModuleSymbol) MangledName() string            { return m.mangledName }
func (m *ModuleSymbol) SetMangledName(name string)     { m.mangledName = name }
func (m *ModuleSymbol) AstType() Type                  { return nil }
func (m *ModuleSymbol) Offset() int                    { return m.Displace }
func (m *ModuleSymbol) SetOffset(disp int)             { m.Displace = disp }

type VariableSymbol struct {
	name        string
	kind        SymbolKind
	props       IdentProps
	astType     Type
	semaType    types.Type
	parent      *LexicalScope
	mangledName string
	Displace    int
}

func NewVariableSymbol(name string, props IdentProps, typ Type) *VariableSymbol {
	return &VariableSymbol{name: name, kind: VariableSymbolKind, props: props, astType: typ}
}
func (v *VariableSymbol) Name() string                   { return v.name }
func (v *VariableSymbol) Kind() SymbolKind               { return v.kind }
func (v *VariableSymbol) Parent() *LexicalScope          { return v.parent }
func (v *VariableSymbol) SetParent(parent *LexicalScope) { v.parent = parent }
func (v *VariableSymbol) AstType() Type                  { return v.astType }
func (v *VariableSymbol) Type() types.Type               { return v.semaType }
func (v *VariableSymbol) SetType(ty types.Type)          { v.semaType = ty }
func (v *VariableSymbol) Props() IdentProps              { return v.props }
func (v *VariableSymbol) MangledName() string            { return v.mangledName }
func (v *VariableSymbol) SetMangledName(name string)     { v.mangledName = name }
func (v *VariableSymbol) Offset() int                    { return v.Displace }
func (v *VariableSymbol) SetOffset(disp int)             { v.Displace = disp }

type ConstantSymbol struct {
	name        string
	kind        SymbolKind
	props       IdentProps
	Value       Expression
	parent      *LexicalScope
	semaType    types.Type
	mangledName string
	Displace    int
}

func NewConstantSymbol(name string, props IdentProps, value Expression) *ConstantSymbol {
	return &ConstantSymbol{name: name, kind: ConstantSymbolKind, props: props, Value: value}
}
func (c *ConstantSymbol) Name() string                   { return c.name }
func (c *ConstantSymbol) Kind() SymbolKind               { return c.kind }
func (c *ConstantSymbol) Parent() *LexicalScope          { return c.parent }
func (c *ConstantSymbol) SetParent(parent *LexicalScope) { c.parent = parent }
func (c *ConstantSymbol) Type() types.Type               { return c.semaType }
func (c *ConstantSymbol) SetType(ty types.Type)          { c.semaType = ty }
func (c *ConstantSymbol) Props() IdentProps              { return c.props }
func (c *ConstantSymbol) MangledName() string            { return c.mangledName }
func (c *ConstantSymbol) SetMangledName(name string)     { c.mangledName = name }
func (c *ConstantSymbol) AstType() Type                  { return nil }
func (c *ConstantSymbol) Offset() int                    { return c.Displace }
func (c *ConstantSymbol) SetOffset(disp int)             { c.Displace = disp }

type FieldSymbol struct {
	name        string
	kind        SymbolKind
	props       IdentProps
	astType     Type
	semaType    types.Type
	parent      *LexicalScope
	mangledName string
	Displace    int
}

func NewFieldSymbol(name string, props IdentProps, typ Type) *FieldSymbol {
	return &FieldSymbol{name: name, kind: FieldSymbolKind, props: props, astType: typ}
}
func (f *FieldSymbol) Name() string                  { return f.name }
func (f *FieldSymbol) Kind() SymbolKind              { return f.kind }
func (f *FieldSymbol) Parent() *LexicalScope         { return f.parent }
func (f *FieldSymbol) SetParent(table *LexicalScope) { f.parent = table }
func (f *FieldSymbol) AstType() Type                 { return f.astType }
func (f *FieldSymbol) Type() types.Type              { return f.semaType }
func (f *FieldSymbol) SetType(ty types.Type)         { f.semaType = ty }
func (f *FieldSymbol) Props() IdentProps             { return f.props }
func (f *FieldSymbol) MangledName() string           { return f.mangledName }
func (f *FieldSymbol) SetMangledName(name string)    { f.mangledName = name }
func (f *FieldSymbol) Offset() int                   { return f.Displace }
func (f *FieldSymbol) SetOffset(disp int)            { f.Displace = disp }

type ParamSymbol struct {
	name        string
	kind        SymbolKind
	Mod         token.Kind
	astType     Type
	semaType    types.Type
	parent      *LexicalScope
	mangledName string
	Displace    int
}

func NewParamSymbol(name string, mod token.Kind, typ Type) *ParamSymbol {
	return &ParamSymbol{name: name, kind: ParamSymbolKind, Mod: mod, astType: typ}
}

func (p *ParamSymbol) Name() string                   { return p.name }
func (p *ParamSymbol) Kind() SymbolKind               { return p.kind }
func (p *ParamSymbol) Parent() *LexicalScope          { return p.parent }
func (p *ParamSymbol) SetParent(parent *LexicalScope) { p.parent = parent }
func (p *ParamSymbol) Type() types.Type               { return p.semaType }
func (p *ParamSymbol) SetType(ty types.Type)          { p.semaType = ty }
func (p *ParamSymbol) Props() IdentProps              { panic("not implemented") }
func (p *ParamSymbol) MangledName() string            { return p.mangledName }
func (p *ParamSymbol) SetMangledName(name string)     { p.mangledName = name }
func (p *ParamSymbol) AstType() Type                  { return p.astType }
func (p *ParamSymbol) Offset() int                    { return p.Displace }
func (p *ParamSymbol) SetOffset(disp int)             { p.Displace = disp }

func Mangle(sym Symbol) string {
	var parts []string
	for env := sym.Parent(); env != nil && env.Name != "global"; env = env.Parent() {
		if env.Name != "" {
			parts = append([]string{env.Name}, parts...)
		}
	}
	parts = append(parts, sym.Name())
	return strings.Join(parts, "$")
}

func IsVarParam(dsg *Designator) bool {
	if dsg.QIdent == nil || dsg.QIdent.Symbol == nil {
		return false
	}

	if _, isVarSym := dsg.Symbol.(*VariableSymbol); isVarSym {
		return true
	}

	paramSym, isParamSym := dsg.QIdent.Symbol.(*ParamSymbol)
	if !isParamSym {

		return paramSym.Mod == token.VAR
	}

	return false
}
