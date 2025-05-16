package scope

import (
	"github.com/anthonyabeo/obx/src/sema/types"
)

type SymKind int

const (
	PROC SymKind = 1 << iota
	VAR
	CONST
	TYPE
	PREDECL
	MOD
)

// Symbol
// ---------------------------------------
type Symbol interface {
	Parent() Scope // scope in which this object is declared; nil for methods and struct fields
	Name() string  // package local object name
	Kind() SymKind
	Type() types.Type // object type
	SetType(ty types.Type)
	Offset() int       // offset of this symbol from the base address of its parent data area
	Props() IdentProps // reports whether the name ends with a + or -
	String() string    // String returns a human-readable string of the object.
	setParent(Scope)   // setParent sets the parent scope of the object.
}

// A symbol implements the common parts of an Object.
type symbol struct {
	parent Scope
	name   string
	kind   SymKind
	typ    types.Type
	props  IdentProps
	offset int
}

func (sym *symbol) Parent() Scope          { return sym.parent }
func (sym *symbol) Name() string           { return sym.name }
func (sym *symbol) Kind() SymKind          { return sym.kind }
func (sym *symbol) Type() types.Type       { return sym.typ }
func (sym *symbol) SetType(ty types.Type)  { sym.typ = ty }
func (sym *symbol) setParent(parent Scope) { sym.parent = parent }
func (sym *symbol) Props() IdentProps      { return sym.props }
func (sym *symbol) Offset() int            { return sym.offset }

// A Variable represents a declared variable (including function parameters and results, and struct fields).
// ----------------------------------------------------------------------------------------------------------
type Variable struct {
	symbol
}

// NewVar returns a new variable.
func NewVar(name string, typ types.Type, props IdentProps, offset int) *Variable {
	return &Variable{symbol: symbol{nil, name, VAR, typ, props, offset}}
}

func (v Variable) String() string { return v.name }

// A Procedure represents a declared procedure or concrete method. Its Type() is always a *Signature.
// --------------------------------------------------------------------------------------------------
type Procedure struct {
	symbol
}

func NewProcedure(name string, sig *types.Signature, props IdentProps, offset int) *Procedure {
	var typ types.Type
	if sig != nil {
		typ = sig
	}

	return &Procedure{symbol{nil, name, PROC, typ, props, offset}}
}

func (p *Procedure) String() string { return "" }

// A PreDeclFuncOrProc represents a built-in function. Builtins don't have a valid type.
// --------------------------------------------------------------------------------------------------
type PreDeclFuncOrProc struct {
	symbol
	Id PreDeclFuncProc
}

func NewPreDeclFuncProc(id PreDeclFuncProc) *PreDeclFuncOrProc {
	return &PreDeclFuncOrProc{symbol{name: PredeclaredProcedures[id].Name, typ: Typ[types.Invalid], kind: PREDECL}, id}
}

func (obj *PreDeclFuncOrProc) String() string { return obj.symbol.name }

// A TypeName represents a name for a (defined or alias) type.
// --------------------------------------------------------------------------------------------------
type TypeName struct {
	symbol
}

func NewTypeName(
	name string,
	ty types.Type,
	props IdentProps,
	offset int,
) *TypeName {
	return &TypeName{symbol{nil, name, TYPE, ty, props, offset}}
}

func (obj *TypeName) String() string { return "" }

// A Const represents a constant declaration or the value of an enumeration
// --------------------------------------------------------------------------------------------------
type Const struct {
	symbol
	//value ast.Expression
}

func NewConst(
	name string,
	ty types.Type,
	props IdentProps,
	//value ast.Expression,
	offset int,
) *Const {
	return &Const{symbol{nil, name, CONST, ty, props, offset}}
}

func (c *Const) String() string { panic("not implemented") }

//func (c *Const) Value() ast.Expression { return c.value }

// Module
// -----------------------
type Module struct {
	symbol
	Scope Scope
}

func NewModule(name string, scp Scope) *Module {
	return &Module{
		Scope:  scp,
		symbol: symbol{nil, name, MOD, Typ[types.Invalid], 0, 0}}
}

func (m *Module) String() string { panic("not implemented") }
