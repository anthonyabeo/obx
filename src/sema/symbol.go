package sema

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// Symbol
// ---------------------------------------
type Symbol interface {
	Parent() *Scope        // scope in which this object is declared; nil for methods and struct fields
	Pos() *token.Position  // position of object identifier in declaration
	Name() string          // package local object name
	Type() types.Type      // object type
	Props() ast.IdentProps // reports whether the name ends with a + or -

	// String returns a human-readable string of the object.
	String() string

	// setParent sets the parent scope of the object.
	setParent(*Scope)
}

// A symbol implements the common parts of an Object.
type symbol struct {
	parent *Scope
	pos    *token.Position
	name   string
	typ    types.Type
	props  ast.IdentProps
}

// Parent returns the scope in which the object is declared.
// The result is nil for methods and struct fields.
func (obj *symbol) Parent() *Scope { return obj.parent }

// Name returns the object's (package-local, unqualified) name.
func (obj *symbol) Name() string { return obj.name }

// Type returns the object's type.
func (obj *symbol) Type() types.Type { return obj.typ }

// Pos returns the declaration position of the object's identifier.
func (obj *symbol) Pos() *token.Position { return obj.pos }

func (obj *symbol) setParent(parent *Scope) { obj.parent = parent }

func (obj *symbol) Props() ast.IdentProps { return obj.props }

// A Variable represents a declared variable (including function parameters and results, and struct fields).
// ----------------------------------------------------------------------------------------------------------
type Variable struct {
	symbol
}

// NewVar returns a new variable.
func NewVar(pos *token.Position, name string, typ types.Type, props ast.IdentProps) *Variable {
	return &Variable{symbol: symbol{nil, pos, name, typ, props}}
}

func (v Variable) String() string {
	return v.name
}

// A Procedure represents a declared procedure or concrete method. Its Type() is always a *Signature.
// --------------------------------------------------------------------------------------------------
type Procedure struct {
	symbol
}

func NewProcedure(pos *token.Position, name string, sig *ast.Signature, props ast.IdentProps) *Procedure {
	var typ types.Type
	if sig != nil {
		typ = sig
	}

	return &Procedure{symbol{nil, pos, name, typ, props}}
}

func (p *Procedure) String() string {
	return ""
}

// A Builtin represents a built-in function. Builtins don't have a valid type.
// --------------------------------------------------------------------------------------------------
type Builtin struct {
	symbol
	id builtinId
}

func newBuiltin(id builtinId) *Builtin {
	return &Builtin{symbol{name: predeclaredProcedures[id].name, typ: Typ[types.Invalid]}, id}
}

func (obj *Builtin) String() string { return obj.symbol.name }

// A TypeName represents a name for a (defined or alias) type.
// --------------------------------------------------------------------------------------------------
type TypeName struct {
	symbol
}

func NewTypeName(pos *token.Position, name string, typ types.Type, props ast.IdentProps) *TypeName {
	return &TypeName{symbol{nil, pos, name, typ, props}}
}

func (obj *TypeName) String() string { return "" }

// A Const represents a constant declaration or the value of an enumeration
// --------------------------------------------------------------------------------------------------
type Const struct {
	symbol
	value ast.Expression
}

func NewConst(pos *token.Position, name string, typ types.Type, props ast.IdentProps, value ast.Expression) *Const {
	return &Const{symbol{nil, pos, name, typ, props}, value}
}

func (c *Const) String() string { panic("not implemented") }
