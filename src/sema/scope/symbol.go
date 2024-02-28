package scope

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type SymKind int

const (
	Invalid SymKind = iota

	PROC
	VAR
	CONST
	TYPE
	PREDECL
	MOD
)

// Symbol
// ---------------------------------------
type Symbol interface {
	Parent() *Scope       // scope in which this object is declared; nil for methods and struct fields
	Pos() *token.Position // position of object identifier in declaration
	Name() string         // package local object name
	Kind() SymKind
	Type() types.Type      // object type
	Offset() int           // offset of this symbol from the base address of its parent data area
	Props() ast.IdentProps // reports whether the name ends with a + or -
	String() string        // String returns a human-readable string of the object.
	setParent(*Scope)      // setParent sets the parent scope of the object.

	Alloca() *ir.AllocaInst
	SetAlloca(*ir.AllocaInst)
}

// A symbol implements the common parts of an Object.
type symbol struct {
	parent *Scope
	pos    *token.Position
	name   string
	kind   SymKind
	typ    types.Type
	props  ast.IdentProps
	offset int
	alloc  *ir.AllocaInst
}

func (sym *symbol) Parent() *Scope                { return sym.parent }
func (sym *symbol) Name() string                  { return sym.name }
func (sym *symbol) Kind() SymKind                 { return sym.kind }
func (sym *symbol) Type() types.Type              { return sym.typ }
func (sym *symbol) Pos() *token.Position          { return sym.pos }
func (sym *symbol) setParent(parent *Scope)       { sym.parent = parent }
func (sym *symbol) Props() ast.IdentProps         { return sym.props }
func (sym *symbol) SetAlloca(inst *ir.AllocaInst) { sym.alloc = inst }
func (sym *symbol) Alloca() *ir.AllocaInst        { return sym.alloc }
func (sym *symbol) Offset() int                   { return sym.offset }

// A Variable represents a declared variable (including function parameters and results, and struct fields).
// ----------------------------------------------------------------------------------------------------------
type Variable struct {
	symbol
}

// NewVar returns a new variable.
func NewVar(pos *token.Position, name string, typ types.Type, props ast.IdentProps, offset int) *Variable {
	return &Variable{symbol: symbol{nil, pos, name, VAR, typ, props, offset, nil}}
}

func (v Variable) String() string { return v.name }

// A Procedure represents a declared procedure or concrete method. Its Type() is always a *Signature.
// --------------------------------------------------------------------------------------------------
type Procedure struct {
	symbol
}

func NewProcedure(pos *token.Position, name string, sig *ast.Signature, props ast.IdentProps, offset int) *Procedure {
	var typ types.Type
	if sig != nil {
		typ = sig
	}

	return &Procedure{symbol{nil, pos, name, PROC, typ, props, offset, nil}}
}

func (p *Procedure) String() string { return "" }

// A Builtin represents a built-in function. Builtins don't have a valid type.
// --------------------------------------------------------------------------------------------------
type Builtin struct {
	symbol
	Id BuiltinId
}

func NewBuiltin(id BuiltinId) *Builtin {
	return &Builtin{symbol{name: PredeclaredProcedures[id].Name, typ: Typ[types.Invalid], kind: PREDECL}, id}
}

func (obj *Builtin) String() string { return obj.symbol.name }

// A TypeName represents a name for a (defined or alias) type.
// --------------------------------------------------------------------------------------------------
type TypeName struct {
	symbol
}

func NewTypeName(
	pos *token.Position,
	name string,
	ty types.Type,
	props ast.IdentProps,
	offset int,
) *TypeName {
	return &TypeName{symbol{nil, pos, name, TYPE, ty, props, offset, nil}}
}

func (obj *TypeName) String() string { return "" }

// A Const represents a constant declaration or the value of an enumeration
// --------------------------------------------------------------------------------------------------
type Const struct {
	symbol
	value ast.Expression
}

func NewConst(
	pos *token.Position,
	name string,
	ty types.Type,
	props ast.IdentProps,
	value ast.Expression,
	offset int,
) *Const {
	return &Const{symbol{nil, pos, name, CONST, ty, props, offset, nil}, value}
}

func (c *Const) String() string { panic("not implemented") }
