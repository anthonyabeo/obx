package desugar

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type ParamKind int

const (
	ValueParam ParamKind = iota
	VarParam
	InParam
)

type (
	Literal struct {
		Kind        token.Kind
		Value       string
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	BinaryExpr struct {
		Left        Expr
		Right       Expr
		Op          token.Kind
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	UnaryExpr struct {
		Operand     Expr
		Op          token.Kind
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	SetExpr struct {
		Elems       []Expr
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	RangeExpr struct {
		Low         Expr
		High        Expr
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	FuncCall struct {
		Func        *FunctionRef
		Args        []Expr
		RetType     types.Type
		StartOffset int
		EndOffset   int
	}

	VariableRef struct {
		Name        string     // name of the variable
		Mangled     string     // mangled name for code emission
		SemaType    types.Type // variable type
		Module      string     // name of the module in which it declared/used
		IsExported  bool
		IsReadOnly  bool
		Size        int
		StartOffset int
		EndOffset   int
	}

	ConstantRef struct {
		Name        string // name of the variable
		Mangled     string // mangled name for code emission
		Value       Expr
		SemaType    types.Type // variable type
		Module      string     // name of the module in which it declared/used
		IsExported  bool
		IsReadOnly  bool
		Offset      int
		Size        int
		StartOffset int
		EndOffset   int
	}

	FunctionRef struct {
		Name        string     // name of the variable
		Mangled     string     // mangled name for code emission
		SemaType    types.Type // variable type
		Kind        ast.ProcedureKind
		Module      string // name of the module in which it declared/used
		IsExported  bool
		IsReadOnly  bool
		StartOffset int
		EndOffset   int
		// FFI fields
		IsExternal bool
		IsVarArgs  bool
	}

	TypeRef struct {
		Name        string
		Mangled     string // mangled name for code emission
		UnderType   types.Type
		Module      string // name of the module in which it declared/used
		IsExported  bool
		IsReadOnly  bool
		Offset      int
		Size        int
		StartOffset int
		EndOffset   int
	}

	// ModuleRef represents a reference to a module or definition module used
	// as a first-class value (e.g. LDMOD(Fmt)).
	ModuleRef struct {
		Name        string
		Mangled     string
		Module      string
		SemaType    types.Type
		IsExported  bool
		IsReadOnly  bool
		StartOffset int
		EndOffset   int
	}

	FieldAccess struct {
		Record      Expr
		Field       string
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	IndexExpr struct {
		Array       Expr
		Index       []Expr
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	DerefExpr struct {
		Pointer     Expr
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	TypeGuardExpr struct {
		Expr        Expr
		Typ         types.Type
		SemaType    types.Type
		StartOffset int
		EndOffset   int
	}

	Param struct {
		Name        string
		Kind        ParamKind // Value, Var, In
		Typ         types.Type
		StartOffset int
		EndOffset   int
	}
)

func (*Literal) expr()       {}
func (*BinaryExpr) expr()    {}
func (*UnaryExpr) expr()     {}
func (*FuncCall) expr()      {}
func (*FieldAccess) expr()   {}
func (*IndexExpr) expr()     {}
func (*DerefExpr) expr()     {}
func (*TypeGuardExpr) expr() {}
func (*SetExpr) expr()       {}
func (*RangeExpr) expr()     {}
func (*VariableRef) expr()   {}
func (*ConstantRef) expr()   {}
func (*FunctionRef) expr()   {}
func (*TypeRef) expr()       {}
func (*ModuleRef) expr()     {}
func (*Param) expr()         {}

func (e *Literal) Type() types.Type       { return e.SemaType }
func (e *BinaryExpr) Type() types.Type    { return e.SemaType }
func (e *UnaryExpr) Type() types.Type     { return e.SemaType }
func (e *FuncCall) Type() types.Type      { return e.RetType }
func (e *FieldAccess) Type() types.Type   { return e.SemaType }
func (e *IndexExpr) Type() types.Type     { return e.SemaType }
func (e *DerefExpr) Type() types.Type     { return e.SemaType }
func (e *TypeGuardExpr) Type() types.Type { return e.SemaType }
func (e *SetExpr) Type() types.Type       { return e.SemaType }
func (e *RangeExpr) Type() types.Type     { return e.SemaType }
func (e *VariableRef) Type() types.Type   { return e.SemaType }
func (e *ConstantRef) Type() types.Type   { return e.SemaType }
func (e *FunctionRef) Type() types.Type   { return e.SemaType }
func (e *TypeRef) Type() types.Type       { return e.UnderType }
func (e *ModuleRef) Type() types.Type     { return e.SemaType }
func (p *Param) Type() types.Type         { return p.Typ }

func (e *Literal) String() string    { return fmt.Sprintf("%s(%v)", e.Kind, e.Value) }
func (e *BinaryExpr) String() string { return fmt.Sprintf("%s %s %s", e.Left, e.Op, e.Right) }
func (e *UnaryExpr) String() string  { return fmt.Sprintf("%s%s", e.Op, e.Operand) }
func (e *FuncCall) String() string {
	var args []string
	for _, arg := range e.Args {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%s(%s)", e.Func, strings.Join(args, ", "))
}
func (e *FieldAccess) String() string { return fmt.Sprintf("%s.%s", e.Record, e.Field) }
func (e *IndexExpr) String() string {
	var indices []string
	for _, index := range e.Index {
		indices = append(indices, fmt.Sprintf("[%s]", index))
	}

	return fmt.Sprintf("%s%s: %s", e.Array, strings.Join(indices, ""), e.SemaType)
}
func (e *DerefExpr) String() string { return fmt.Sprintf("%s^", e.Pointer) }
func (e *TypeGuardExpr) String() string {
	return fmt.Sprintf("%s(%s)", e.Expr, e.Typ)
}
func (e *SetExpr) String() string {
	var elems []string
	for _, el := range e.Elems {
		elems = append(elems, el.String())
	}
	return fmt.Sprintf("{%s}: %s", strings.Join(elems, ", "), e.SemaType)
}
func (e *RangeExpr) String() string {
	var low, high string
	if e.Low != nil {
		low = e.Low.String()
	}
	if e.High != nil {
		high = e.High.String()
	}
	return fmt.Sprintf("%s..%s: %s", low, high, e.SemaType)
}
func (e *VariableRef) String() string { return e.Name }
func (e *ConstantRef) String() string { return e.Name }
func (e *FunctionRef) String() string { return e.Name }
func (e *TypeRef) String() string     { return e.Name }
func (e *ModuleRef) String() string   { return e.Name }
func (p *Param) String() string       { return p.Name }

func (e *Literal) Pos() int       { return e.StartOffset }
func (e *BinaryExpr) Pos() int    { return e.StartOffset }
func (e *UnaryExpr) Pos() int     { return e.StartOffset }
func (e *FuncCall) Pos() int      { return e.StartOffset }
func (e *FieldAccess) Pos() int   { return e.StartOffset }
func (e *IndexExpr) Pos() int     { return e.StartOffset }
func (e *DerefExpr) Pos() int     { return e.StartOffset }
func (e *TypeGuardExpr) Pos() int { return e.StartOffset }
func (e *SetExpr) Pos() int       { return e.StartOffset }
func (e *RangeExpr) Pos() int     { return e.StartOffset }
func (e *VariableRef) Pos() int   { return e.StartOffset }
func (e *ConstantRef) Pos() int   { return e.StartOffset }
func (e *FunctionRef) Pos() int   { return e.StartOffset }
func (e *TypeRef) Pos() int       { return e.StartOffset }
func (e *ModuleRef) Pos() int     { return e.StartOffset }
func (e *Param) Pos() int         { return e.StartOffset }

func (e *Literal) End() int       { return e.EndOffset }
func (e *BinaryExpr) End() int    { return e.EndOffset }
func (e *UnaryExpr) End() int     { return e.EndOffset }
func (e *FuncCall) End() int      { return e.EndOffset }
func (e *FieldAccess) End() int   { return e.EndOffset }
func (e *IndexExpr) End() int     { return e.EndOffset }
func (e *DerefExpr) End() int     { return e.EndOffset }
func (e *TypeGuardExpr) End() int { return e.EndOffset }
func (e *SetExpr) End() int       { return e.EndOffset }
func (e *RangeExpr) End() int     { return e.EndOffset }
func (e *VariableRef) End() int   { return e.EndOffset }
func (e *ConstantRef) End() int   { return e.EndOffset }
func (e *FunctionRef) End() int   { return e.EndOffset }
func (e *TypeRef) End() int       { return e.EndOffset }
func (e *ModuleRef) End() int     { return e.EndOffset }
func (p *Param) End() int         { return p.EndOffset }
