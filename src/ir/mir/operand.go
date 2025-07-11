package mir

import "fmt"

type Operand interface {
	isValue()
	String() string
}

type (
	Variable struct {
		Name string
	}

	FieldAccess struct {
		Record Operand
		Field  string
	}

	IndexExpr struct {
		Array Operand
		Index []Operand
	}

	DerefExpr struct {
		Pointer Operand
	}

	TypeGuardExpr struct {
		Expr VarDecl
	}

	IntConst struct {
		Value int64
	}

	BoolConst struct {
		Value bool
	}

	StringConst struct {
		Value string
	}

	SetConst struct {
		Elems []int
	}

	Nil struct{}
)

func (*Variable) isValue()    {}
func (*FieldAccess) isValue() {}
func (*IndexExpr) isValue()   {}
func (*DerefExpr) isValue()   {}
func (*IntConst) isValue()    {}
func (*BoolConst) isValue()   {}
func (*StringConst) isValue() {}
func (*SetConst) isValue()    {}
func (*Nil) isValue()         {}
func (*CmpInst) isValue()     {}

func (v *Variable) String() string    { return v.Name }
func (v *FieldAccess) String() string { return fmt.Sprintf("%v.%v", v.Record, v.Field) }
func (v *IndexExpr) String() string   { return fmt.Sprintf("%v[%v]", v.Array, v.Index) }
func (v *DerefExpr) String() string   { return fmt.Sprintf("%v^", v.Pointer) }
func (v *IntConst) String() string    { return fmt.Sprintf("%d", v.Value) }
func (v *BoolConst) String() string   { return fmt.Sprintf("%t", v.Value) }
func (v *StringConst) String() string { return v.Value }
func (v *SetConst) String() string    { panic("not implemented") }
func (v *Nil) String() string         { return "nil" }
