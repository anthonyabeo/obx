package ir

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/sema/types"
)

type OperandKind int

const (
	Err OperandKind = iota

	KRegister
	KNumber
	KLabel
)

type Operand interface {
	fmt.Stringer
	Id() string
	Kind() OperandKind
}

//func CreateOperand(name string, kind OperandKind) *Operand {
//	return &Operand{Name: name, Kind: kind}
//}

// Register ...
// ---------------------------------------------
type Register struct {
	Name   string
	Offset int
	Type   types.Type
	//Attr   ast.IdentProps

	OpKind OperandKind
	//Obj    sema.Symbol
}

func (r Register) Id() string {
	return r.Name
}

func (r Register) Kind() OperandKind {
	return r.OpKind
}

func (r Register) String() string {
	return fmt.Sprintf("r%s", r.Name)
}

// Number ...
// ------------------------
type Number struct {
	Value  string
	OpKind OperandKind
}

func (n Number) Id() string {
	return n.Value
}

func (n Number) Kind() OperandKind {
	return n.OpKind
}

func (n Number) String() string {
	return n.Value
}
