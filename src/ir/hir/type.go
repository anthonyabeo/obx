package hir

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type (
	IntType struct {
		Bits   int // 8, 16, 32, 64
		Signed bool
	}

	RealType struct {
		Bits int // 32 or 64
	}

	BoolType struct{}

	CharType struct{}

	StringType struct{}

	SetType struct{}

	NilType struct{}

	UnknownType struct{}

	ArrayType struct {
		Length int64
		Base   Type
	}

	PointerType struct {
		Base Type
	}

	RecordType struct {
		Base   *RecordType
		Fields []*Field
	}

	ProcType struct {
		Params []Type
		Result Type
	}

	Field struct {
		Name string
		Type Type
	}

	EnumType struct {
		Variants map[string]int
	}

	ProcedureType struct {
		Params          []*Param // parameter types only; use signature binding for names/kinds
		Result          Type     // nil if PROCEDURE is a command (no result)
		IsTypeBoundType bool
	}

	NamedType struct {
		Name   string // fully qualified, e.g., M.T or just T
		Symbol ast.Symbol
	}
)

func (*IntType) isType()       {}
func (*RealType) isType()      {}
func (*BoolType) isType()      {}
func (*CharType) isType()      {}
func (*StringType) isType()    {}
func (*SetType) isType()       {}
func (*ArrayType) isType()     {}
func (*PointerType) isType()   {}
func (*RecordType) isType()    {}
func (*ProcType) isType()      {}
func (*EnumType) isType()      {}
func (*NilType) isType()       {}
func (*ProcedureType) isType() {}
func (*UnknownType) isType()   {}
func (*NamedType) isType()     {}

func (i *IntType) String() string {
	return fmt.Sprintf("INT%d", i.Bits)
}
func (ty *RealType) String() string      { panic("not implemented") }
func (ty *BoolType) String() string      { panic("not implemented") }
func (ty *CharType) String() string      { return "CHAR" }
func (ty *StringType) String() string    { panic("not implemented") }
func (ty *SetType) String() string       { panic("not implemented") }
func (ty *ArrayType) String() string     { panic("not implemented") }
func (ty *PointerType) String() string   { panic("not implemented") }
func (ty *RecordType) String() string    { panic("not implemented") }
func (ty *ProcType) String() string      { panic("not implemented") }
func (ty *EnumType) String() string      { panic("not implemented") }
func (ty *NilType) String() string       { panic("not implemented") }
func (ty *ProcedureType) String() string { panic("not implemented") }
func (ty *UnknownType) String() string   { panic("not implemented") }
func (ty *NamedType) String() string     { return ty.Name }
