package lir

import "fmt"

type (
	VoidType struct{}

	IntegerType struct {
		Bits   int
		Signed bool
	}

	RealType struct {
		Bits int
	}

	PtrType struct {
		Base Type
	}

	ArrayType struct {
		Size     int
		ElemType Type
	}
)

func (*VoidType) isLIRType()    {}
func (*IntegerType) isLIRType() {}
func (*RealType) isLIRType()    {}
func (*PtrType) isLIRType()     {}
func (*ArrayType) isLIRType()   {}

func (*VoidType) Children() []Node    { return []Node{} }
func (*IntegerType) Children() []Node { return []Node{} }
func (*RealType) Children() []Node    { return []Node{} }
func (*PtrType) Children() []Node     { return []Node{} }
func (*ArrayType) Children() []Node   { return []Node{} }

func (*VoidType) String() string      { return "void" }
func (i *IntegerType) String() string { return fmt.Sprintf("i%d", i.Bits) }
func (r *RealType) String() string    { return fmt.Sprintf("f%d", r.Bits) }
func (p *PtrType) String() string     { return fmt.Sprintf("ptr %s", p.Base) }
func (a *ArrayType) String() string   { return fmt.Sprintf("[%d x %s]", a.Size, a.ElemType) }

var (
	Void = &VoidType{}

	// Boolean
	Int1Type = &IntegerType{Bits: 1, Signed: true}

	// Integer types
	Int8Type  = &IntegerType{Bits: 8, Signed: true}
	Int16Type = &IntegerType{Bits: 16, Signed: true}
	Int32Type = &IntegerType{Bits: 32, Signed: true}
	Int64Type = &IntegerType{Bits: 64, Signed: true}

	UInt8Type  = &IntegerType{Bits: 8, Signed: false}
	UInt16Type = &IntegerType{Bits: 16, Signed: false}
	UInt32Type = &IntegerType{Bits: 32, Signed: false}
	UInt64Type = &IntegerType{Bits: 64, Signed: false}

	// Real types
	Float32Type = &RealType{Bits: 32}
	Float64Type = &RealType{Bits: 64}
)
