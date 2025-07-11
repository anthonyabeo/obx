package mir

import "fmt"

type Type interface {
	isMIRType()
	String() string
}

type (
	IntegerType struct {
		Bits   int
		Signed bool
	}

	PointerType struct {
		Ref Type
	}
)

func (p PointerType) isMIRType()     {}
func (p PointerType) String() string { return fmt.Sprintf("*%s", p.Ref) }

func (*IntegerType) isMIRType()       {}
func (i *IntegerType) String() string { return fmt.Sprintf("i%d", i.Bits) }

var (
	Int1Type = &IntegerType{Bits: 1, Signed: true}

	Int8Type  = &IntegerType{Bits: 8, Signed: true}
	Int16Type = &IntegerType{Bits: 16, Signed: true}
	Int32Type = &IntegerType{Bits: 32, Signed: true}
	Int64Type = &IntegerType{Bits: 64, Signed: true}

	UInt8Type  = &IntegerType{Bits: 8, Signed: false}
	UInt16Type = &IntegerType{Bits: 16, Signed: false}
	UInt32Type = &IntegerType{Bits: 32, Signed: false}
	UInt64Type = &IntegerType{Bits: 64, Signed: false}
)
