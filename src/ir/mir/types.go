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

	FloatType struct {
		Bits int
	}

	PointerType struct {
		Ref Type
	}
)

func (*IntegerType) isMIRType() {}
func (*PointerType) isMIRType() {}
func (*FloatType) isMIRType()   {}

func (t *PointerType) String() string { return fmt.Sprintf("*%s", t.Ref) }
func (t *IntegerType) String() string {
	if t.Signed {
		return fmt.Sprintf("i%d", t.Bits)
	}

	return fmt.Sprintf("u%d", t.Bits)
}
func (t *FloatType) String() string { return fmt.Sprintf("f%d", t.Bits) }

var (
	Int1Type = &IntegerType{Bits: 1, Signed: true}

	Int8Type   = &IntegerType{Bits: 8, Signed: true}
	Int16Type  = &IntegerType{Bits: 16, Signed: true}
	Int32Type  = &IntegerType{Bits: 32, Signed: true}
	Int64Type  = &IntegerType{Bits: 64, Signed: true}
	UInt8Type  = &IntegerType{Bits: 8, Signed: false}
	UInt16Type = &IntegerType{Bits: 16, Signed: false}
	UInt32Type = &IntegerType{Bits: 32, Signed: false}
	UInt64Type = &IntegerType{Bits: 64, Signed: false}

	Float32Type = &FloatType{Bits: 32}
	Float64Type = &FloatType{Bits: 64}
)
