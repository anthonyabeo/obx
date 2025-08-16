package mir

import (
	"fmt"
	"strings"
)

type Type interface {
	String() string
	Width() int
}

type (
	IntegerType struct {
		Bits   int
		Signed bool
	}

	FloatType struct {
		Bits int
	}

	ArrayType struct {
		Dimns     []int64 // Dimensions of the array, -1 for open arrays
		Elem      Type    // Element type of the array
		ElemWidth int
	}

	PointerType struct {
		Ref Type
	}

	VoidType string
)

func (a ArrayType) IsOpen() bool {
	return len(a.Dimns) == 1 && a.Dimns[0] == -1
}
func (a ArrayType) String() string {
	if a.IsOpen() {
		return fmt.Sprintf("[%s]", a.Elem.String())
	}

	var sb strings.Builder
	for i := 0; i < len(a.Dimns); i++ {
		sb.WriteString(fmt.Sprintf("[%d x ", a.Dimns[i]))
	}
	sb.WriteString(fmt.Sprintf("%s", a.Elem.String()))

	for i := 0; i < len(a.Dimns); i++ {
		sb.WriteString("]")
	}
	return sb.String()

}
func (a ArrayType) Width() int {
	if a.IsOpen() {
		return -1 // Open array, width is not defined
	}

	baseWidth := a.Elem.Width()
	if baseWidth == -1 {
		return -1 // If base type has undefined width, return -1
	}

	// Use all dimensions to calculate total width
	total := int64(1)
	for _, dim := range a.Dimns {
		if dim == -1 {
			return -1 // Open dimension, width is not defined
		}
		total *= dim
	}
	return int(total) * baseWidth
}

func (t *IntegerType) String() string {
	if t.Signed {
		return fmt.Sprintf("i%d", t.Bits)
	}

	return fmt.Sprintf("u%d", t.Bits)
}
func (t *IntegerType) Width() int { return t.Bits / 8 }

func (t *PointerType) String() string { return fmt.Sprintf("*%s", t.Ref) }
func (t *PointerType) Width() int {
	if t.Ref == nil {
		return 0 // Pointer to nothing, width is 0
	}
	return 8 // Assuming 64-bit pointers, adjust as necessary
}

func (t *FloatType) String() string { return fmt.Sprintf("f%d", t.Bits) }
func (t *FloatType) Width() int {
	switch t.Bits {
	case 32:
		return 4
	case 64:
		return 8
	default:
		return -1 // Unsupported float size
	}
}

func (t VoidType) String() string { return "void" }
func (t VoidType) Width() int     { return 0 /* Void type has no width*/ }

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

	Void = VoidType("void")
)
