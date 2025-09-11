package mir

import (
	"fmt"
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
		Len  int
		Elem Type // Element type of the array
	}

	PointerType struct {
		Ref Type
	}

	VoidType string
)

func (a ArrayType) IsOpen() bool { return a.Len == -1 }
func (a ArrayType) String() string {
	if a.IsOpen() {
		return fmt.Sprintf("[%s]", a.Elem.String())
	}
	return fmt.Sprintf("[%d x %s]", a.Len, a.Elem.String())
}
func (a ArrayType) Width() int {
	if a.IsOpen() {
		return -1 // Open arrays have undefined width
	}

	baseWidth := a.Elem.Width()
	if baseWidth == -1 {
		return -1 // If base type has undefined width, return -1
	}

	totalWidth := baseWidth * a.Len
	return totalWidth
}
func (a ArrayType) Dimensions() []int {
	if !a.IsOpen() {
		dims := []int{a.Len}
		if sub, ok := a.Elem.(*ArrayType); ok {
			dims = append(dims, sub.Dimensions()...)
		}

		return dims
	}

	return nil
}
func (a ArrayType) Strides() []int {
	dims := a.Dimensions()
	n := len(dims)
	strides := make([]int, n)
	stride := a.BaseElemSize()
	for i := n - 1; i >= 0; i-- {
		strides[i] = stride
		stride *= dims[i]
	}
	return strides
}
func (a ArrayType) BaseElemSize() int {
	if a.IsOpen() {
		return -1 // Open arrays have undefined base element size
	}

	if arr, ok := a.Elem.(*ArrayType); ok {
		return arr.BaseElemSize()
	}

	return a.Elem.Width()
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
