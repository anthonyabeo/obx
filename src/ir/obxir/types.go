package obxir

import "fmt"

// Type is implemented by all OBX IR types.
// Types are a distinct hierarchy from Values: they carry only structural
// information (representation width and printable name) and do NOT implement
// the Value interface.
type Type interface {
	String() string
	Width() int
}

// ─── Concrete types ────────────────────────────────────────────────────────

type (
	IntegerType struct {
		Bits   int
		Signed bool
	}

	FloatType struct {
		Bits int
	}

	ArrayType struct {
		Len  int  // -1 means open (dynamic) array
		Elem Type // element type
	}

	RecordType struct {
		Fields map[string]RecordField
		Size   int
	}

	RecordField struct {
		Name   string
		Offset int
		Type   Type
	}

	PointerType struct {
		Ref Type
	}

	CPointerType struct {
		Ref Type
	}

	VoidType struct{}

	StringType struct {
		Length int // -1 for dynamic
	}

	Set struct{}

	EnumType struct{}
)

// ─── IntegerType ──────────────────────────────────────────────────────────

func (t *IntegerType) String() string {
	if t.Signed {
		return fmt.Sprintf("i%d", t.Bits)
	}
	return fmt.Sprintf("u%d", t.Bits)
}
func (t *IntegerType) Width() int { return t.Bits / 8 }

// ─── FloatType ────────────────────────────────────────────────────────────

func (t *FloatType) String() string { return fmt.Sprintf("f%d", t.Bits) }
func (t *FloatType) Width() int {
	switch t.Bits {
	case 32:
		return 4
	case 64:
		return 8
	default:
		return -1
	}
}

// ─── ArrayType ────────────────────────────────────────────────────────────

func (a *ArrayType) IsOpen() bool { return a.Len == -1 }
func (a *ArrayType) String() string {
	if a.IsOpen() {
		return fmt.Sprintf("[%s]", a.Elem.String())
	}
	return fmt.Sprintf("[%d x %s]", a.Len, a.Elem.String())
}
func (a *ArrayType) Width() int {
	if a.IsOpen() {
		return -1
	}
	base := a.Elem.Width()
	if base == -1 {
		return -1
	}
	return base * a.Len
}
func (a *ArrayType) Dimensions() []int {
	if !a.IsOpen() {
		dims := []int{a.Len}
		if sub, ok := a.Elem.(*ArrayType); ok {
			dims = append(dims, sub.Dimensions()...)
		}
		return dims
	}
	return nil
}
func (a *ArrayType) Strides() []int {
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
func (a *ArrayType) BaseElemSize() int {
	if a.IsOpen() {
		return -1
	}
	if arr, ok := a.Elem.(*ArrayType); ok {
		return arr.BaseElemSize()
	}
	return a.Elem.Width()
}

// ─── RecordType ───────────────────────────────────────────────────────────

func (r *RecordType) String() string { return "record" }
func (r *RecordType) Width() int     { return r.Size }
func (r *RecordType) Field(name string) (RecordField, bool) {
	f, ok := r.Fields[name]
	return f, ok
}

// ─── PointerType ──────────────────────────────────────────────────────────

func PointerTo(t Type) *PointerType { return &PointerType{Ref: t} }
func (t *PointerType) String() string {
	if t.Ref == nil {
		return "*void"
	}
	return fmt.Sprintf("*%s", t.Ref)
}
func (t *PointerType) Width() int {
	if t.Ref == nil {
		return 0
	}
	return 8 // 64-bit pointer
}

// ─── CPointerType ──────────────────────────────────────────────────────────

func CPointerTo(t Type) *CPointerType { return &CPointerType{Ref: t} }
func (t *CPointerType) String() string {
	if t.Ref == nil {
		return "*void"
	}
	return fmt.Sprintf("*%s", t.Ref)
}
func (t *CPointerType) Width() int {
	if t.Ref == nil {
		return 0
	}
	return 8 // 64-bit pointer
}

// ─── VoidType ─────────────────────────────────────────────────────────────

func (t *VoidType) String() string { return "void" }
func (t *VoidType) Width() int     { return 0 }

// ─── StringType ───────────────────────────────────────────────────────────

func (t *StringType) String() string { return fmt.Sprintf("string(%d)", t.Length) }
func (t *StringType) Width() int {
	if t.Length == -1 {
		return 16 // dynamic: pointer + length (8 bytes each)
	}
	return t.Length
}

// ─── Set ──────────────────────────────────────────────────────────────────

func (t *Set) String() string { return "set" }
func (t *Set) Width() int     { return 4 }

// ─── EnumType ─────────────────────────────────────────────────────────────

func (t *EnumType) String() string { return "enum" }
func (t *EnumType) Width() int     { panic("EnumType: Width not implemented") }

// ─── Predefined type singletons ───────────────────────────────────────────

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

	Void    = &VoidType{}
	SetType = &Set{}
)
