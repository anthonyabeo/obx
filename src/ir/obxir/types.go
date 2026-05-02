package obxir

import "fmt"

// Type is implemented by all OBX IR types.
// Types are a distinct hierarchy from Values: they carry only structural
// information (representation width and printable name) and do NOT implement
// the Value interface.
type Type interface {
	String() string
	Width() int
	// Equal reports whether this type is structurally identical to t.
	Equal(t Type) bool
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

	// PointerType represents both safe OBX pointers and raw C-interop pointers.
	// IsC=true marks a C-interop pointer (previously CPointerType); the Width
	// and codegen representation are identical — the flag exists solely for
	// alias-analysis and FFI-safety annotations.
	PointerType struct {
		Ref Type
		IsC bool // true for C-interop / unsafe pointers
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
func (t *IntegerType) Equal(o Type) bool {
	if ot, ok := o.(*IntegerType); ok {
		return t.Bits == ot.Bits && t.Signed == ot.Signed
	}
	return false
}

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
func (t *FloatType) Equal(o Type) bool {
	if ot, ok := o.(*FloatType); ok {
		return t.Bits == ot.Bits
	}
	return false
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
func (a *ArrayType) Equal(o Type) bool {
	if ot, ok := o.(*ArrayType); ok {
		return a.Len == ot.Len && a.Elem.Equal(ot.Elem)
	}
	return false
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
func (r *RecordType) Equal(o Type) bool {
	ot, ok := o.(*RecordType)
	if !ok || r.Size != ot.Size || len(r.Fields) != len(ot.Fields) {
		return false
	}
	for name, f := range r.Fields {
		of, exists := ot.Fields[name]
		if !exists || f.Offset != of.Offset || !f.Type.Equal(of.Type) {
			return false
		}
	}
	return true
}

// ─── PointerType ──────────────────────────────────────────────────────────

// PointerTo creates a safe OBX pointer to t.
func PointerTo(t Type) *PointerType { return &PointerType{Ref: t} }

// CPointerTo creates a C-interop (unsafe) pointer to t.
// This replaces the former CPointerType.
func CPointerTo(t Type) *PointerType { return &PointerType{Ref: t, IsC: true} }

func (t *PointerType) String() string {
	prefix := "*"
	if t.IsC {
		prefix = "c*"
	}
	if t.Ref == nil {
		return prefix + "void"
	}
	return fmt.Sprintf("%s%s", prefix, t.Ref)
}

// Width always returns the pointer word size (8 bytes for 64-bit targets).
// A pointer's storage width is independent of the referent type.
func (t *PointerType) Width() int { return 8 }

func (t *PointerType) Equal(o Type) bool {
	if ot, ok := o.(*PointerType); ok {
		if t.IsC != ot.IsC {
			return false
		}
		if t.Ref == nil && ot.Ref == nil {
			return true
		}
		if t.Ref == nil || ot.Ref == nil {
			return false
		}
		return t.Ref.Equal(ot.Ref)
	}
	return false
}

// ─── VoidType ─────────────────────────────────────────────────────────────

func (t *VoidType) String() string   { return "void" }
func (t *VoidType) Width() int       { return 0 }
func (t *VoidType) Equal(o Type) bool { _, ok := o.(*VoidType); return ok }

// ─── StringType ───────────────────────────────────────────────────────────

func (t *StringType) String() string { return fmt.Sprintf("string(%d)", t.Length) }
func (t *StringType) Width() int {
	if t.Length == -1 {
		return 16 // dynamic: pointer + length (8 bytes each)
	}
	return t.Length
}
func (t *StringType) Equal(o Type) bool {
	if ot, ok := o.(*StringType); ok {
		return t.Length == ot.Length
	}
	return false
}

// ─── Set ──────────────────────────────────────────────────────────────────

func (t *Set) String() string    { return "set" }
func (t *Set) Width() int        { return 4 }
func (t *Set) Equal(o Type) bool { _, ok := o.(*Set); return ok }

// ─── EnumType ─────────────────────────────────────────────────────────────

func (t *EnumType) String() string { return "enum" }
// EnumType is represented as a 32-bit integer at the IR level.
func (t *EnumType) Width() int { return 4 }
func (t *EnumType) Equal(o Type) bool { _, ok := o.(*EnumType); return ok }

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
