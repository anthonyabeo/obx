package minir

import (
	"fmt"
	"strconv"
)

// Minimal types for a compact SSA 3AC IR.

// Type is the interface implemented by all IR types.
type Type interface {
	String() string
	Equal(Type) bool
}

// PrimitiveType represents a simple machine primitive type (i32, i64, f32, f64, bool).
type PrimitiveType struct {
	Name string
}

func (p *PrimitiveType) String() string {
	switch p.Name {
	case "bool", "b":
		return "i1"
	case "i1":
		return "i1"
	case "i32", "int32", "int":
		return "i32"
	case "i64", "int64":
		return "i64"
	case "f32", "float32", "float":
		return "f32"
	case "f64", "double", "float64":
		return "f64"
	default:
		return p.Name
	}
}
func (p *PrimitiveType) Equal(t Type) bool {
	if o, ok := t.(*PrimitiveType); ok {
		return p.Name == o.Name
	}
	return false
}

// PointerType represents a pointer to an element type.
type PointerType struct {
	Elem Type
}

func (p *PointerType) String() string { return "ptr." + p.Elem.String() }
func (p *PointerType) Equal(t Type) bool {
	if o, ok := t.(*PointerType); ok {
		return p.Elem.Equal(o.Elem)
	}
	return false
}

// FunctionType represents a function signature.
type FunctionType struct {
	Params []Type
	Result Type // nil means void
}

func (f *FunctionType) String() string {
	return "func"
}
func (f *FunctionType) Equal(t Type) bool {
	if o, ok := t.(*FunctionType); ok {
		if (f.Result == nil) != (o.Result == nil) {
			return false
		}
		if f.Result != nil && !f.Result.Equal(o.Result) {
			return false
		}
		if len(f.Params) != len(o.Params) {
			return false
		}
		for i := range f.Params {
			if !f.Params[i].Equal(o.Params[i]) {
				return false
			}
		}
		return true
	}
	return false
}

// RecordField is a single named field slot inside a RecordType.
type RecordField struct {
	Name   string // field name
	Type   Type   // field type (normalized)
	Offset int    // byte offset within the record
}

// RecordType is an ordered, named aggregate of fields (struct / record).
// TypeName is optional; when non-empty it is used in String() for brevity.
type RecordType struct {
	TypeName string        // optional symbolic name, e.g. "Point"
	Fields   []RecordField // in declaration order
}

func (r *RecordType) String() string {
	if r.TypeName != "" {
		return "record." + r.TypeName
	}
	s := "record{"
	for i, f := range r.Fields {
		if i > 0 {
			s += ", "
		}
		ty := "<nil>"
		if f.Type != nil {
			ty = f.Type.String()
		}
		s += f.Name + ":" + ty
	}
	return s + "}"
}
func (r *RecordType) Equal(t Type) bool {
	o, ok := t.(*RecordType)
	if !ok {
		return false
	}
	// Two named record types are equal when their names match.
	if r.TypeName != "" || o.TypeName != "" {
		return r.TypeName == o.TypeName
	}
	// Anonymous record types: structural equality.
	if len(r.Fields) != len(o.Fields) {
		return false
	}
	for i := range r.Fields {
		a, b := r.Fields[i], o.Fields[i]
		if a.Name != b.Name || a.Offset != b.Offset {
			return false
		}
		if (a.Type == nil) != (b.Type == nil) {
			return false
		}
		if a.Type != nil && !a.Type.Equal(b.Type) {
			return false
		}
	}
	return true
}

// FieldIndex returns the 0-based index of the field named name, or -1 if not found.
func (r *RecordType) FieldIndex(name string) int {
	for i, f := range r.Fields {
		if f.Name == name {
			return i
		}
	}
	return -1
}

// NewRecordType constructs a RecordType and normalizes each field's type.
func NewRecordType(name string, fields []RecordField) *RecordType {
	norm := make([]RecordField, len(fields))
	for i, f := range fields {
		norm[i] = RecordField{Name: f.Name, Type: NormalizeType(f.Type), Offset: f.Offset}
	}
	return &RecordType{TypeName: name, Fields: norm}
}

// ArrayType is a fixed-length array whose elements all have the same type.
// Len == 0 represents an open / unsized array.
type ArrayType struct {
	Len  int  // number of elements
	Elem Type // element type (normalized)
}

func (a *ArrayType) String() string {
	elem := "<nil>"
	if a.Elem != nil {
		elem = a.Elem.String()
	}
	return fmt.Sprintf("[%d x %s]", a.Len, elem)
}

func (a *ArrayType) Equal(t Type) bool {
	o, ok := t.(*ArrayType)
	if !ok {
		return false
	}
	if a.Len != o.Len {
		return false
	}
	if (a.Elem == nil) != (o.Elem == nil) {
		return false
	}
	if a.Elem != nil && !a.Elem.Equal(o.Elem) {
		return false
	}
	return true
}

// NewArrayType constructs an ArrayType and normalizes the element type.
func NewArrayType(length int, elem Type) *ArrayType {
	return &ArrayType{Len: length, Elem: NormalizeType(elem)}
}

// Temp is an SSA local / value produced by an instruction.
type Temp struct {
	ID      int
	NameStr string
	Ty      Type
	IsAddr  bool // whether this temp represents an address
}

func (t *Temp) Type() Type    { return t.Ty }
func (t *Temp) Name() string  { return t.NameStr }
func (t *Temp) IsMem() bool   { return t.IsAddr }
func (t *Temp) IsConst() bool { return false }
func (t *Temp) String() string {
	if t == nil {
		return "<nil>"
	}
	if t.NameStr != "" {
		return fmt.Sprintf("%%%s", t.NameStr)
	}
	return fmt.Sprintf("%%t%d", t.ID)
}

// Value represents any IR value that can be used as an operand: either a
// Temp (SSA definition) or a Constant.
type Value interface {
	Type() Type
	String() string
	IsConst() bool
}

// Constant is an immutable immediate value.
type Constant struct {
	NameStr string // optional name for printing (e.g., "42" or "C0")
	Val     interface{}
	Ty      Type
}

func (c *Constant) Type() Type { return c.Ty }
func (c *Constant) String() string {
	if c == nil {
		return "<nil>"
	}
	if c.NameStr != "" {
		return fmt.Sprintf("%s", c.NameStr)
	}
	return fmt.Sprintf("%v", c.Val)
}
func (c *Constant) IsConst() bool { return true }

// Canonical primitive singletons and factory functions.
var (
	primI1  = &PrimitiveType{Name: "i1"}
	primI32 = &PrimitiveType{Name: "i32"}
	primI64 = &PrimitiveType{Name: "i64"}
	primF32 = &PrimitiveType{Name: "f32"}
	primF64 = &PrimitiveType{Name: "f64"}
)

func I1() *PrimitiveType   { return primI1 }
func I32() *PrimitiveType  { return primI32 }
func I64() *PrimitiveType  { return primI64 }
func F32() *PrimitiveType  { return primF32 }
func F64() *PrimitiveType  { return primF64 }
func Bool() *PrimitiveType { return primI1 }

// Temp ID generator for NewTemp/NewAnonTemp
var tempIDCounter int

func nextTempID() int {
	tempIDCounter++
	return tempIDCounter
}

// ResetTempCounter resets the package-level temp ID counter to zero.
// Exported so that external test packages (e.g. miniropt_test) can produce
// deterministic temp IDs without being inside the minir package.
func ResetTempCounter() { tempIDCounter = 0 }

// NewTemp creates a Temp with the given name and (canonicalized) type.
// The temp is assigned a fresh unique ID.
func NewTemp(name string, ty Type) *Temp {
	return &Temp{ID: nextTempID(), NameStr: name, Ty: NormalizeType(ty)}
}

// NewAnonTemp creates an anonymous Temp (no NameStr) with a fresh ID.
func NewAnonTemp(ty Type) *Temp {
	return &Temp{ID: nextTempID(), Ty: NormalizeType(ty)}
}

// NewConst constructs a Constant. If name is empty the textual value is
// left blank; caller may set NameStr if desired.
func NewConst(name string, val interface{}, ty Type) *Constant {
	nty := NormalizeType(ty)
	// Canonicalize concrete Go representation of the value according to the
	// normalized type so later consumers can rely on predictable concrete
	// types (e.g. integer constants -> int64, floats -> float64).
	var v interface{} = val
	switch t := nty.(type) {
	case *PrimitiveType:
		switch t.String() {
		case "i1", "i32", "i64":
			// canonicalize integers to int64 when possible
			switch x := val.(type) {
			case int64:
				v = x
			case int:
				v = int64(x)
			case int32:
				v = int64(x)
			case int16:
				v = int64(x)
			case int8:
				v = int64(x)
			case uint64:
				v = int64(x)
			case uint32:
				v = int64(x)
			case uint16:
				v = int64(x)
			case uint8:
				v = int64(x)
			case string:
				if iv, err := strconv.ParseInt(x, 10, 64); err == nil {
					v = iv
				} else {
					// fallback to formatting parse
					if iv2, err2 := strconv.ParseInt(fmt.Sprintf("%v", val), 10, 64); err2 == nil {
						v = iv2
					}
				}
			default:
				// try formatted parse
				if iv, err := strconv.ParseInt(fmt.Sprintf("%v", val), 10, 64); err == nil {
					v = iv
				}
			}
		case "f32", "f64":
			// canonicalize floats to float64
			switch x := val.(type) {
			case float64:
				v = x
			case float32:
				v = float64(x)
			case string:
				if fv, err := strconv.ParseFloat(x, 64); err == nil {
					v = fv
				}
			default:
				// try formatted parse
				if fv, err := strconv.ParseFloat(fmt.Sprintf("%v", val), 64); err == nil {
					v = fv
				}
			}
		default:
			// leave as-is for other primitive kinds
			v = val
		}
	case *PointerType:
		// pointer immediates: represent as int64 (zero/null) when possible
		switch x := val.(type) {
		case int64:
			v = x
		case int:
			v = int64(x)
		case string:
			if iv, err := strconv.ParseInt(x, 10, 64); err == nil {
				v = iv
			}
		default:
			// leave as-is
			v = val
		}
	default:
		// For arrays/records/other aggregates keep the original representation
		v = val
	}
	return &Constant{NameStr: name, Val: v, Ty: nty}
}

// ConstantToInt attempts to convert a *Constant's Val to int. It returns an
// error when conversion is not possible. It prefers the canonical concrete
// representation produced by NewConst (int64 for integer types).
func ConstantToInt(c *Constant) (int, error) {
	if c == nil {
		return 0, fmt.Errorf("nil constant")
	}
	switch v := c.Val.(type) {
	case int:
		return v, nil
	case int8:
		return int(v), nil
	case int16:
		return int(v), nil
	case int32:
		return int(v), nil
	case int64:
		return int(v), nil
	case uint:
		return int(v), nil
	case uint8:
		return int(v), nil
	case uint16:
		return int(v), nil
	case uint32:
		return int(v), nil
	case uint64:
		return int(v), nil
	case string:
		if n, err := strconv.Atoi(v); err == nil {
			return n, nil
		}
		if n, err := strconv.Atoi(fmt.Sprintf("%v", v)); err == nil {
			return n, nil
		}
		return 0, fmt.Errorf("cannot parse string constant %q as int", v)
	default:
		// fallback: try to format and parse
		if n, err := strconv.Atoi(fmt.Sprintf("%v", v)); err == nil {
			return n, nil
		}
		return 0, fmt.Errorf("unsupported constant type %T (value=%v)", v, v)
	}
}

// pointer cache for canonical pointer types (keyed by element type string)
var ptrCache = map[string]*PointerType{}

// Ptr returns a canonical PointerType for elem (normalizes elem first).
func Ptr(elem Type) *PointerType {
	if elem == nil {
		return &PointerType{Elem: nil}
	}
	ne := NormalizeType(elem)
	key := ne.String()
	if p, ok := ptrCache[key]; ok {
		return p
	}
	p := &PointerType{Elem: ne}
	ptrCache[key] = p
	return p
}

// NormalizeType returns a canonical Type: primitives are mapped to singletons
// and pointer element types are normalized recursively.
func NormalizeType(t Type) Type {
	if t == nil {
		return nil
	}
	switch tt := t.(type) {
	case *PrimitiveType:
		switch tt.String() {
		case "i1":
			return primI1
		case "i32":
			return primI32
		case "i64":
			return primI64
		case "f32":
			return primF32
		case "f64":
			return primF64
		default:
			return tt
		}
	case *PointerType:
		return Ptr(NormalizeType(tt.Elem))
	case *FunctionType:
		// normalize params and result
		np := make([]Type, len(tt.Params))
		for i, p := range tt.Params {
			np[i] = NormalizeType(p)
		}
		nr := NormalizeType(tt.Result)
		return &FunctionType{Params: np, Result: nr}
	case *RecordType:
		return NewRecordType(tt.TypeName, tt.Fields)
	case *ArrayType:
		return NewArrayType(tt.Len, tt.Elem)
	default:
		return t
	}
}
