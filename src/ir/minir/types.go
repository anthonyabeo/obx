package minir

import "fmt"

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
		if t.Ty != nil {
			return fmt.Sprintf("%%%s:%s", t.NameStr, t.Ty.String())
		}
		return fmt.Sprintf("%%%s", t.NameStr)
	}
	if t.Ty != nil {
		return fmt.Sprintf("%%t%d:%s", t.ID, t.Ty.String())
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
	if c.NameStr != "" {
		return c.NameStr
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

func I1() *PrimitiveType  { return primI1 }
func I32() *PrimitiveType { return primI32 }
func I64() *PrimitiveType { return primI64 }
func F32() *PrimitiveType { return primF32 }
func F64() *PrimitiveType { return primF64 }
func Bool() *PrimitiveType { return primI1 }

// Temp ID generator for NewTemp/NewAnonTemp
var tempIDCounter int

func nextTempID() int {
	tempIDCounter++
	return tempIDCounter
}

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
	return &Constant{NameStr: name, Val: val, Ty: NormalizeType(ty)}
}

// pointer cache for canonical pointer types (keyed by element type string)
var ptrCache = map[string]*PointerType{}

// Ptr returns a canonical PointerType for elem (normalizes elem first).
func Ptr(elem Type) *PointerType {
	if elem == nil { return &PointerType{Elem: nil} }
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
	if t == nil { return nil }
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
		for i, p := range tt.Params { np[i] = NormalizeType(p) }
		nr := NormalizeType(tt.Result)
		return &FunctionType{Params: np, Result: nr}
	default:
		return t
	}
}

