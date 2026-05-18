package minir

import (
	"fmt"
	"math"
	"strconv"
)

// ── Constant interface ────────────────────────────────────────────────────────

// Constant is the interface implemented by all compile-time immutable values.
// It extends Value with a Name accessor that the pretty-printer and other
// textual-output paths use instead of digging into unexported fields.
//
// Concrete implementations:
//   - *IntegerConst   – booleans and all integer widths
//   - *FloatConst     – single- and double-precision IEEE 754
//   - *StringConst    – immutable string literals
//   - *NilConst       – null/zero pointer
//   - *AggregateConst – catch-all for records, arrays, and opaque Go values
type Constant interface {
	Value
	// Name returns the optional display label (e.g. "42", "true", "null").
	// Returns "" when no explicit label was supplied; String() is the fallback.
	Name() string
}

// ── IntegerConst ─────────────────────────────────────────────────────────────

// IntegerConst is a compile-time integer constant for any width.
//
//   - Signed == true  → Value is a two's-complement signed integer;
//     its Go equivalent is int64(Value).
//   - Signed == false → Value is unsigned; use Value directly.
//   - BitWidth is 1, 8, 16, 32, or 64.
type IntegerConst struct {
	Value    uint64 // bit pattern; interpret via Signed / BitWidth
	Signed   bool
	BitWidth int
	NameStr  string
	Ty       Type
}

func (c *IntegerConst) Type() Type    { return c.Ty }
func (c *IntegerConst) IsConst() bool { return true }
func (c *IntegerConst) Name() string  { return c.NameStr }
func (c *IntegerConst) String() string {
	if c.NameStr != "" {
		return c.NameStr
	}
	if c.Signed {
		return fmt.Sprintf("%d", int64(c.Value))
	}
	return fmt.Sprintf("%d", c.Value)
}

// AsInt returns the value sign-extended to int64.
func (c *IntegerConst) AsInt() int64 { return int64(c.Value) }

// AsUint returns the raw unsigned bit pattern.
func (c *IntegerConst) AsUint() uint64 { return c.Value }

// ── FloatConst ────────────────────────────────────────────────────────────────

// FloatConst is a compile-time IEEE 754 floating-point constant.
// The value is always stored as float64; single-precision (f32) constants
// lose no information because float32 ⊆ float64.
type FloatConst struct {
	Value   float64
	NameStr string
	Ty      Type
}

func (c *FloatConst) Type() Type    { return c.Ty }
func (c *FloatConst) IsConst() bool { return true }
func (c *FloatConst) Name() string  { return c.NameStr }
func (c *FloatConst) String() string {
	if c.NameStr != "" {
		return c.NameStr
	}
	return strconv.FormatFloat(c.Value, 'g', -1, 64)
}

// ── StringConst ───────────────────────────────────────────────────────────────

// StringConst is a compile-time string literal.
// Its minir type is left as nil by the constructor;
// callers that need a typed operand should set Ty explicitly or wrap the
// value in a GlobalVar with the appropriate array type.
type StringConst struct {
	Value   string
	NameStr string
	Ty      Type // optional; may be nil
}

func (c *StringConst) Type() Type    { return c.Ty }
func (c *StringConst) IsConst() bool { return true }
func (c *StringConst) Name() string  { return c.NameStr }
func (c *StringConst) String() string {
	if c.NameStr != "" {
		return c.NameStr
	}
	return fmt.Sprintf("%q", c.Value)
}

// ── NilConst ─────────────────────────────────────────────────────────────────

// NilConst represents a null / zero-pointer constant.
// Its Ty field must be a *PointerType (or nil, in which case the IR type is
// unresolved).
type NilConst struct {
	NameStr string
	Ty      Type // should be *PointerType
}

func (c *NilConst) Type() Type    { return c.Ty }
func (c *NilConst) IsConst() bool { return true }
func (c *NilConst) Name() string  { return c.NameStr }
func (c *NilConst) String() string {
	if c.NameStr != "" {
		return c.NameStr
	}
	return "null"
}

// ── AggregateConst ────────────────────────────────────────────────────────────

// AggregateConst is a catch-all constant for structured or opaque compile-time
// values (records, arrays, raw Go values) that do not fit the more precise
// types above. The Val field carries the original untyped representation.
type AggregateConst struct {
	Val     interface{}
	NameStr string
	Ty      Type
}

func (c *AggregateConst) Type() Type    { return c.Ty }
func (c *AggregateConst) IsConst() bool { return true }
func (c *AggregateConst) Name() string  { return c.NameStr }
func (c *AggregateConst) String() string {
	if c.NameStr != "" {
		return c.NameStr
	}
	return fmt.Sprintf("%v", c.Val)
}

// ── Typed constructors ────────────────────────────────────────────────────────

// ConstInt creates a signed integer constant. ty must be an integer PrimitiveType.
func ConstInt(name string, v int64, ty Type) *IntegerConst {
	ty = NormalizeType(ty)
	return &IntegerConst{
		Value:    uint64(v),
		Signed:   true,
		BitWidth: bitWidthOf(ty),
		NameStr:  name,
		Ty:       ty,
	}
}

// ConstUint creates an unsigned integer constant. ty must be an integer PrimitiveType.
func ConstUint(name string, v uint64, ty Type) *IntegerConst {
	ty = NormalizeType(ty)
	return &IntegerConst{
		Value:    v,
		Signed:   false,
		BitWidth: bitWidthOf(ty),
		NameStr:  name,
		Ty:       ty,
	}
}

// ConstBool creates a 1-bit boolean constant (false = 0, true = 1).
func ConstBool(name string, v bool) *IntegerConst {
	var u uint64
	if v {
		u = 1
	}
	return &IntegerConst{
		Value:    u,
		Signed:   false,
		BitWidth: 1,
		NameStr:  name,
		Ty:       primI1,
	}
}

// ConstFloat32 creates a single-precision floating-point constant.
// The value is stored as float64 internally (no precision loss).
func ConstFloat32(name string, v float32) *FloatConst {
	return &FloatConst{Value: float64(v), NameStr: name, Ty: primF32}
}

// ConstFloat64 creates a double-precision floating-point constant.
func ConstFloat64(name string, v float64) *FloatConst {
	return &FloatConst{Value: v, NameStr: name, Ty: primF64}
}

// ConstString creates a string constant. Pass a non-empty name to give it a
// printable label; pass "" to fall back to a quoted representation.
// The minir type is left nil; set Ty explicitly if the struct is used as a
// typed IR operand.
func ConstString(name string, v string) *StringConst {
	return &StringConst{Value: v, NameStr: name}
}

// ConstStringTyped creates a string constant with an explicit IR type.
func ConstStringTyped(name string, v string, ty Type) *StringConst {
	return &StringConst{Value: v, NameStr: name, Ty: NormalizeType(ty)}
}

// ConstNil creates a null-pointer constant for the given pointer type.
func ConstNil(name string, ty Type) *NilConst {
	return &NilConst{NameStr: name, Ty: NormalizeType(ty)}
}

// ConstAggregate creates a catch-all constant for records, arrays, or other
// opaque Go values that don't have a more precise constant kind.
func ConstAggregate(name string, val interface{}, ty Type) *AggregateConst {
	return &AggregateConst{Val: val, NameStr: name, Ty: NormalizeType(ty)}
}

// NewConst is the backward-compatible constant factory. It inspects val and ty
// to construct the most precise concrete Constant:
//
//   - integer primitive (i1/i8/i16/i32/i64) → *IntegerConst (signed)
//   - unsigned primitive (u8/u16/u32)        → *IntegerConst (unsigned)
//   - float primitive (f32/f64)              → *FloatConst
//   - pointer type                           → *NilConst (null pointer)
//   - string val with no other match         → *StringConst
//   - anything else                          → *AggregateConst
//
// The return type is the Constant interface so call sites that hold a Value can
// use the result directly without an explicit conversion.
func NewConst(name string, val interface{}, ty Type) Constant {
	nty := NormalizeType(ty)
	switch t := nty.(type) {
	case *PrimitiveType:
		switch t.String() {
		case "i1", "i8", "i16", "i32", "i64":
			return ConstInt(name, toInt64(val), nty)
		case "u8", "u16", "u32":
			return ConstUint(name, toUint64(val), nty)
		case "f32":
			return ConstFloat32(name, float32(toFloat64(val)))
		case "f64":
			return ConstFloat64(name, toFloat64(val))
		}
	case *PointerType:
		return ConstNil(name, nty)
	}
	if sv, ok := val.(string); ok {
		return ConstStringTyped(name, sv, nty)
	}
	return ConstAggregate(name, val, nty)
}

// ── Extraction helpers ────────────────────────────────────────────────────────

// AsInt64 returns the integer value of c sign-extended to int64 and true,
// or 0 and false if c is not an *IntegerConst.
func AsInt64(c Constant) (int64, bool) {
	if ic, ok := c.(*IntegerConst); ok {
		return int64(ic.Value), true
	}
	return 0, false
}

// AsUint64 returns the raw unsigned bit pattern of c and true,
// or 0 and false if c is not an *IntegerConst.
func AsUint64(c Constant) (uint64, bool) {
	if ic, ok := c.(*IntegerConst); ok {
		return ic.Value, true
	}
	return 0, false
}

// AsFloat64 returns the floating-point value of c and true,
// or 0 and false if c is not a *FloatConst.
func AsFloat64(c Constant) (float64, bool) {
	if fc, ok := c.(*FloatConst); ok {
		return fc.Value, true
	}
	return 0, false
}

// AsString returns the string value of c and true,
// or "" and false if c is not a *StringConst.
func AsString(c Constant) (string, bool) {
	if sc, ok := c.(*StringConst); ok {
		return sc.Value, true
	}
	return "", false
}

// IsNilConst reports whether c is a null-pointer constant.
func IsNilConst(c Constant) bool {
	_, ok := c.(*NilConst)
	return ok
}

// ConstAsValue safely converts a Constant to a Value for use in any Value slot.
// This is a no-op at runtime (both are interfaces) but makes intent clear at
// call sites that explicitly want to widen the type.
func ConstAsValue(c Constant) Value { return c }

// ── internal helpers ──────────────────────────────────────────────────────────

// bitWidthOf returns the bit width of a PrimitiveType, defaulting to 64 for
// non-integers and unknown types.
func bitWidthOf(ty Type) int {
	pt, ok := ty.(*PrimitiveType)
	if !ok {
		return 64
	}
	switch pt.String() {
	case "i1":
		return 1
	case "i8", "u8":
		return 8
	case "i16", "u16":
		return 16
	case "i32", "u32":
		return 32
	default:
		return 64
	}
}

// isSigned returns true when the primitive kind represents a signed integer.
func isSigned(pt *PrimitiveType) bool {
	switch pt.String() {
	case "i1", "i8", "i16", "i32", "i64":
		return true
	}
	return false
}

// toInt64 coerces an arbitrary Go value to int64 for use in integer constant
// construction.  Returns 0 when conversion is not possible.
func toInt64(val interface{}) int64 {
	switch x := val.(type) {
	case int64:
		return x
	case int:
		return int64(x)
	case int32:
		return int64(x)
	case int16:
		return int64(x)
	case int8:
		return int64(x)
	case uint64:
		return int64(x)
	case uint32:
		return int64(x)
	case uint16:
		return int64(x)
	case uint8:
		return int64(x)
	case uint:
		return int64(x)
	case bool:
		if x {
			return 1
		}
		return 0
	case string:
		if v, err := strconv.ParseInt(x, 0, 64); err == nil {
			return v
		}
	case float32:
		return int64(x)
	case float64:
		return int64(x)
	}
	// last-resort: format and parse
	if v, err := strconv.ParseInt(fmt.Sprintf("%v", val), 0, 64); err == nil {
		return v
	}
	return 0
}

// toUint64 coerces an arbitrary Go value to uint64.  Returns 0 on failure.
func toUint64(val interface{}) uint64 {
	switch x := val.(type) {
	case uint64:
		return x
	case uint32:
		return uint64(x)
	case uint16:
		return uint64(x)
	case uint8:
		return uint64(x)
	case uint:
		return uint64(x)
	case int64:
		return uint64(x)
	case int:
		return uint64(x)
	case int32:
		return uint64(x)
	case int16:
		return uint64(x)
	case int8:
		return uint64(x)
	case bool:
		if x {
			return 1
		}
		return 0
	case string:
		if v, err := strconv.ParseUint(x, 0, 64); err == nil {
			return v
		}
	}
	if v, err := strconv.ParseUint(fmt.Sprintf("%v", val), 0, 64); err == nil {
		return v
	}
	return 0
}

// toFloat64 coerces an arbitrary Go value to float64.  Returns 0 on failure.
func toFloat64(val interface{}) float64 {
	switch x := val.(type) {
	case float64:
		return x
	case float32:
		return float64(x)
	case int64:
		return float64(x)
	case int:
		return float64(x)
	case int32:
		return float64(x)
	case uint64:
		return float64(x)
	case uint32:
		return float64(x)
	case string:
		if v, err := strconv.ParseFloat(x, 64); err == nil {
			return v
		}
	}
	if v, err := strconv.ParseFloat(fmt.Sprintf("%v", val), 64); err == nil {
		return v
	}
	return math.NaN()
}
