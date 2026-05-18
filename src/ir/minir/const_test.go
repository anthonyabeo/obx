package core

import (
	"math"
	"testing"
)

// ── IntegerConst ──────────────────────────────────────────────────────────────

func TestConstInt_SignedPositive(t *testing.T) {
	c := ConstInt("42", 42, I32())
	if c.Value != 42 {
		t.Errorf("Value: want 42, got %d", c.Value)
	}
	if !c.Signed {
		t.Error("Signed: want true")
	}
	if c.BitWidth != 32 {
		t.Errorf("BitWidth: want 32, got %d", c.BitWidth)
	}
	if c.Type() != primI32 {
		t.Errorf("Type: want i32, got %s", c.Type())
	}
	if c.Name() != "42" {
		t.Errorf("Name: want 42, got %q", c.Name())
	}
	if c.String() != "42" {
		t.Errorf("String: want '42', got %q", c.String())
	}
	if !c.IsConst() {
		t.Error("IsConst: want true")
	}
}

func TestConstInt_SignedNegative(t *testing.T) {
	c := ConstInt("", -1, I64())
	if int64(c.Value) != -1 {
		t.Errorf("Value: want -1, got %d", int64(c.Value))
	}
	if c.String() != "-1" {
		t.Errorf("String: want '-1', got %q", c.String())
	}
	if c.Name() != "" {
		t.Errorf("Name: want empty, got %q", c.Name())
	}
}

func TestConstUint(t *testing.T) {
	c := ConstUint("255", 255, U8())
	if c.Value != 255 {
		t.Errorf("Value: want 255, got %d", c.Value)
	}
	if c.Signed {
		t.Error("Signed: want false")
	}
	if c.BitWidth != 8 {
		t.Errorf("BitWidth: want 8, got %d", c.BitWidth)
	}
	if c.String() != "255" {
		t.Errorf("String: want '255', got %q", c.String())
	}
}

func TestConstBool(t *testing.T) {
	tr := ConstBool("true", true)
	fa := ConstBool("false", false)

	if tr.Value != 1 {
		t.Errorf("true Value: want 1, got %d", tr.Value)
	}
	if fa.Value != 0 {
		t.Errorf("false Value: want 0, got %d", fa.Value)
	}
	if tr.BitWidth != 1 || fa.BitWidth != 1 {
		t.Error("BitWidth: want 1 for both")
	}
	if tr.Type() != primI1 || fa.Type() != primI1 {
		t.Error("Type: want i1 for both")
	}
}

// ── FloatConst ────────────────────────────────────────────────────────────────

func TestConstFloat64(t *testing.T) {
	c := ConstFloat64("pi", math.Pi)
	if c.Value != math.Pi {
		t.Errorf("Value: want %v, got %v", math.Pi, c.Value)
	}
	if c.Type() != primF64 {
		t.Errorf("Type: want f64, got %s", c.Type())
	}
	if c.Name() != "pi" {
		t.Errorf("Name: want 'pi', got %q", c.Name())
	}
	if !c.IsConst() {
		t.Error("IsConst: want true")
	}
}

func TestConstFloat32(t *testing.T) {
	c := ConstFloat32("", 1.5)
	if c.Value != float64(float32(1.5)) {
		t.Errorf("Value: want %v, got %v", float64(float32(1.5)), c.Value)
	}
	if c.Type() != primF32 {
		t.Errorf("Type: want f32, got %s", c.Type())
	}
}

// ── StringConst ───────────────────────────────────────────────────────────────

func TestConstString(t *testing.T) {
	c := ConstString("hello", "hello world")
	if c.Value != "hello world" {
		t.Errorf("Value: want 'hello world', got %q", c.Value)
	}
	if c.Name() != "hello" {
		t.Errorf("Name: want 'hello', got %q", c.Name())
	}
	if c.String() != "hello" {
		t.Errorf("String: want 'hello', got %q", c.String())
	}
}

func TestConstString_NoName(t *testing.T) {
	c := ConstString("", "test")
	// Without a name, String() should return quoted value
	if c.String() != `"test"` {
		t.Errorf("String: want '\"test\"', got %q", c.String())
	}
}

// ── NilConst ─────────────────────────────────────────────────────────────────

func TestConstNil(t *testing.T) {
	c := ConstNil("null", Ptr(I32()))
	if c.Type() == nil {
		t.Error("Type: want non-nil")
	}
	if c.String() != "null" {
		t.Errorf("String: want 'null', got %q", c.String())
	}
	if c.Name() != "null" {
		t.Errorf("Name: want 'null', got %q", c.Name())
	}
	if !IsNilConst(c) {
		t.Error("IsNilConst: want true")
	}
}

func TestConstNil_AnonymousString(t *testing.T) {
	c := ConstNil("", Ptr(I64()))
	if c.String() != "null" {
		t.Errorf("String: want 'null', got %q", c.String())
	}
}

// ── AggregateConst ────────────────────────────────────────────────────────────

func TestConstAggregate(t *testing.T) {
	c := ConstAggregate("agg", []int{1, 2, 3}, NewArrayType(3, I32()))
	if c.Name() != "agg" {
		t.Errorf("Name: want 'agg', got %q", c.Name())
	}
	if c.Val == nil {
		t.Error("Val: want non-nil")
	}
	if !c.IsConst() {
		t.Error("IsConst: want true")
	}
}

// ── Constant interface ────────────────────────────────────────────────────────

func TestConstantInterface_IsImplementedByAllTypes(t *testing.T) {
	var _ Constant = ConstInt("", 0, I32())
	var _ Constant = ConstUint("", 0, U8())
	var _ Constant = ConstBool("", false)
	var _ Constant = ConstFloat32("", 0)
	var _ Constant = ConstFloat64("", 0)
	var _ Constant = ConstString("", "")
	var _ Constant = ConstNil("", Ptr(I32()))
	var _ Constant = ConstAggregate("", nil, I32())
}

func TestConstantInterface_IsValue(t *testing.T) {
	var _ Value = ConstInt("", 42, I32())
	var _ Value = ConstFloat64("", 3.14)
	var _ Value = ConstNil("null", Ptr(I32()))
}

// ── Extraction helpers ────────────────────────────────────────────────────────

func TestAsInt64(t *testing.T) {
	ic := ConstInt("", -99, I64())
	if v, ok := AsInt64(ic); !ok || v != -99 {
		t.Errorf("AsInt64(IntegerConst -99): got %d, ok=%v", v, ok)
	}
	fc := ConstFloat64("", 3.14)
	if _, ok := AsInt64(fc); ok {
		t.Error("AsInt64(FloatConst): want false")
	}
	nc := ConstNil("", Ptr(I32()))
	if _, ok := AsInt64(nc); ok {
		t.Error("AsInt64(NilConst): want false")
	}
}

func TestAsUint64(t *testing.T) {
	uc := ConstUint("", 255, U8())
	if v, ok := AsUint64(uc); !ok || v != 255 {
		t.Errorf("AsUint64(IntegerConst 255): got %d, ok=%v", v, ok)
	}
	if _, ok := AsUint64(ConstFloat64("", 1.0)); ok {
		t.Error("AsUint64(FloatConst): want false")
	}
}

func TestAsFloat64(t *testing.T) {
	fc := ConstFloat64("", math.Pi)
	if v, ok := AsFloat64(fc); !ok || v != math.Pi {
		t.Errorf("AsFloat64: got %v, ok=%v", v, ok)
	}
	if _, ok := AsFloat64(ConstInt("", 1, I32())); ok {
		t.Error("AsFloat64(IntegerConst): want false")
	}
}

func TestAsString(t *testing.T) {
	sc := ConstString("", "hello")
	if v, ok := AsString(sc); !ok || v != "hello" {
		t.Errorf("AsString: got %q, ok=%v", v, ok)
	}
	if _, ok := AsString(ConstInt("", 0, I32())); ok {
		t.Error("AsString(IntegerConst): want false")
	}
}

func TestIsNilConst(t *testing.T) {
	if !IsNilConst(ConstNil("", Ptr(I32()))) {
		t.Error("IsNilConst(NilConst): want true")
	}
	if IsNilConst(ConstInt("", 0, I32())) {
		t.Error("IsNilConst(IntegerConst): want false")
	}
}

// ── NewConst backward-compat factory ─────────────────────────────────────────

func TestNewConst_Integer(t *testing.T) {
	c := NewConst("42", int64(42), I32())
	ic, ok := c.(*IntegerConst)
	if !ok {
		t.Fatalf("NewConst integer: want *IntegerConst, got %T", c)
	}
	if int64(ic.Value) != 42 {
		t.Errorf("Value: want 42, got %d", ic.Value)
	}
	if !ic.Signed {
		t.Error("Signed: want true for i32")
	}
}

func TestNewConst_UnsignedInt(t *testing.T) {
	c := NewConst("5", int64(5), U8())
	uc, ok := c.(*IntegerConst)
	if !ok {
		t.Fatalf("NewConst unsigned: want *IntegerConst, got %T", c)
	}
	if uc.Signed {
		t.Error("Signed: want false for u8")
	}
}

func TestNewConst_Float(t *testing.T) {
	c := NewConst("1.5", float64(1.5), F64())
	fc, ok := c.(*FloatConst)
	if !ok {
		t.Fatalf("NewConst float: want *FloatConst, got %T", c)
	}
	if fc.Value != 1.5 {
		t.Errorf("Value: want 1.5, got %v", fc.Value)
	}
}

func TestNewConst_Pointer(t *testing.T) {
	c := NewConst("null", int64(0), Ptr(I32()))
	nc, ok := c.(*NilConst)
	if !ok {
		t.Fatalf("NewConst pointer: want *NilConst, got %T", c)
	}
	if nc.Name() != "null" {
		t.Errorf("Name: want 'null', got %q", nc.Name())
	}
}

func TestNewConst_StringVal(t *testing.T) {
	c := NewConst("str", "hello", NewArrayType(6, I32()))
	sc, ok := c.(*StringConst)
	if !ok {
		t.Fatalf("NewConst string val: want *StringConst, got %T", c)
	}
	if sc.Value != "hello" {
		t.Errorf("Value: want 'hello', got %q", sc.Value)
	}
}

// ── ValueString / ShortValueString ───────────────────────────────────────────

func TestValueString_Constant(t *testing.T) {
	c := ConstInt("answer", 42, I32())
	if s := ValueString(c); s != "answer" {
		t.Errorf("ValueString named: want 'answer', got %q", s)
	}
	c2 := ConstInt("", 7, I32())
	if s := ValueString(c2); s != "7" {
		t.Errorf("ValueString unnamed: want '7', got %q", s)
	}
}

func TestShortValueString_Constant(t *testing.T) {
	c := ConstFloat64("zero", 0)
	if s := ShortValueString(c); s != "zero" {
		t.Errorf("ShortValueString named: want 'zero', got %q", s)
	}
}
