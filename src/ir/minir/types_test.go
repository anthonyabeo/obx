package core

import (
	"testing"
)

// ── RecordType ────────────────────────────────────────────────────────────────

func TestRecordTypeString_Named(t *testing.T) {
	r := NewRecordType("Point", []RecordField{
		{Name: "x", Type: I32(), Offset: 0},
		{Name: "y", Type: I32(), Offset: 4},
	})
	want := "record.Point"
	if got := r.String(); got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}
}

func TestRecordTypeString_Anonymous(t *testing.T) {
	r := NewRecordType("", []RecordField{
		{Name: "a", Type: I32(), Offset: 0},
		{Name: "b", Type: F64(), Offset: 8},
	})
	want := "record{a:i32, b:f64}"
	if got := r.String(); got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}
}

func TestRecordTypeEqual_Named(t *testing.T) {
	r1 := NewRecordType("Point", []RecordField{{Name: "x", Type: I32(), Offset: 0}})
	r2 := NewRecordType("Point", []RecordField{{Name: "x", Type: I32(), Offset: 0}})
	r3 := NewRecordType("Rect", []RecordField{{Name: "x", Type: I32(), Offset: 0}})
	if !r1.Equal(r2) {
		t.Error("same-named records should be equal")
	}
	if r1.Equal(r3) {
		t.Error("differently-named records should not be equal")
	}
}

func TestRecordTypeEqual_Anonymous(t *testing.T) {
	r1 := NewRecordType("", []RecordField{
		{Name: "a", Type: I32(), Offset: 0},
		{Name: "b", Type: I64(), Offset: 8},
	})
	r2 := NewRecordType("", []RecordField{
		{Name: "a", Type: I32(), Offset: 0},
		{Name: "b", Type: I64(), Offset: 8},
	})
	r3 := NewRecordType("", []RecordField{
		{Name: "a", Type: I32(), Offset: 0},
		{Name: "c", Type: I64(), Offset: 8}, // different field name
	})
	r4 := NewRecordType("", []RecordField{
		{Name: "a", Type: I32(), Offset: 0},
		{Name: "b", Type: I64(), Offset: 16}, // different offset
	})
	if !r1.Equal(r2) {
		t.Error("structurally identical anonymous records should be equal")
	}
	if r1.Equal(r3) {
		t.Error("records with different field names should not be equal")
	}
	if r1.Equal(r4) {
		t.Error("records with different field offsets should not be equal")
	}
}

func TestRecordTypeEqual_DifferentFieldCount(t *testing.T) {
	r1 := NewRecordType("", []RecordField{{Name: "a", Type: I32(), Offset: 0}})
	r2 := NewRecordType("", []RecordField{
		{Name: "a", Type: I32(), Offset: 0},
		{Name: "b", Type: I32(), Offset: 4},
	})
	if r1.Equal(r2) {
		t.Error("records with different field counts should not be equal")
	}
}

func TestRecordTypeEqual_CrossKind(t *testing.T) {
	r := NewRecordType("T", []RecordField{{Name: "x", Type: I32(), Offset: 0}})
	if r.Equal(I32()) {
		t.Error("RecordType should not equal a PrimitiveType")
	}
}

func TestRecordType_FieldIndex(t *testing.T) {
	r := NewRecordType("P", []RecordField{
		{Name: "x", Type: I32(), Offset: 0},
		{Name: "y", Type: I32(), Offset: 4},
		{Name: "z", Type: I32(), Offset: 8},
	})
	if got := r.FieldIndex("x"); got != 0 {
		t.Errorf("FieldIndex(x) = %d, want 0", got)
	}
	if got := r.FieldIndex("y"); got != 1 {
		t.Errorf("FieldIndex(y) = %d, want 1", got)
	}
	if got := r.FieldIndex("z"); got != 2 {
		t.Errorf("FieldIndex(z) = %d, want 2", got)
	}
	if got := r.FieldIndex("missing"); got != -1 {
		t.Errorf("FieldIndex(missing) = %d, want -1", got)
	}
}

// ── ArrayType ─────────────────────────────────────────────────────────────────

func TestArrayTypeString(t *testing.T) {
	a := NewArrayType(4, I32())
	want := "[4 x i32]"
	if got := a.String(); got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}
}

func TestArrayTypeString_Open(t *testing.T) {
	a := NewArrayType(0, F64())
	want := "[0 x f64]"
	if got := a.String(); got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}
}

func TestArrayTypeString_NestedArray(t *testing.T) {
	inner := NewArrayType(4, I32())
	outer := NewArrayType(3, inner)
	want := "[3 x [4 x i32]]"
	if got := outer.String(); got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}
}

func TestArrayTypeEqual(t *testing.T) {
	a1 := NewArrayType(4, I32())
	a2 := NewArrayType(4, I32())
	a3 := NewArrayType(8, I32())
	a4 := NewArrayType(4, I64())
	if !a1.Equal(a2) {
		t.Error("same length+elem arrays should be equal")
	}
	if a1.Equal(a3) {
		t.Error("different-length arrays should not be equal")
	}
	if a1.Equal(a4) {
		t.Error("different-elem arrays should not be equal")
	}
}

func TestArrayTypeEqual_CrossKind(t *testing.T) {
	a := NewArrayType(4, I32())
	if a.Equal(I32()) {
		t.Error("ArrayType should not equal a PrimitiveType")
	}
}

func TestArrayTypeEqual_Nested(t *testing.T) {
	a1 := NewArrayType(3, NewArrayType(4, I32()))
	a2 := NewArrayType(3, NewArrayType(4, I32()))
	a3 := NewArrayType(3, NewArrayType(4, I64()))
	if !a1.Equal(a2) {
		t.Error("identical nested arrays should be equal")
	}
	if a1.Equal(a3) {
		t.Error("nested arrays with different elem should not be equal")
	}
}

// ── NormalizeType ─────────────────────────────────────────────────────────────

func TestNormalizeType_Record(t *testing.T) {
	r := &RecordType{
		TypeName: "T",
		Fields: []RecordField{
			{Name: "v", Type: &PrimitiveType{Name: "i32"}, Offset: 0},
		},
	}
	norm := NormalizeType(r).(*RecordType)
	if norm.Fields[0].Type != primI32 {
		t.Error("NormalizeType should canonicalize field type to primI32 singleton")
	}
}

func TestNormalizeType_Array(t *testing.T) {
	a := &ArrayType{Len: 3, Elem: &PrimitiveType{Name: "f64"}}
	norm := NormalizeType(a).(*ArrayType)
	if norm.Elem != primF64 {
		t.Error("NormalizeType should canonicalize array elem to primF64 singleton")
	}
}

func TestNormalizeType_ArrayOfRecord(t *testing.T) {
	r := &RecordType{TypeName: "Node", Fields: []RecordField{
		{Name: "val", Type: &PrimitiveType{Name: "i32"}, Offset: 0},
	}}
	a := &ArrayType{Len: 8, Elem: r}
	norm := NormalizeType(a).(*ArrayType)
	rec := norm.Elem.(*RecordType)
	if rec.Fields[0].Type != primI32 {
		t.Error("NormalizeType should normalize nested record field types")
	}
}

// ── Ptr to aggregate ──────────────────────────────────────────────────────────

func TestPtr_ToRecord(t *testing.T) {
	r := NewRecordType("Node", []RecordField{{Name: "val", Type: I32(), Offset: 0}})
	p := Ptr(r)
	want := "ptr.record.Node"
	if got := p.String(); got != want {
		t.Errorf("Ptr(record) String() = %q, want %q", got, want)
	}
	// pointer equality: same string key → same pointer from ptrCache
	p2 := Ptr(r)
	if p != p2 {
		t.Error("Ptr should return cached PointerType for the same record")
	}
}

func TestPtr_ToArray(t *testing.T) {
	a := NewArrayType(10, I32())
	p := Ptr(a)
	want := "ptr.[10 x i32]"
	if got := p.String(); got != want {
		t.Errorf("Ptr(array) String() = %q, want %q", got, want)
	}
}

// ── GEP builder and verifier with aggregate ElemType ─────────────────────────

func TestVerify_GEP_RecordElemType(t *testing.T) {
	tempIDCounter = 0
	i32 := I32()

	recTy := NewRecordType("Pt", []RecordField{
		{Name: "x", Type: i32, Offset: 0},
		{Name: "y", Type: i32, Offset: 4},
	})
	base := NewTemp("pt", Ptr(recTy))
	base.IsAddr = true
	dst := NewTemp("px", Ptr(i32))
	dst.IsAddr = true

	gep := &GEPInst{Dst: dst, Base: base, ElemType: recTy, Offsets: []int{0}}
	ret := &ReturnInst{}

	entry := &Block{
		ID: 0, Label: "entry",
		Preds:  map[int]*Block{},
		Succs:  map[int]*Block{},
		Instrs: []Instr{gep, ret},
		Term:   ret,
	}
	fn := &Function{
		FnName: "gep_record",
		Entry:  entry,
		Exit:   entry,
		Blocks: map[int]*Block{0: entry},
	}

	errs := VerifyIR(fn)
	if len(errs) != 0 {
		for _, e := range errs {
			t.Logf("verify error: %s", e.Error())
		}
		t.Fatalf("expected no verify errors, got %d", len(errs))
	}
}

func TestVerify_GEP_ArrayElemType(t *testing.T) {
	tempIDCounter = 0
	i32 := I32()

	arrTy := NewArrayType(16, i32)
	base := NewTemp("arr", Ptr(arrTy))
	base.IsAddr = true
	dst := NewTemp("elem", Ptr(i32))
	dst.IsAddr = true

	gep := &GEPInst{Dst: dst, Base: base, ElemType: arrTy, Offsets: []int{3}}
	ret := &ReturnInst{}

	entry := &Block{
		ID: 0, Label: "entry",
		Preds:  map[int]*Block{},
		Succs:  map[int]*Block{},
		Instrs: []Instr{gep, ret},
		Term:   ret,
	}
	fn := &Function{
		FnName: "gep_array",
		Entry:  entry,
		Exit:   entry,
		Blocks: map[int]*Block{0: entry},
	}

	errs := VerifyIR(fn)
	if len(errs) != 0 {
		for _, e := range errs {
			t.Logf("verify error: %s", e.Error())
		}
		t.Fatalf("expected no verify errors, got %d", len(errs))
	}
}

// ── FormatInstr / FormatFunction with aggregate types ─────────────────────────

func TestFormatGEP_Record(t *testing.T) {
	tempIDCounter = 0
	i32 := I32()

	recTy := NewRecordType("Vec2", []RecordField{
		{Name: "x", Type: i32, Offset: 0},
		{Name: "y", Type: i32, Offset: 4},
	})
	base := NewTemp("v", Ptr(recTy))
	base.IsAddr = true
	dst := NewTemp("vx", Ptr(i32))
	dst.IsAddr = true

	gep := &GEPInst{Dst: dst, Base: base, ElemType: recTy, Offsets: []int{0}}
	out := pretty.FormatInstr(gep)
	t.Logf("formatted: %s", out)
	// should contain the record type name
	want := "record.Vec2"
	if len(out) == 0 {
		t.Fatal("FormatInstr returned empty string")
	}
	for _, s := range []string{"%vx", "%v", want} {
		found := false
		for i := 0; i+len(s) <= len(out); i++ {
			if out[i:i+len(s)] == s {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("FormatInstr output %q missing expected substring %q", out, s)
		}
	}
}

func TestFormatGEP_Array(t *testing.T) {
	tempIDCounter = 0
	i32 := I32()

	arrTy := NewArrayType(8, i32)
	base := NewTemp("buf", Ptr(arrTy))
	base.IsAddr = true
	dst := NewTemp("e3", Ptr(i32))
	dst.IsAddr = true

	gep := &GEPInst{Dst: dst, Base: base, ElemType: arrTy, Offsets: []int{3}}
	out := pretty.FormatInstr(gep)
	t.Logf("formatted: %s", out)
	want := "[8 x i32]"
	for _, s := range []string{"%e3", "%buf", want} {
		found := false
		for i := 0; i+len(s) <= len(out); i++ {
			if out[i:i+len(s)] == s {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("FormatInstr output %q missing expected substring %q", out, s)
		}
	}
}
