package minir

import "testing"

func TestVerifyIR_GEPAllowsPointerTypedTempBase(t *testing.T) {
	ResetTempCounter()

	rec := NewRecordType("Tm", []RecordField{{Name: "tm_sec", Type: I32(), Offset: 0}})
	base := NewTemp("tptr", Ptr(rec)) // pointer-typed, but not IsAddr=true
	dst := NewTemp("tm_sec", Ptr(I32()))
	dst.IsAddr = true

	gep := &GEPInst{Dst: dst, Base: base, ElemType: rec, Offsets: []int{0}}
	ret := &ReturnInst{}
	entry := &Block{ID: 0, Label: "entry", Instrs: []Instr{gep, ret}, Term: ret, Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	fn := &Function{FnName: "f", Blocks: map[int]*Block{0: entry}, Entry: entry, Exit: entry}

	errs := VerifyIR(fn)
	for _, err := range errs {
		if err.Msg == "GEPInst base is not an address value (IsAddr *Temp or *GlobalRef)" {
			t.Fatalf("unexpected verifier error for pointer-typed temp base: %v", err)
		}
	}
}

func TestIsAddrValue_RejectsNonPointerNonAddrTemp(t *testing.T) {
	tmp := NewTemp("x", I32())
	tmp.IsAddr = false
	if IsAddrValue(tmp) {
		t.Fatalf("expected plain i32 temp to be rejected as address value")
	}
}
