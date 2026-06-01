package minir

import (
	"bytes"
	"testing"
)

func TestEncodeDecodeInstrAddrInstr(t *testing.T) {
	ResetTempCounter()

	dst := NewTemp("dst_addr", Ptr(Void()))
	dst.IsAddr = true

	of := NewTemp("base_addr", Ptr(I32()))
	of.IsAddr = true

	in := &AddrInstr{Dst: dst, Of: of}

	var buf bytes.Buffer
	if err := EncodeInstr(&buf, in); err != nil {
		t.Fatalf("EncodeInstr(AddrInstr): %v", err)
	}

	outInstr, err := DecodeInstr(bytes.NewReader(buf.Bytes()))
	if err != nil {
		t.Fatalf("DecodeInstr(AddrInstr): %v", err)
	}

	out, ok := outInstr.(*AddrInstr)
	if !ok {
		t.Fatalf("decoded instruction type = %T, want *AddrInstr", outInstr)
	}
	if out.Dst == nil {
		t.Fatalf("decoded AddrInstr has nil Dst")
	}
	if out.Dst.NameStr != dst.NameStr || out.Dst.ID != dst.ID || !out.Dst.IsAddr {
		t.Fatalf("decoded Dst mismatch: got %+v, want name=%q id=%d isAddr=true", out.Dst, dst.NameStr, dst.ID)
	}

	outOf, ok := out.Of.(*Temp)
	if !ok {
		t.Fatalf("decoded Of type = %T, want *Temp", out.Of)
	}
	if outOf.NameStr != of.NameStr || outOf.ID != of.ID || !outOf.IsAddr {
		t.Fatalf("decoded Of mismatch: got %+v, want name=%q id=%d isAddr=true", outOf, of.NameStr, of.ID)
	}
}
