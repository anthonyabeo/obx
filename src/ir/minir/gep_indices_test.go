package core

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/minir/lower"
)

// helper: compute strides for a nested ArrayType. Returns -1 for a stride that
// depends on an open (Len==0) dimension in the suffix.
func computeStrides(a *ArrayType) []int {
	// collect lengths
	var lens []int
	cur := a
	for cur != nil {
		lens = append(lens, cur.Len)
		if nt, ok := cur.Elem.(*ArrayType); ok {
			cur = nt
		} else {
			break
		}
	}
	// base element size
	var baseSize int
	switch bt := a.Elem.(type) {
	case *ArrayType:
		// find innermost element
		inn := bt
		for {
			if na, ok := inn.Elem.(*ArrayType); ok {
				inn = na
			} else {
				break
			}
		}
		// now inn.Elem is primitive
		switch inn.Elem {
		case primI32:
			baseSize = 4
		case primI64:
			baseSize = 8
		case primF32:
			baseSize = 4
		case primF64:
			baseSize = 8
		case primI1:
			baseSize = 1
		default:
			baseSize = 4
		}
	default:
		switch a.Elem {
		case primI32:
			baseSize = 4
		case primI64:
			baseSize = 8
		case primF32:
			baseSize = 4
		case primF64:
			baseSize = 8
		case primI1:
			baseSize = 1
		default:
			baseSize = 4
		}
	}

	n := len(lens)
	strides := make([]int, n)
	// compute product of suffix lengths; if any later len==0 then mark as dynamic (-1)
	for i := n - 1; i >= 0; i-- {
		if i == n-1 {
			strides[i] = baseSize
		} else {
			if lens[i+1] == 0 || strides[i+1] == -1 {
				strides[i] = -1
			} else {
				strides[i] = strides[i+1] * lens[i+1]
			}
		}
	}
	return strides
}

func TestGEP_MixedIndices_Ai2j(t *testing.T) {
	//tempIDCounter = 0
	lower.ResetTempCounter()

	// type: [3 x [4 x [5 x i32]]]
	arr := NewArrayType(3, NewArrayType(4, NewArrayType(5, I32())))
	base := lower.NewTemp("a", Ptr(arr))
	base.IsAddr = true
	i := lower.NewTemp("i", I32())
	j := lower.NewTemp("j", I32())
	dst := lower.NewTemp("dst", Ptr(I32()))

	// represent a[i][2][j] -> Offsets [0,2,0], Indices [i,j]
	gep := &GEPInst{Dst: dst, Base: base, ElemType: arr, Offsets: []int{0, 2, 0}, Indices: []Value{i, j}}

	// Uses should include base then i then j
	uses := gep.Uses()
	if len(uses) != 3 {
		t.Fatalf("expected 3 uses, got %d", len(uses))
	}
	if uses[0] != base || uses[1] != i || uses[2] != j {
		t.Fatalf("uses order incorrect: %v", uses)
	}

	out := FormatInstr(gep)
	if out == "" {
		t.Fatal("FormatInstr returned empty string")
	}
	// ensure offs and idxs appear
	if !(contains(out, "offs=[0, 2, 0]") || contains(out, "offs=[0,2,0]")) {
		t.Fatalf("formatted output missing offsets: %s", out)
	}
	if !contains(out, "idxs=[%i") && !contains(out, "idxs=[%j") {
		// formatted may show temps as %i/%j or %tN; just assert idxs presence
		if !contains(out, "idxs=[") {
			t.Fatalf("formatted output missing idxs: %s", out)
		}
	}
}

func TestGEP_AllConst_A2_3_4(t *testing.T) {
	//tempIDCounter = 0
	lower.ResetTempCounter()

	arr := NewArrayType(2, NewArrayType(3, NewArrayType(4, I32())))
	base := lower.NewTemp("b", Ptr(arr))
	base.IsAddr = true
	dst := lower.NewTemp("d", Ptr(I32()))
	gep := &GEPInst{Dst: dst, Base: base, ElemType: arr, Offsets: []int{2, 3, 4}, Indices: nil}

	if len(gep.Offsets) != 3 {
		t.Fatalf("expected 3 offsets, got %d", len(gep.Offsets))
	}
	if gep.Indices != nil && len(gep.Indices) != 0 {
		t.Fatalf("expected no indices, got %v", gep.Indices)
	}
	out := FormatInstr(gep)
	if !contains(out, "offs=[2, 3, 4]") && !contains(out, "offs=[2,3,4]") {
		t.Fatalf("formatted output missing offsets: %s", out)
	}
}

func TestGEP_AllDynamic_AiAj(t *testing.T) {
	//tempIDCounter = 0
	lower.ResetTempCounter()
	arr := NewArrayType(0, NewArrayType(0, I32()))
	base := lower.NewTemp("c", Ptr(arr))
	base.IsAddr = true
	i := lower.NewTemp("i", I32())
	j := lower.NewTemp("j", I32())
	dst := lower.NewTemp("dst2", Ptr(I32()))
	gep := &GEPInst{Dst: dst, Base: base, ElemType: arr, Offsets: []int{0, 0}, Indices: []Value{i, j}}

	if len(gep.Offsets) != 2 {
		t.Fatalf("expected 2 offsets, got %d", len(gep.Offsets))
	}
	if len(gep.Indices) != 2 {
		t.Fatalf("expected 2 indices, got %d", len(gep.Indices))
	}
	if gep.Indices[0] != i || gep.Indices[1] != j {
		t.Fatalf("indices ordering incorrect: %v", gep.Indices)
	}
}

func TestArrayStrides_OpenArrays(t *testing.T) {
	// Case 1: [0 x [4 x i32]] -> dims [0,4], strides should be [16,4]
	arr1 := NewArrayType(0, NewArrayType(4, I32()))
	s1 := computeStrides(arr1)
	if len(s1) != 2 {
		t.Fatalf("expected 2 strides, got %d", len(s1))
	}
	if s1[0] != 16 || s1[1] != 4 {
		t.Fatalf("unexpected strides for arr1: %v", s1)
	}

	// Case 2: [0 x [0 x i32]] -> dims [0,0], strides should be [-1,4]
	arr2 := NewArrayType(0, NewArrayType(0, I32()))
	s2 := computeStrides(arr2)
	if len(s2) != 2 {
		t.Fatalf("expected 2 strides, got %d", len(s2))
	}
	if s2[0] != -1 || s2[1] != 4 {
		t.Fatalf("unexpected strides for arr2: %v", s2)
	}
}

// small helper for substring check
func contains(s, sub string) bool {
	return len(s) >= len(sub) && (indexOf(s, sub) >= 0)
}

func indexOf(s, sub string) int {
	for i := 0; i+len(sub) <= len(s); i++ {
		if s[i:i+len(sub)] == sub {
			return i
		}
	}
	return -1
}
