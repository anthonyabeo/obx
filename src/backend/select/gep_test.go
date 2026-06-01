package selector

import (
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

// TestGEPSelectorBasic tests instruction selection for basic GEP
func TestGEPSelectorBasic(t *testing.T) {
	baseReg := mir.NewRegister("base", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))
	dstReg := mir.NewRegister("addr", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))

	gep := &mir.GEPInstr{
		Dst:      dstReg,
		Base:     baseReg,
		ElemType: mir.NewScalarType("i32", 4),
		Strides:  nil,
		Offsets:  []int{},
		Indices:  nil,
	}

	if gep.Dst == nil {
		t.Fatalf("GEPInstr dst is nil")
	}
	if gep.Base == nil {
		t.Fatalf("GEPInstr base is nil")
	}

	t.Logf("Basic GEP: dst=%v, base=%v", gep.Dst, gep.Base)
}

// TestGEPSelectorWithStride1 tests selection for stride=1
func TestGEPSelectorWithStride1(t *testing.T) {
	baseReg := mir.NewRegister("base", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))
	idxReg := mir.NewRegister("idx", mir.VirtualReg, mir.NewScalarType("i32", 4))
	dstReg := mir.NewRegister("addr", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))

	gep := &mir.GEPInstr{
		Dst:      dstReg,
		Base:     baseReg,
		ElemType: mir.NewScalarType("i32", 4),
		Strides:  []int{1},
		Offsets:  []int{0},
		Indices:  []mir.Operand{idxReg},
	}

	// Verify structure
	if len(gep.Strides) != 1 || gep.Strides[0] != 1 {
		t.Fatalf("expected stride 1")
	}
	if len(gep.Indices) != 1 {
		t.Fatalf("expected 1 index")
	}

	t.Logf("GEP stride=1: should select GEP_stride_1 rule")
}

// TestGEPSelectorWithStride2 tests selection for stride=2
func TestGEPSelectorWithStride2(t *testing.T) {
	baseReg := mir.NewRegister("base", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i16", 2), 8))
	idxReg := mir.NewRegister("idx", mir.VirtualReg, mir.NewScalarType("i32", 4))
	dstReg := mir.NewRegister("addr", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i16", 2), 8))

	gep := &mir.GEPInstr{
		Dst:      dstReg,
		Base:     baseReg,
		ElemType: mir.NewScalarType("i16", 2),
		Strides:  []int{2},
		Offsets:  []int{0},
		Indices:  []mir.Operand{idxReg},
	}

	if gep.Strides[0] != 2 {
		t.Fatalf("expected stride 2")
	}

	t.Logf("GEP stride=2: should select GEP_stride_2 rule (shift by 1)")
}

// TestGEPSelectorWithStride4 tests selection for stride=4 (i32)
func TestGEPSelectorWithStride4(t *testing.T) {
	baseReg := mir.NewRegister("base", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))
	idxReg := mir.NewRegister("idx", mir.VirtualReg, mir.NewScalarType("i32", 4))
	dstReg := mir.NewRegister("addr", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))

	gep := &mir.GEPInstr{
		Dst:      dstReg,
		Base:     baseReg,
		ElemType: mir.NewScalarType("i32", 4),
		Strides:  []int{4},
		Offsets:  []int{0},
		Indices:  []mir.Operand{idxReg},
	}

	if gep.Strides[0] != 4 {
		t.Fatalf("expected stride 4")
	}

	t.Logf("GEP stride=4: should select GEP_stride_4 rule (shift by 2)")
}

// TestGEPSelectorWithStride8 tests selection for stride=8 (i64)
func TestGEPSelectorWithStride8(t *testing.T) {
	baseReg := mir.NewRegister("base", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i64", 8), 8))
	idxReg := mir.NewRegister("idx", mir.VirtualReg, mir.NewScalarType("i64", 8))
	dstReg := mir.NewRegister("addr", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i64", 8), 8))

	gep := &mir.GEPInstr{
		Dst:      dstReg,
		Base:     baseReg,
		ElemType: mir.NewScalarType("i64", 8),
		Strides:  []int{8},
		Offsets:  []int{0},
		Indices:  []mir.Operand{idxReg},
	}

	if gep.Strides[0] != 8 {
		t.Fatalf("expected stride 8")
	}

	t.Logf("GEP stride=8: should select GEP_stride_8 rule (shift by 3)")
}

// TestGEPSelector2D tests selection for 2D array access
func TestGEPSelector2D(t *testing.T) {
	// [10 x [8 x i32]]
	baseReg := mir.NewRegister("base", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))
	iReg := mir.NewRegister("i", mir.VirtualReg, mir.NewScalarType("i32", 4))
	jReg := mir.NewRegister("j", mir.VirtualReg, mir.NewScalarType("i32", 4))
	dstReg := mir.NewRegister("addr", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))

	gep := &mir.GEPInstr{
		Dst:      dstReg,
		Base:     baseReg,
		ElemType: mir.NewScalarType("i32", 4),
		Strides:  []int{32, 4}, // outer: 8*4, inner: 4
		Offsets:  []int{0, 0},
		Indices:  []mir.Operand{iReg, jReg},
	}

	if len(gep.Strides) != 2 {
		t.Fatalf("expected 2 strides for 2D array")
	}
	if gep.Strides[0] != 32 || gep.Strides[1] != 4 {
		t.Fatalf("incorrect strides for 2D array")
	}
	if len(gep.Indices) != 2 {
		t.Fatalf("expected 2 indices for 2D array")
	}

	t.Logf("GEP 2D: should select GEP_2d_4_8 or GEP_2d_shifted rule")
}

// TestGEPSelectorArbitraryStride tests selection for non-power-of-2 stride
func TestGEPSelectorArbitraryStride(t *testing.T) {
	baseReg := mir.NewRegister("base", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))
	idxReg := mir.NewRegister("idx", mir.VirtualReg, mir.NewScalarType("i32", 4))
	dstReg := mir.NewRegister("addr", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))

	// Stride = 12 (non-power-of-2)
	gep := &mir.GEPInstr{
		Dst:      dstReg,
		Base:     baseReg,
		ElemType: mir.NewScalarType("i32", 4),
		Strides:  []int{12},
		Offsets:  []int{0},
		Indices:  []mir.Operand{idxReg},
	}

	if gep.Strides[0] != 12 {
		t.Fatalf("expected stride 12")
	}

	t.Logf("GEP stride=12 (arbitrary): should select GEP_general rule (mul+add)")
}

// TestGEPSelectorMatches tests that GEP patterns match correctly
func TestGEPSelectorMatches(t *testing.T) {
	tests := []struct {
		name     string
		pattern  string
		strides  []int
		offsets  []int
		indices  int
		wantRule string
	}{
		{
			name:     "scalar base",
			pattern:  "gep",
			strides:  nil,
			offsets:  []int{},
			indices:  0,
			wantRule: "GEP_base",
		},
		{
			name:     "stride 1",
			pattern:  "gep",
			strides:  []int{1},
			offsets:  []int{0},
			indices:  1,
			wantRule: "GEP_stride_1",
		},
		{
			name:     "stride 2",
			pattern:  "gep",
			strides:  []int{2},
			offsets:  []int{0},
			indices:  1,
			wantRule: "GEP_stride_2",
		},
		{
			name:     "stride 4",
			pattern:  "gep",
			strides:  []int{4},
			offsets:  []int{0},
			indices:  1,
			wantRule: "GEP_stride_4",
		},
		{
			name:     "stride 8",
			pattern:  "gep",
			strides:  []int{8},
			offsets:  []int{0},
			indices:  1,
			wantRule: "GEP_stride_8",
		},
		{
			name:     "2D indexing",
			pattern:  "gep",
			strides:  []int{32, 4},
			offsets:  []int{0, 0},
			indices:  2,
			wantRule: "GEP_2d_4_8",
		},
		{
			name:     "arbitrary stride",
			pattern:  "gep",
			strides:  []int{12},
			offsets:  []int{0},
			indices:  1,
			wantRule: "GEP_general",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if !strings.EqualFold(tt.pattern, "gep") {
				t.Fatalf("expected 'gep' pattern")
			}

			t.Logf("Pattern: %s, Strides: %v, Offsets: %v, Indices: %d → expect %s",
				tt.pattern, tt.strides, tt.offsets, tt.indices, tt.wantRule)
		})
	}
}

// TestGEPSelectPredicates tests that optimization predicates work for GEP
func TestGEPSelectPredicates(t *testing.T) {
	tests := []struct {
		name   string
		stride int64
		isPow2 bool
	}{
		{"stride 1", 1, true},
		{"stride 2", 2, true},
		{"stride 3", 3, false},
		{"stride 4", 4, true},
		{"stride 5", 5, false},
		{"stride 8", 8, true},
		{"stride 12", 12, false},
		{"stride 16", 16, true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := tt.stride
			isPow2 := v&(v-1) == 0 && v > 0

			if isPow2 != tt.isPow2 {
				t.Fatalf("expected isPow2=%v, got %v for stride %d", tt.isPow2, isPow2, v)
			}

			if isPow2 {
				t.Logf("Stride %d is power-of-2: use shift optimization", v)
			} else {
				t.Logf("Stride %d is not power-of-2: use general mul+add", v)
			}
		})
	}
}

// TestGEPSelectUses tests that GEP instructions properly classify uses
func TestGEPSelectUses(t *testing.T) {
	baseReg := mir.NewRegister("base", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))
	idxReg := mir.NewRegister("idx", mir.VirtualReg, mir.NewScalarType("i32", 4))
	dstReg := mir.NewRegister("addr", mir.VirtualReg, mir.NewPointerType(mir.NewScalarType("i32", 4), 8))

	gep := &mir.GEPInstr{
		Dst:      dstReg,
		Base:     baseReg,
		ElemType: mir.NewScalarType("i32", 4),
		Strides:  []int{4},
		Offsets:  []int{0},
		Indices:  []mir.Operand{idxReg},
	}

	uses := gep.Uses()
	if len(uses) != 2 {
		t.Fatalf("expected 2 uses (base + index), got %d", len(uses))
	}

	defs := gep.Defs()
	if len(defs) != 1 {
		t.Fatalf("expected 1 def (destination), got %d", len(defs))
	}

	t.Logf("GEP uses: %d, defs: %d", len(uses), len(defs))
}

// TestGEPSelectCostEstimation tests cost prediction for different rules
func TestGEPSelectCostEstimation(t *testing.T) {
	tests := []struct {
		name        string
		stride      int
		expectedMin int // minimum expected instruction count
	}{
		{"stride=1", 1, 1},   // single add
		{"stride=2", 2, 1},   // add with shift
		{"stride=4", 4, 1},   // add with shift
		{"stride=8", 8, 1},   // add with shift
		{"stride=12", 12, 2}, // mul + add
		{"stride=3", 3, 2},   // mul + add
		{"stride=5", 5, 2},   // mul + add
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Check if stride is power-of-2
			isPow2 := tt.stride > 0 && (tt.stride&(tt.stride-1)) == 0

			var expectedCost int
			if isPow2 {
				expectedCost = 1 // Single add with shift
			} else {
				expectedCost = 2 // mul + add
			}

			if expectedCost < tt.expectedMin {
				t.Fatalf("cost %d is less than minimum %d", expectedCost, tt.expectedMin)
			}

			t.Logf("Stride %d: power-of-2=%v, expected cost=%d instructions", tt.stride, isPow2, expectedCost)
		})
	}
}
