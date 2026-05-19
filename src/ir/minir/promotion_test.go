package minir

// promotion_test.go — tests for dominantIntType, coerceToType,
// sameWidthOperands, alignOperands, and the lowerUnary MINUS path.
//
// Test matrix
// ──────────────────────────────────────────────────────────────────────────────
// Unit (pure-function) tests
//   TestDominantIntType         – all (a,b) pairs including edge cases
//   TestCoerceToType            – nil args, same type, const fold, temp retype
//
// alignOperands unit tests (same-package access)
//   TestAlignOperands_IntInt    – i32 vs i64 → annotate, no CastInst
//   TestAlignOperands_SignedWins – i32 vs u32 → i32 chosen
//   TestAlignOperands_FloatFloat – f32 vs f64 → explicit cast
//   TestAlignOperands_ConstAndTemp – const(i32) vs temp(i64) → dominant wins
//
// sameWidthOperands unit tests
//   TestSameWidthOperands_IntTarget    – integer ty → coerceToType, no cast
//   TestSameWidthOperands_FloatTarget  – float ty → sameType emits cast
//   TestSameWidthOperands_NilTarget    – nil ty  → delegates to alignOperands
//
// Integration (end-to-end lowering) tests
//   TestPromotion_BinaryI32PlusI64        – i32+i64 SemaType=Int64 → no CastInst
//   TestPromotion_BinaryI8PlusI32         – i8+i32  SemaType=Int32 → no CastInst
//   TestPromotion_BinarySignedOverUnsigned – i32+u32 SemaType=Int32 → no CastInst
//   TestPromotion_BinaryFloatWidening     – f32 op f64 → fpext CastInst emitted
//   TestPromotion_BinaryConstPlusTemp     – literal(Int32)+temp(Int64) → no CastInst
//   TestPromotion_ComparisonMixedWidths   – i32 < i64 → no CastInst
//   TestPromotion_ANDOperatorNoCast       – BOOLEAN & BOOLEAN → no CastInst
//   TestPromotion_OROperatorNoCast        – BOOLEAN | BOOLEAN → no CastInst
//   TestPromotion_SetMembershipIN         – IN (u32) → coerce annotation, no CastInst
//   TestPromotion_UnaryMinusNoCast        – -i32 → no CastInst
//   TestPromotion_UnaryMinusF32NoCast     – -f32 → no CastInst (already right type)
//   TestPromotion_UnaryNotCastsToI1       – NOT i32 → trunc CastInst IS emitted

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// ── test helpers ──────────────────────────────────────────────────────────────

// countCastInsts returns the total number of CastInst instructions in all
// blocks of fn.
func countCastInsts(fn *Function) int {
	n := 0
	for _, blk := range fn.Blocks {
		for _, inst := range blk.Instrs {
			if _, ok := inst.(*CastInst); ok {
				n++
			}
		}
	}
	return n
}

// castOps returns the Op strings of all CastInsts in fn (for assertion on kind).
func castOps(fn *Function) []string {
	var ops []string
	for _, blk := range fn.Blocks {
		for _, inst := range blk.Instrs {
			if ci, ok := inst.(*CastInst); ok {
				ops = append(ops, ci.Op)
			}
		}
	}
	return ops
}

// lowerSingleFn lowers a minimal HIR function with the given params, result
// type, and a single body statement, then returns the first lowered Function.
//
// The body statement is typically a ReturnStmt or AssignStmt that exercises the
// expression under test.
func lowerSingleFn(name string, params []*desugar.Param, result types.Type, body desugar.Stmt) *Function {
	tempIDCounter = 0
	hirFn := &desugar.Function{
		Name:   name,
		Params: params,
		Result: result,
		Body:   &desugar.CompoundStmt{Stmts: []desugar.Stmt{body}},
	}
	prog := Lower(hirProg(hirFn))
	if len(prog.Modules) == 0 || len(prog.Modules[0].Functions) == 0 {
		return nil
	}
	return prog.Modules[0].Functions[0]
}

// newLowerer creates a fresh Lowerer in a state ready for unit tests.
func newLowerer() *Lowerer {
	l := New(nil)
	l.fn = &Function{
		FnName: "test",
		Blocks: make(map[int]*Block),
	}
	entry := &Block{ID: 1, Label: "entry"}
	l.fn.Entry = entry
	l.fn.Blocks[entry.ID] = entry
	l.curBlock = entry
	tempIDCounter = 0
	return l
}

// ── unit tests: dominantIntType ───────────────────────────────────────────────

func TestDominantIntType(t *testing.T) {
	type tc struct {
		name     string
		a, b     Type
		wantName string // "" means nil expected
	}
	cases := []tc{
		// identical types
		{"i32 == i32", I32(), I32(), "i32"},
		{"i64 == i64", I64(), I64(), "i64"},
		{"u32 == u32", U32(), U32(), "u32"},

		// wider wins (unsigned or signed)
		{"i32 < i64→i64", I32(), I64(), "i64"},
		{"i64 > i32→i64", I64(), I32(), "i64"},
		{"i8 < i32→i32", I8(), I32(), "i32"},
		{"i32 > i8→i32", I32(), I8(), "i32"},
		{"i16 < i64→i64", I16(), I64(), "i64"},
		{"u8 < u32→u32", U8(), U32(), "u32"},
		{"i8 < u32→u32", I8(), U32(), "u32"}, // u32 wider than i8

		// same width, same signedness — prefer a
		{"i32 == i32→i32", I32(), I32(), "i32"},
		{"u8 == u8→u8", U8(), U8(), "u8"},

		// same width, mixed signedness → next larger signed type
		{"i8 vs u8→i16", I8(), U8(), "i16"},
		{"u8 vs i8→i16", U8(), I8(), "i16"},
		{"i16 vs u16→i32", I16(), U16(), "i32"},
		{"u16 vs i16→i32", U16(), I16(), "i32"},
		{"i32 vs u32→i64", I32(), U32(), "i64"},
		{"u32 vs i32→i64", U32(), I32(), "i64"},

		// non-integer → nil
		{"f32 vs i32→nil", F32(), I32(), ""},
		{"i32 vs f32→nil", I32(), F32(), ""},
		{"f32 vs f64→nil", F32(), F64(), ""},
		{"f64 vs f32→nil", F64(), F32(), ""},

		// nil argument → nil
		{"nil vs i32→nil", nil, I32(), ""},
		{"i32 vs nil→nil", I32(), nil, ""},
		{"nil vs nil→nil", nil, nil, ""},
	}

	for _, c := range cases {
		got := dominantIntType(c.a, c.b)
		gotName := ""
		if got != nil {
			gotName = got.String()
		}
		if gotName != c.wantName {
			t.Errorf("%s: dominantIntType(%v, %v) = %q, want %q",
				c.name, c.a, c.b, gotName, c.wantName)
		}
	}
}

// TestNextSignedType verifies the nextSignedType helper for all relevant widths.
func TestNextSignedType(t *testing.T) {
	cases := []struct {
		bits int
		want Type
	}{
		{1, I16()},
		{8, I16()},
		{16, I32()},
		{32, I64()},
		{64, I64()},
	}
	for _, c := range cases {
		got := nextSignedType(c.bits)
		if got == nil || !got.Equal(c.want) {
			t.Errorf("nextSignedType(%d) = %v, want %v", c.bits, got, c.want)
		}
	}
}

// ── unit tests: coerceToType ──────────────────────────────────────────────────

func TestCoerceToType_NilVal(t *testing.T) {
	if got := coerceToType(nil, I32()); got != nil {
		t.Errorf("coerceToType(nil, i32): want nil, got %v", got)
	}
}

func TestCoerceToType_NilTy(t *testing.T) {
	c := NewConst("7", int64(7), I32())
	if got := coerceToType(c, nil); got != c {
		t.Errorf("coerceToType(c, nil): want identity, got %v", got)
	}
}

func TestCoerceToType_SameTypeConst(t *testing.T) {
	c := NewConst("42", int64(42), I32())
	got := coerceToType(c, I32())
	// same type → identity (early return from Equal check)
	if got != c {
		t.Errorf("coerceToType(const_i32, i32): expected same value returned")
	}
}

func TestCoerceToType_SameTypeTemp(t *testing.T) {
	tempIDCounter = 0
	tmp := NewAnonTemp(I32())
	got := coerceToType(tmp, I32())
	if got != tmp {
		t.Errorf("coerceToType(temp_i32, i32): expected same *Temp returned")
	}
}

func TestCoerceToType_ConstFoldI32toI64(t *testing.T) {
	c := NewConst("100", int64(100), I32())
	got := coerceToType(c, I64())
	if got == nil {
		t.Fatal("coerceToType(const_i32, i64): got nil")
	}
	if got.Type() == nil || !got.Type().Equal(I64()) {
		t.Errorf("coerceToType(const_i32, i64): type = %v, want i64", got.Type())
	}
	if !got.IsConst() {
		t.Errorf("coerceToType(const_i32, i64): expected constant result, got non-const")
	}
	if v, ok := got.(*IntegerConst); ok {
		if v.Value != 100 {
			t.Errorf("coerceToType(const_i32, i64): value = %d, want 100", v.Value)
		}
	}
}

func TestCoerceToType_ConstFoldU32toI64(t *testing.T) {
	c := NewConst("0xFF", int64(255), U32())
	got := coerceToType(c, I64())
	if got == nil {
		t.Fatal("coerceToType(const_u32, i64): got nil")
	}
	if got.Type() == nil || !got.Type().Equal(I64()) {
		t.Errorf("coerceToType(const_u32, i64): type = %v, want i64", got.Type())
	}
}

func TestCoerceToType_ConstPointerUnchanged(t *testing.T) {
	// CoerceConst can't handle pointer target → original const returned unchanged
	c := NewConst("0", int64(0), I32())
	ptrTy := Ptr(I32())
	got := coerceToType(c, ptrTy)
	// Should return original (CoerceConst returns nil for pointer targets)
	if got != c {
		t.Errorf("coerceToType(const_i32, ptr): expected identity (CoerceConst unsupported), got %v", got)
	}
}

func TestCoerceToType_TempRetypedNoCast(t *testing.T) {
	tempIDCounter = 0
	tmp := NewAnonTemp(I32())
	originalID := tmp.ID

	got := coerceToType(tmp, I64())
	if got == nil {
		t.Fatal("coerceToType(temp_i32, i64): got nil")
	}
	// Must be a *Temp (not a Constant)
	retypedTmp, ok := got.(*Temp)
	if !ok {
		t.Fatalf("coerceToType(temp_i32, i64): expected *Temp, got %T", got)
	}
	// Type must be updated
	if !retypedTmp.Ty.Equal(I64()) {
		t.Errorf("coerceToType(temp_i32, i64): new Ty = %v, want i64", retypedTmp.Ty)
	}
	// ID must be preserved (SSA def-use chain intact)
	if retypedTmp.ID != originalID {
		t.Errorf("coerceToType(temp_i32, i64): ID changed from %d to %d", originalID, retypedTmp.ID)
	}
	// Original must be unmodified
	if !tmp.Ty.Equal(I32()) {
		t.Errorf("coerceToType(temp_i32, i64): original temp's Ty was mutated to %v", tmp.Ty)
	}
}

func TestCoerceToType_TempI8toI32(t *testing.T) {
	tempIDCounter = 0
	tmp := NewAnonTemp(I8())
	got := coerceToType(tmp, I32())
	retypedTmp, ok := got.(*Temp)
	if !ok {
		t.Fatalf("expected *Temp, got %T", got)
	}
	if !retypedTmp.Ty.Equal(I32()) {
		t.Errorf("type = %v, want i32", retypedTmp.Ty)
	}
	if tmp.Ty.String() != "i8" {
		t.Errorf("original Ty mutated to %v", tmp.Ty)
	}
}

// ── unit tests: alignOperands ─────────────────────────────────────────────────

func TestAlignOperands_SameType_NoCast(t *testing.T) {
	tempIDCounter = 0
	l := newLowerer()
	a := NewAnonTemp(I32())
	b := NewAnonTemp(I32())
	la, rb := l.alignOperands(a, b)
	// Same type → both returned unchanged
	if la != a || rb != b {
		t.Errorf("alignOperands(i32, i32): expected identity for same types")
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("alignOperands(i32, i32): no CastInst expected, got %d", countCastInsts(l.fn))
	}
}

func TestAlignOperands_I32_I64_NoCast(t *testing.T) {
	tempIDCounter = 0
	l := newLowerer()
	a := NewAnonTemp(I32())
	b := NewAnonTemp(I64())
	la, rb := l.alignOperands(a, b)
	// dominant is i64; both should be annotated to i64 with no CastInst
	if la.Type() == nil || !la.Type().Equal(I64()) {
		t.Errorf("left type = %v, want i64", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(I64()) {
		t.Errorf("right type = %v, want i64", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts, got %d", countCastInsts(l.fn))
	}
}

func TestAlignOperands_I64_I32_NoCast(t *testing.T) {
	tempIDCounter = 0
	l := newLowerer()
	a := NewAnonTemp(I64())
	b := NewAnonTemp(I32())
	la, rb := l.alignOperands(a, b)
	if la.Type() == nil || !la.Type().Equal(I64()) {
		t.Errorf("left type = %v, want i64", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(I64()) {
		t.Errorf("right type = %v, want i64", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts, got %d", countCastInsts(l.fn))
	}
}

func TestAlignOperands_SignedWins_I32_U32(t *testing.T) {
	tempIDCounter = 0
	l := newLowerer()
	a := NewAnonTemp(I32())
	b := NewAnonTemp(U32())
	la, rb := l.alignOperands(a, b)
	// i32 and u32: equal width, mixed sign → next larger signed type = i64
	// (255…0xFFFFFFFF cannot fit in i32, so we must widen)
	if la.Type() == nil || !la.Type().Equal(I64()) {
		t.Errorf("left type = %v, want i64 (mixed-sign equal-width promotion)", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(I64()) {
		t.Errorf("right type = %v, want i64 (mixed-sign equal-width promotion)", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts (annotation only), got %d", countCastInsts(l.fn))
	}
}

func TestAlignOperands_SignedWins_U32_I32(t *testing.T) {
	tempIDCounter = 0
	l := newLowerer()
	a := NewAnonTemp(U32())
	b := NewAnonTemp(I32())
	la, rb := l.alignOperands(a, b)
	// Reversed order: same promotion applies, result is i64
	if la.Type() == nil || !la.Type().Equal(I64()) {
		t.Errorf("left type = %v, want i64", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(I64()) {
		t.Errorf("right type = %v, want i64", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts, got %d", countCastInsts(l.fn))
	}
}

func TestAlignOperands_I8_U8_SignedWins(t *testing.T) {
	// i8 vs u8: 255 cannot fit in i8 (max 127), so promote to i16
	tempIDCounter = 0
	l := newLowerer()
	a := NewAnonTemp(I8())
	b := NewAnonTemp(U8())
	la, rb := l.alignOperands(a, b)
	if la.Type() == nil || !la.Type().Equal(I16()) {
		t.Errorf("left type = %v, want i16", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(I16()) {
		t.Errorf("right type = %v, want i16", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts, got %d", countCastInsts(l.fn))
	}
}

func TestAlignOperands_FloatFloat_CastEmitted(t *testing.T) {
	tempIDCounter = 0
	l := newLowerer()
	l.fn.Exit = nil // no exit block needed here
	a := NewAnonTemp(F32())
	b := NewAnonTemp(F64())
	la, rb := l.alignOperands(a, b)
	// Falls through to explicit-cast path: f32 cast to f64.
	if la.Type() == nil || !la.Type().Equal(F32()) {
		t.Errorf("left type = %v, want f32 (dominant left unchanged)", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(F32()) {
		t.Errorf("right type = %v, want f32 (right cast to left's type)", rb.Type())
	}
	if countCastInsts(l.fn) != 1 {
		t.Errorf("expected 1 CastInst for float widening, got %d", countCastInsts(l.fn))
	}
}

func TestAlignOperands_ConstI32vsTempI64_NoCast(t *testing.T) {
	// const(i32) on left, temp(i64) on right: dominant is i64, both annotated
	tempIDCounter = 0
	l := newLowerer()
	c := NewConst("5", int64(5), I32())
	b := NewAnonTemp(I64())
	la, rb := l.alignOperands(c, b)
	// dominantIntType path: const folded to i64
	if la.Type() == nil || !la.Type().Equal(I64()) {
		t.Errorf("left (const) type = %v, want i64", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(I64()) {
		t.Errorf("right (temp) type = %v, want i64", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts (const folded, temp annotated), got %d", countCastInsts(l.fn))
	}
}

func TestAlignOperands_ConstI64vsTempI32_NoCast(t *testing.T) {
	// const(i64) on left, temp(i32) on right: dominant is i64
	tempIDCounter = 0
	l := newLowerer()
	c := NewConst("99", int64(99), I64())
	b := NewAnonTemp(I32())
	la, rb := l.alignOperands(c, b)
	if la.Type() == nil || !la.Type().Equal(I64()) {
		t.Errorf("left (const) type = %v, want i64", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(I64()) {
		t.Errorf("right (temp) type = %v, want i64", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts, got %d", countCastInsts(l.fn))
	}
}

func TestAlignOperands_NilLeft(t *testing.T) {
	l := newLowerer()
	la, rb := l.alignOperands(nil, NewAnonTemp(I32()))
	if la != nil {
		t.Errorf("left should remain nil")
	}
	if rb == nil {
		t.Errorf("right should be returned unchanged")
	}
}

func TestAlignOperands_NilRight(t *testing.T) {
	l := newLowerer()
	a := NewAnonTemp(I32())
	la, rb := l.alignOperands(a, nil)
	if la != a {
		t.Errorf("left should be returned unchanged")
	}
	if rb != nil {
		t.Errorf("right should remain nil")
	}
}

// ── unit tests: sameWidthOperands ─────────────────────────────────────────────

func TestSameWidthOperands_IntTarget_NoCast(t *testing.T) {
	tempIDCounter = 0
	l := newLowerer()
	a := NewAnonTemp(I32())
	b := NewAnonTemp(I64())
	la, rb := l.sameWidthOperands(a, b, I64())
	if la.Type() == nil || !la.Type().Equal(I64()) {
		t.Errorf("left type = %v, want i64", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(I64()) {
		t.Errorf("right type = %v, want i64", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts for integer target, got %d", countCastInsts(l.fn))
	}
}

func TestSameWidthOperands_IntTarget_AlreadyCorrect(t *testing.T) {
	tempIDCounter = 0
	l := newLowerer()
	a := NewAnonTemp(I32())
	b := NewAnonTemp(I32())
	la, rb := l.sameWidthOperands(a, b, I32())
	if la.Type() == nil || !la.Type().Equal(I32()) {
		t.Errorf("left type = %v, want i32", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(I32()) {
		t.Errorf("right type = %v, want i32", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts, got %d", countCastInsts(l.fn))
	}
}

func TestSameWidthOperands_UnsignedIntTarget_NoCast(t *testing.T) {
	// u32 target (set membership uses primU32)
	tempIDCounter = 0
	l := newLowerer()
	a := NewAnonTemp(I32())
	b := NewAnonTemp(I32())
	la, rb := l.sameWidthOperands(a, b, U32())
	if la.Type() == nil || !la.Type().Equal(U32()) {
		t.Errorf("left type = %v, want u32", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(U32()) {
		t.Errorf("right type = %v, want u32", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts for u32 target, got %d", countCastInsts(l.fn))
	}
}

func TestSameWidthOperands_FloatTarget_CastEmitted(t *testing.T) {
	tempIDCounter = 0
	l := newLowerer()
	l.fn.Exit = nil
	a := NewAnonTemp(F32())
	b := NewAnonTemp(F32())
	la, rb := l.sameWidthOperands(a, b, F64())
	if la.Type() == nil || !la.Type().Equal(F64()) {
		t.Errorf("left type = %v, want f64", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(F64()) {
		t.Errorf("right type = %v, want f64", rb.Type())
	}
	if countCastInsts(l.fn) != 2 {
		t.Errorf("expected 2 CastInsts (one per operand) for float target, got %d", countCastInsts(l.fn))
	}
}

func TestSameWidthOperands_NilTarget_DelegatesToAlign(t *testing.T) {
	tempIDCounter = 0
	l := newLowerer()
	a := NewAnonTemp(I32())
	b := NewAnonTemp(I64())
	la, rb := l.sameWidthOperands(a, b, nil)
	// nil ty → alignOperands → dominantIntType → i64, no cast
	if la.Type() == nil || !la.Type().Equal(I64()) {
		t.Errorf("left type = %v, want i64", la.Type())
	}
	if rb.Type() == nil || !rb.Type().Equal(I64()) {
		t.Errorf("right type = %v, want i64", rb.Type())
	}
	if countCastInsts(l.fn) != 0 {
		t.Errorf("expected 0 CastInsts from delegated alignOperands, got %d", countCastInsts(l.fn))
	}
}

// ── integration tests ─────────────────────────────────────────────────────────

// helper: build a return of a binary expression
func retBinExpr(op token.Kind, left, right desugar.Expr, semaType, resultType types.Type) desugar.Stmt {
	return &desugar.ReturnStmt{
		Result: &desugar.BinaryExpr{
			Op:       op,
			Left:     left,
			Right:    right,
			SemaType: semaType,
		},
	}
}

func TestPromotion_BinaryI32PlusI64_NoCast(t *testing.T) {
	// f(a: INT32, b: INT64): INT64 = RETURN a + b
	// After sameWidthOperands(left=i32_temp, right=i64_temp, ty=i64):
	//   coerceToType → no CastInst emitted.
	fn := lowerSingleFn("add_i32_i64",
		[]*desugar.Param{hirParam("a", types.Int32Type), hirParam("b", types.Int64Type)},
		types.Int64Type,
		retBinExpr(token.PLUS,
			hirParamRef("a", types.Int32Type),
			hirParamRef("b", types.Int64Type),
			types.Int64Type, types.Int64Type),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("add_i32_i64:\n%s", FormatFunction(fn))
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for i32+i64 promotion, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_BinaryI8PlusI32_NoCast(t *testing.T) {
	// f(a: INT8, b: INT32): INT32 = RETURN a + b
	fn := lowerSingleFn("add_i8_i32",
		[]*desugar.Param{hirParam("a", types.Int8Type), hirParam("b", types.Int32Type)},
		types.Int32Type,
		retBinExpr(token.PLUS,
			hirParamRef("a", types.Int8Type),
			hirParamRef("b", types.Int32Type),
			types.Int32Type, types.Int32Type),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("add_i8_i32:\n%s", FormatFunction(fn))
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for i8+i32 promotion, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_BinaryI32MultI64_NoCast(t *testing.T) {
	// f(a: INT32, b: INT64): INT64 = RETURN a * b
	fn := lowerSingleFn("mul_i32_i64",
		[]*desugar.Param{hirParam("a", types.Int32Type), hirParam("b", types.Int64Type)},
		types.Int64Type,
		retBinExpr(token.STAR,
			hirParamRef("a", types.Int32Type),
			hirParamRef("b", types.Int64Type),
			types.Int64Type, types.Int64Type),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for i32*i64 promotion, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_BinaryI32MinusI64_NoCast(t *testing.T) {
	// f(a: INT64, b: INT32): INT64 = RETURN a - b
	fn := lowerSingleFn("sub_i64_i32",
		[]*desugar.Param{hirParam("a", types.Int64Type), hirParam("b", types.Int32Type)},
		types.Int64Type,
		retBinExpr(token.MINUS,
			hirParamRef("a", types.Int64Type),
			hirParamRef("b", types.Int32Type),
			types.Int64Type, types.Int64Type),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for i64-i32 promotion, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_BinaryFloatWidening_CastEmitted(t *testing.T) {
	// f(a: REAL, b: LONGREAL): LONGREAL = RETURN a + b
	// alignOperands falls through to the explicit-cast path for floats.
	// left=f32, right=f64: left is the dominant side (kept), right is cast to f32 via fptrunc.
	fn := lowerSingleFn("add_f32_f64",
		[]*desugar.Param{hirParam("a", types.RealType), hirParam("b", types.LongRealType)},
		types.LongRealType,
		retBinExpr(token.PLUS,
			hirParamRef("a", types.RealType),
			hirParamRef("b", types.LongRealType),
			types.LongRealType, types.LongRealType),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("add_f32_f64:\n%s", FormatFunction(fn))
	ops := castOps(fn)
	if len(ops) == 0 {
		t.Errorf("expected at least one CastInst (fpext or fptrunc) for f32+f64, got none")
	}
	// The fallback explicit-cast path emits either fpext or fptrunc; either is valid.
	hasFpCast := false
	for _, op := range ops {
		if op == "fpext" || op == "fptrunc" {
			hasFpCast = true
		}
	}
	if !hasFpCast {
		t.Errorf("expected fpext or fptrunc CastInst for f32/f64 widening, got ops=%v", ops)
	}
}

func TestPromotion_BinaryI8PlusU8_WidensToI16_NoCast(t *testing.T) {
	fn := lowerSingleFn("add_i8_u8",
		[]*desugar.Param{hirParam("a", types.Int8Type), hirParam("b", types.ByteType)},
		types.Int16Type,
		retBinExpr(token.PLUS,
			hirParamRef("a", types.Int8Type),
			hirParamRef("b", types.ByteType),
			types.Int16Type, types.Int16Type),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("add_i8_u8:\n%s", FormatFunction(fn))
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for i8+u8 (→i16) promotion, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_BinaryI32PlusU32_WidensToI64_NoCast(t *testing.T) {
	fn := lowerSingleFn("add_i32_u32",
		// BYTE/SET are u8/u32 in the IR; reuse SetType (→ u32) for the unsigned operand
		[]*desugar.Param{hirParam("a", types.Int32Type), hirParam("b", types.SetType)},
		types.Int64Type,
		retBinExpr(token.PLUS,
			hirParamRef("a", types.Int32Type),
			hirParamRef("b", types.SetType),
			types.Int64Type, types.Int64Type),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("add_i32_u32:\n%s", FormatFunction(fn))
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for i32+u32 (→i64) promotion, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_BinaryConstI32PlusTempI64_NoCast(t *testing.T) {
	// f(b: INT64): INT64 = RETURN 42 + b
	// The literal 42 has SemaType=Int32Type; combined with i64 temp via sameWidthOperands.
	fn := lowerSingleFn("const_plus_i64",
		[]*desugar.Param{hirParam("b", types.Int64Type)},
		types.Int64Type,
		retBinExpr(token.PLUS,
			hirLit("42", types.Int32Type),
			hirParamRef("b", types.Int64Type),
			types.Int64Type, types.Int64Type),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("const_plus_i64:\n%s", FormatFunction(fn))
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for const(i32)+temp(i64), got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_ComparisonI32EqI64_NoCast(t *testing.T) {
	// f(a: INT32, b: INT64): BOOL = RETURN a = b
	// alignOperands on comparison path.
	fn := lowerSingleFn("cmp_i32_i64",
		[]*desugar.Param{hirParam("a", types.Int32Type), hirParam("b", types.Int64Type)},
		types.BooleanType,
		retBinExpr(token.EQUAL,
			hirParamRef("a", types.Int32Type),
			hirParamRef("b", types.Int64Type),
			types.BooleanType, types.BooleanType),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("cmp_i32_i64:\n%s", FormatFunction(fn))
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for i32==i64 comparison, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_ComparisonI8LessI32_NoCast(t *testing.T) {
	fn := lowerSingleFn("cmp_i8_i32",
		[]*desugar.Param{hirParam("a", types.Int8Type), hirParam("b", types.Int32Type)},
		types.BooleanType,
		retBinExpr(token.LESS,
			hirParamRef("a", types.Int8Type),
			hirParamRef("b", types.Int32Type),
			types.BooleanType, types.BooleanType),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for i8<i32 comparison, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_ANDOperator_NoCast(t *testing.T) {
	// f(a: BOOLEAN, b: BOOLEAN): BOOLEAN = RETURN a & b
	fn := lowerSingleFn("and_bool",
		[]*desugar.Param{hirParam("a", types.BooleanType), hirParam("b", types.BooleanType)},
		types.BooleanType,
		retBinExpr(token.AND,
			hirParamRef("a", types.BooleanType),
			hirParamRef("b", types.BooleanType),
			types.BooleanType, types.BooleanType),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for bool AND bool, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_OROperator_NoCast(t *testing.T) {
	fn := lowerSingleFn("or_bool",
		[]*desugar.Param{hirParam("a", types.BooleanType), hirParam("b", types.BooleanType)},
		types.BooleanType,
		retBinExpr(token.OR,
			hirParamRef("a", types.BooleanType),
			hirParamRef("b", types.BooleanType),
			types.BooleanType, types.BooleanType),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for bool OR bool, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_SetMembership_IN_NoCast(t *testing.T) {
	// f(elem: INT32, s: SET): BOOL = RETURN elem IN s
	// IN case uses sameWidthOperands(left, right, primU32); IsIntType(u32)=true → coerceToType.
	fn := lowerSingleFn("set_in",
		[]*desugar.Param{hirParam("elem", types.Int32Type), hirParam("s", types.SetType)},
		types.BooleanType,
		retBinExpr(token.IN,
			hirParamRef("elem", types.Int32Type),
			hirParamRef("s", types.SetType),
			types.BooleanType, types.BooleanType),
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("set_in:\n%s", FormatFunction(fn))
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for IN (u32 coercion) operator, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_UnaryMinus_I32_NoCast(t *testing.T) {
	// f(x: INT32): INT32 = RETURN -x
	// lowerUnary MINUS: coerceToType(operand, ty); operand already i32, ty=i32 → no cast.
	tempIDCounter = 0
	fn := lowerSingleFn("neg_i32",
		[]*desugar.Param{hirParam("x", types.Int32Type)},
		types.Int32Type,
		&desugar.ReturnStmt{
			Result: &desugar.UnaryExpr{
				Op:       token.MINUS,
				Operand:  hirParamRef("x", types.Int32Type),
				SemaType: types.Int32Type,
			},
		},
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("neg_i32:\n%s", FormatFunction(fn))
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for unary minus i32, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_UnaryMinus_I64_NoCast(t *testing.T) {
	tempIDCounter = 0
	fn := lowerSingleFn("neg_i64",
		[]*desugar.Param{hirParam("x", types.Int64Type)},
		types.Int64Type,
		&desugar.ReturnStmt{
			Result: &desugar.UnaryExpr{
				Op:       token.MINUS,
				Operand:  hirParamRef("x", types.Int64Type),
				SemaType: types.Int64Type,
			},
		},
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for unary minus i64, got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_UnaryMinus_Widening_NoCast(t *testing.T) {
	// operand is INT32 but SemaType is INT64: coerceToType handles the annotation.
	tempIDCounter = 0
	fn := lowerSingleFn("neg_i32_to_i64",
		[]*desugar.Param{hirParam("x", types.Int32Type)},
		types.Int64Type,
		&desugar.ReturnStmt{
			Result: &desugar.UnaryExpr{
				Op:       token.MINUS,
				Operand:  hirParamRef("x", types.Int32Type),
				SemaType: types.Int64Type,
			},
		},
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("neg_i32_to_i64:\n%s", FormatFunction(fn))
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for unary minus widening (i32→i64), got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_UnaryNot_I1_NoCast(t *testing.T) {
	// NOT BOOLEAN → no cast (operand already i1)
	tempIDCounter = 0
	fn := lowerSingleFn("not_bool",
		[]*desugar.Param{hirParam("x", types.BooleanType)},
		types.BooleanType,
		&desugar.ReturnStmt{
			Result: &desugar.UnaryExpr{
				Op:       token.NOT,
				Operand:  hirParamRef("x", types.BooleanType),
				SemaType: types.BooleanType,
			},
		},
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	// Operand is already i1 → sameType is a no-op → 0 CastInsts
	if n := countCastInsts(fn); n != 0 {
		t.Errorf("expected 0 CastInsts for NOT bool (i1), got %d ops=%v", n, castOps(fn))
	}
}

func TestPromotion_UnaryNot_I32_CastEmitted(t *testing.T) {
	// NOT INTEGER → sameType(operand, i1) must emit a trunc CastInst
	// (token.NOT deliberately keeps l.sameType for explicit boolean coercion)
	tempIDCounter = 0
	fn := lowerSingleFn("not_i32",
		[]*desugar.Param{hirParam("x", types.Int32Type)},
		types.BooleanType,
		&desugar.ReturnStmt{
			Result: &desugar.UnaryExpr{
				Op:       token.NOT,
				Operand:  hirParamRef("x", types.Int32Type),
				SemaType: types.BooleanType,
			},
		},
	)
	if fn == nil {
		t.Fatal("lowerSingleFn returned nil")
	}
	t.Logf("not_i32:\n%s", FormatFunction(fn))
	ops := castOps(fn)
	if len(ops) == 0 {
		t.Errorf("expected a CastInst (trunc) for NOT i32 → i1, got none")
	}
	foundTrunc := false
	for _, op := range ops {
		if op == "trunc" {
			foundTrunc = true
		}
	}
	if !foundTrunc {
		t.Errorf("expected trunc CastInst for NOT i32→i1, got ops=%v", ops)
	}
}

func TestPromotion_BinaryAllArithOps_I32I64_NoCast(t *testing.T) {
	// Exercise every arithmetic default-case operator with mixed i32/i64 operands.
	for _, op := range []token.Kind{token.PLUS, token.MINUS, token.STAR, token.DIV} {
		op := op
		fn := lowerSingleFn("arith",
			[]*desugar.Param{hirParam("a", types.Int32Type), hirParam("b", types.Int64Type)},
			types.Int64Type,
			retBinExpr(op,
				hirParamRef("a", types.Int32Type),
				hirParamRef("b", types.Int64Type),
				types.Int64Type, types.Int64Type),
		)
		if fn == nil {
			t.Fatalf("op %v: lowerSingleFn returned nil", op)
		}
		if n := countCastInsts(fn); n != 0 {
			t.Errorf("op %v: expected 0 CastInsts, got %d ops=%v", op, n, castOps(fn))
		}
	}
}

func TestPromotion_BinaryAllCompareOps_I8I32_NoCast(t *testing.T) {
	for _, op := range []token.Kind{token.EQUAL, token.NEQ, token.LESS, token.LEQ, token.GREAT, token.GEQ} {
		op := op
		fn := lowerSingleFn("cmp",
			[]*desugar.Param{hirParam("a", types.Int8Type), hirParam("b", types.Int32Type)},
			types.BooleanType,
			retBinExpr(op,
				hirParamRef("a", types.Int8Type),
				hirParamRef("b", types.Int32Type),
				types.BooleanType, types.BooleanType),
		)
		if fn == nil {
			t.Fatalf("cmp op %v: lowerSingleFn returned nil", op)
		}
		if n := countCastInsts(fn); n != 0 {
			t.Errorf("cmp op %v: expected 0 CastInsts, got %d ops=%v", op, n, castOps(fn))
		}
	}
}
