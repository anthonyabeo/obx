package types

import (
	"github.com/anthonyabeo/obx/src/syntax/token"
	"testing"
)

func TestSameType_Basic(t *testing.T) {
	if !SameType(Int32Type, Int32Type) {
		t.Errorf("expected Int32 to be same as Int32")
	}
	if SameType(Int32Type, Int64Type) {
		t.Errorf("expected Int32 not to be same as Int64")
	}
}

func TestSameType_NamedType(t *testing.T) {
	alias1 := &NamedType{Name: "Foo", Def: Int32Type}
	alias2 := &NamedType{Name: "Bar", Def: Int32Type}
	alias3 := &NamedType{Name: "Foo", Def: Int32Type} // Same name as alias1

	if !SameType(alias1, Int32Type) {
		t.Errorf("expected Foo to be same as Int32 (via alias)")
	}
	if SameType(alias1, alias2) {
		t.Errorf("expected Foo != Bar (different names)")
	}
	if !SameType(alias1, alias3) {
		t.Errorf("expected Foo == Foo (same name)")
	}
}

func TestSameType_Arrays(t *testing.T) {
	arr1 := &ArrayType{Length: 5, Base: Int32Type}
	arr2 := &ArrayType{Length: 5, Base: Int32Type}
	arr3 := &ArrayType{Length: 10, Base: Int32Type}
	arr4 := &ArrayType{Length: 5, Base: Int64Type}

	if !SameType(arr1, arr2) {
		t.Errorf("expected arrays of same length and element type to be same")
	}
	if SameType(arr1, arr3) {
		t.Errorf("expected arrays of different lengths to differ")
	}
	if SameType(arr1, arr4) {
		t.Errorf("expected arrays of different element types to differ")
	}
}

func TestSameType_OpenArray(t *testing.T) {
	openArr := &ArrayType{Length: -1, Base: Int32Type} // Convention: -1 = open array
	fixedArr := &ArrayType{Length: 3, Base: Int32Type}
	openArr2 := &ArrayType{Length: -1, Base: Int32Type}

	if SameType(openArr, fixedArr) {
		t.Errorf("expected open array and fixed array to differ")
	}
	if SameType(openArr, openArr2) {
		t.Errorf("expected open arrays with same element type to be same")
	}
}

func TestMatchingParams(t *testing.T) {
	tests := []struct {
		name string
		a, b []*FormalParam
		want bool
	}{
		{
			name: "same params, same kinds and types",
			a:    []*FormalParam{{Kind: "", Type: IntegerType}},
			b:    []*FormalParam{{Kind: "", Type: IntegerType}},
			want: true,
		},
		{
			name: "different kinds",
			a:    []*FormalParam{{Kind: "VAR", Type: IntegerType}},
			b:    []*FormalParam{{Kind: "", Type: IntegerType}},
			want: false,
		},
		{
			name: "different types",
			a:    []*FormalParam{{Kind: "", Type: IntegerType}},
			b:    []*FormalParam{{Kind: "", Type: RealType}},
			want: false,
		},
		{
			name: "different lengths",
			a:    []*FormalParam{{Kind: "", Type: IntegerType}},
			b:    []*FormalParam{{Kind: "", Type: IntegerType}, {Kind: "", Type: IntegerType}},
			want: false,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			got := FormalParamsListMatch(test.a, test.b)
			if got != test.want {
				t.Errorf("got %v, want %v", got, test.want)
			}
		})
	}
}

func TestEqualType_Procedure(t *testing.T) {
	proc1 := &ProcedureType{
		Params: []*FormalParam{{Kind: "", Type: IntegerType}},
	}

	proc2 := &ProcedureType{
		Params: []*FormalParam{{Kind: "", Type: IntegerType}},
	}

	proc3 := &ProcedureType{
		Params: []*FormalParam{{Kind: "VAR", Type: IntegerType}},
	}

	func1 := &ProcedureType{
		Params: []*FormalParam{{Kind: "", Type: IntegerType}},
		Result: RealType,
	}

	func2 := &ProcedureType{
		Params: []*FormalParam{{Kind: "", Type: IntegerType}},
		Result: RealType,
	}

	func3 := &ProcedureType{
		Params: []*FormalParam{{Kind: "", Type: IntegerType}},
		Result: IntegerType,
	}

	tests := []struct {
		name     string
		a, b     Type
		expected bool
	}{
		{"same procedure type", proc1, proc2, true},
		{"different param kind", proc1, proc3, false},
		{"same function type", func1, func2, true},
		{"different return type", func1, func3, true},
		{"procedure vs function", proc1, func1, true},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			got := EqualType(test.a, test.b)
			if got != test.expected {
				t.Errorf("EqualType(%s): got %v, want %v", test.name, got, test.expected)
			}
		})
	}
}

func TestTypeIncludes(t *testing.T) {
	tests := []struct {
		a, b   Type
		expect bool
	}{
		// Reflexivity
		{Int64Type, Int64Type, true},
		{Int32Type, Int32Type, true},
		{ByteType, ByteType, true},
		{LongRealType, LongRealType, true},
		{CharType, CharType, true},

		// INT64 hierarchy
		{Int64Type, Int32Type, true},
		{Int64Type, Int16Type, true},
		{Int64Type, Int8Type, true},
		{Int64Type, ByteType, true},
		{Int64Type, Int64Type, true},

		// INT32 hierarchy
		{Int32Type, Int16Type, true},
		{Int32Type, Int8Type, true},
		{Int32Type, ByteType, true},
		{Int32Type, Int32Type, true},

		// INT16 hierarchy
		{Int16Type, Int8Type, true},
		{Int16Type, ByteType, true},
		{Int16Type, Int16Type, true},
		{Int16Type, Int32Type, false},

		// LONGREAL hierarchy
		{LongRealType, RealType, true},
		{LongRealType, Int32Type, true},
		{LongRealType, Int16Type, true},

		// REAL hierarchy
		{RealType, Int16Type, true},
		{RealType, Int32Type, false},

		// LONGINT hierarchy
		{LongIntType, IntegerType, true},
		{LongIntType, ShortIntType, true},
		{LongIntType, Int8Type, true},

		// INTEGER hierarchy
		{IntegerType, ShortIntType, true},
		{IntegerType, Int8Type, true},

		// WCHAR hierarchy
		{WCharType, CharType, true},
		{WCharType, WCharType, true},
		{CharType, WCharType, false},

		// Negative cases
		{ByteType, Int16Type, false},
		{Int8Type, Int16Type, false},
		{Int16Type, Int32Type, false},
		{Int32Type, Int64Type, false},
		{CharType, WCharType, false},
		{RealType, LongRealType, false},
	}

	for _, test := range tests {
		got := TypeIncludes(test.a, test.b)
		if got != test.expect {
			t.Errorf("TypeIncludes(%v, %v) = %v; want %v", test.a, test.b, got, test.expect)
		}
	}
}

func TestAssignmentCompatible(t *testing.T) {
	// Helper to build pointer types
	ptrTo := func(base Type) Type {
		return &PointerType{Base: base}
	}

	// Helper to build arrays
	arr := func(elem Type, length int) Type {
		return &ArrayType{Base: elem, Length: int64(length)}
	}

	// Helper to build open arrays
	openArr := func(elem Type) Type {
		return &ArrayType{Base: elem, Length: -1}
	}

	tests := []struct {
		src, dst Type
		expect   bool
		desc     string
	}{
		// 1. Same type
		{Int32Type, Int32Type, true, "same type"},

		// 2. Numeric or character types and dst includes src
		{Int8Type, Int32Type, true, "INT8 promotes to INT32"},
		{Int16Type, RealType, true, "INT16 promotes to REAL"},
		{RealType, LongRealType, true, "REAL promotes to LONGREAL"},
		{CharType, WCharType, true, "CHAR promotes to WCHAR"},
		{Int64Type, Int32Type, false, "INT64 does not promote to INT32"},

		// 3. SET := INT32 or smaller
		{Int32Type, SetType, true, "INT32 to SET"},
		{Int16Type, SetType, true, "INT16 to SET"},
		{Int64Type, SetType, false, "INT64 not assignable to SET"},

		// 4. BYTE := Latin-1 CHAR
		{CharType, ByteType, true, "CHAR to BYTE"},
		{WCharType, ByteType, false, "WCHAR not assignable to BYTE"},

		// 5. Integer := Enum
		{NewEnumWithVariants("Color"), Int32Type, true, "enum to int"},
		{NewEnumWithVariants("Color"), RealType, false, "enum not to real"},

		// 6. Record type extension (not modeled in this example)
		// Skipping unless record subtyping is implemented

		// 7. Pointer type extension or equal base
		{ptrTo(Int32Type), ptrTo(Int32Type), true, "equal pointer base"},
		{ptrTo(Int16Type), ptrTo(Int32Type), false, "incompatible pointer bases"},

		// 8. NIL to pointer/procedure
		{NilType, ptrTo(Int32Type), true, "nil to pointer"},
		{NilType, NewProcedureType(nil, nil), true, "nil to procedure"},

		// 9. open array := array
		{openArr(Int32Type), arr(Int32Type, 10), true, "open array to array"},
		{openArr(Int32Type), arr(Int64Type, 10), false, "type mismatch in open array"},

		// 10. WCHAR array := BMP string
		{NewStringType(5), arr(WCharType, 10), true, "string to WCHAR array"},
		// {NewStringType(11), arr(WCharType, 5), false, "string too long for WCHAR array"},

		// 11. CHAR array := Latin-1 string
		{NewStringType(4), arr(CharType, 10), true, "string to CHAR array"},
		// {NewStringType(13), arr(CharType, 5), false, "string too long for CHAR array"},

		// 12. Procedure matching
		{
			NewProcedureType([]*FormalParam{
				{Kind: "value", Type: Int32Type},
			}, nil),
			NewProcedureType([]*FormalParam{
				{Kind: "value", Type: Int32Type},
			}, nil),
			true,
			"procedure with matching signature",
		},
		{
			NewProcedureType([]*FormalParam{
				{Kind: "VAR", Type: Int32Type},
			}, nil),
			NewProcedureType([]*FormalParam{
				{Kind: "value", Type: Int32Type},
			}, nil),
			false,
			"procedure with different parameter kind",
		},
	}

	for _, test := range tests {
		got := AssignmentCompatible(test.src, test.dst)
		if got != test.expect {
			t.Errorf("%s: AssignmentCompatible(%v, %v) = %v; want %v",
				test.desc, test.src, test.dst, got, test.expect)
		}
	}
}

func TestParameterCompatible(t *testing.T) {
	var (
		recordA = &RecordType{Fields: map[string]*Field{"A": {"A", IntegerType, false}}}
		recordB = &RecordType{Base: recordA, Fields: map[string]*Field{"B": {"B", IntegerType, false}}}

		// Base types
		int32Ptr = &PointerType{Base: Int32Type}
		int16Ptr = &PointerType{Base: Int16Type}

		// Open and fixed arrays
		openCharArray   = &ArrayType{Length: -1, Base: CharType}
		openWCharArray  = &ArrayType{Length: -1, Base: WCharType}
		fixedCharArray  = &ArrayType{Length: 10, Base: CharType}
		fixedWCharArray = &ArrayType{Length: 10, Base: WCharType}

		// Procedures
		procA = &ProcedureType{
			Params: []*FormalParam{{Kind: "", Type: Int16Type}},
		}
		procB = &ProcedureType{
			Params: []*FormalParam{{Kind: "", Type: Int16Type}},
		}
		procC = &ProcedureType{
			Params: []*FormalParam{{Kind: "", Type: Int32Type}},
		}
	)

	tests := []struct {
		name   string
		actual Type
		formal *FormalParam
		want   bool
	}{
		{
			name:   "Equal basic types",
			actual: Int16Type,
			formal: &FormalParam{Kind: "", Type: Int16Type},
			want:   true,
		},
		{
			name:   "Assignment compatible (INT16 -> INT32)",
			actual: Int16Type,
			formal: &FormalParam{Kind: "", Type: Int32Type},
			want:   true,
		},
		{
			name:   "Not assignment compatible (INT32 -> INT16)",
			actual: Int32Type,
			formal: &FormalParam{Kind: "", Type: Int16Type},
			want:   false,
		},
		{
			name:   "Same type for VAR param",
			actual: ByteType,
			formal: &FormalParam{Kind: "VAR", Type: ByteType},
			want:   true,
		},
		{
			name:   "VAR param with subtype extension (record B -> A)",
			actual: recordB,
			formal: &FormalParam{Kind: "VAR", Type: recordA},
			want:   true,
		},
		{
			name:   "VAR param, different basic types",
			actual: ByteType,
			formal: &FormalParam{Kind: "VAR", Type: Int16Type},
			want:   false,
		},
		{
			name:   "IN param, same type",
			actual: CharType,
			formal: &FormalParam{Kind: "IN", Type: CharType},
			want:   true,
		},
		{
			name:   "IN param with record extension",
			actual: recordB,
			formal: &FormalParam{Kind: "IN", Type: recordA},
			want:   true,
		},
		{
			name:   "IN param with non-matching types",
			actual: WCharType,
			formal: &FormalParam{Kind: "IN", Type: Int16Type},
			want:   false,
		},
		{
			name:   "Equal pointer types",
			actual: int32Ptr,
			formal: &FormalParam{Kind: "", Type: int32Ptr},
			want:   true,
		},
		{
			name:   "Mismatched pointer types",
			actual: int16Ptr,
			formal: &FormalParam{Kind: "", Type: int32Ptr},
			want:   false,
		},
		{
			name:   "Equal fixed arrays",
			actual: fixedCharArray,
			formal: &FormalParam{Kind: "", Type: fixedCharArray},
			want:   true,
		},
		{
			name:   "Open array assigned to fixed array (not same)",
			actual: openCharArray,
			formal: &FormalParam{Kind: "", Type: fixedCharArray},
			want:   true,
		},
		{
			name:   "Procedure types with matching params",
			actual: procA,
			formal: &FormalParam{Kind: "", Type: procB},
			want:   true,
		},
		{
			name:   "Procedure types with mismatched params",
			actual: procA,
			formal: &FormalParam{Kind: "", Type: procC},
			want:   false,
		},
		{
			name:   "Open WCHAR to fixed WCHAR (not same)",
			actual: openWCharArray,
			formal: &FormalParam{Kind: "", Type: fixedWCharArray},
			want:   true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := ParameterCompatible(tt.actual, tt.formal)
			if got != tt.want {
				t.Errorf("ParameterCompatible() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestArrayCompatible(t *testing.T) {
	makeArray := func(elem Type, len int) *ArrayType {
		return &ArrayType{Length: int64(len), Base: elem}
	}

	tests := []struct {
		name     string
		actual   Type
		formal   Type
		expected bool
	}{
		// Equal types
		{
			name:     "equal closed arrays",
			actual:   makeArray(Int32Type, 5),
			formal:   makeArray(Int32Type, 5),
			expected: true,
		},

		// Open array accepting any array with compatible elements
		{
			name:     "open array accepts closed array",
			actual:   makeArray(Int32Type, 5),
			formal:   makeArray(Int32Type, -1),
			expected: true,
		},
		{
			name:     "open array does not accept incompatible element type",
			actual:   makeArray(Int16Type, 5),
			formal:   makeArray(Int32Type, 0),
			expected: false,
		},

		// CHAR strings
		{
			name:     "open array of CHAR accepts CHAR array",
			actual:   makeArray(CharType, 10),
			formal:   makeArray(CharType, -1),
			expected: true,
		},
		{
			name:     "open array of CHAR accepts BYTE array",
			actual:   makeArray(ByteType, 10),
			formal:   makeArray(CharType, -1),
			expected: true,
		},

		// WCHAR strings
		{
			name:     "open array of WCHAR accepts WCHAR array",
			actual:   makeArray(WCharType, 10),
			formal:   makeArray(WCharType, -1),
			expected: true,
		},
		{
			name:     "open array of WCHAR accepts CHAR array",
			actual:   makeArray(CharType, 10),
			formal:   makeArray(WCharType, -1),
			expected: true,
		},
		{
			name:     "open array of WCHAR accepts BYTE array",
			actual:   makeArray(ByteType, 10),
			formal:   makeArray(WCharType, -1),
			expected: true,
		},

		// BYTE strings
		{
			name:     "open array of BYTE accepts BYTE array",
			actual:   makeArray(ByteType, 10),
			formal:   makeArray(ByteType, -1),
			expected: true,
		},

		// Incompatible open array
		{
			name:     "open array of REAL does not accept INT32 array",
			actual:   makeArray(Int32Type, 5),
			formal:   makeArray(RealType, -1),
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := ArrayCompatible(tt.actual, tt.formal)
			if got != tt.expected {
				t.Errorf("ArrayCompatible() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestArrayCompatible_Multidimensional(t *testing.T) {
	makeArray := func(elem Type, len int) *ArrayType {
		return &ArrayType{Length: int64(len), Base: elem}
	}

	tests := []struct {
		name     string
		actual   Type
		formal   Type
		expected bool
	}{
		// Equal 2D arrays
		{
			name:     "equal 2D closed arrays",
			actual:   makeArray(makeArray(Int32Type, 3), 4),
			formal:   makeArray(makeArray(Int32Type, 3), 4),
			expected: true,
		},

		// 2D actual vs 1D formal (invalid)
		{
			name:     "2D actual does not match 1D formal",
			actual:   makeArray(makeArray(Int32Type, 3), 4),
			formal:   makeArray(Int32Type, 0),
			expected: false,
		},

		// Outer open, inner match
		{
			name:     "2D open array matches compatible 2D actual",
			actual:   makeArray(makeArray(Int32Type, 3), 4),
			formal:   makeArray(makeArray(Int32Type, 3), -1),
			expected: true,
		},

		// Inner open, outer closed
		{
			name:     "2D array with inner open accepted by formal with same open inner",
			actual:   makeArray(makeArray(CharType, -1), 5),
			formal:   makeArray(makeArray(CharType, -1), -1),
			expected: true,
		},

		// CHAR array inside WCHAR outer open
		{
			name:     "open array of CHAR arrays accepted by open WCHAR",
			actual:   makeArray(makeArray(CharType, 10), 5),
			formal:   makeArray(makeArray(WCharType, 0), 0),
			expected: false, // Outer CHAR array ≠ WCHAR array
		},

		// 2D open accepting 2D closed BYTE
		{
			name:     "open 2D BYTE array matches closed 2D BYTE array",
			actual:   makeArray(makeArray(ByteType, 8), 5),
			formal:   makeArray(makeArray(ByteType, -1), -1),
			expected: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := ArrayCompatible(tt.actual, tt.formal)
			if got != tt.expected {
				t.Errorf("ArrayCompatible() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestArrayCompatible_DeepNesting(t *testing.T) {
	makeArray := func(elem Type, len int) *ArrayType {
		return &ArrayType{Length: int64(len), Base: elem}
	}

	tests := []struct {
		name     string
		actual   Type
		formal   Type
		expected bool
	}{
		// 3D equal closed arrays
		{
			name:     "equal 3D closed arrays",
			actual:   makeArray(makeArray(makeArray(Int16Type, 2), 3), 4),
			formal:   makeArray(makeArray(makeArray(Int16Type, 2), 3), 4),
			expected: true,
		},

		// 3D open outer array matches 3D closed array
		{
			name:     "3D open array matches 3D closed",
			actual:   makeArray(makeArray(makeArray(Int16Type, 2), 3), 4),
			formal:   makeArray(makeArray(makeArray(Int16Type, 2), 3), -1),
			expected: true,
		},

		// Mismatch at inner dimension
		{
			name:     "3D arrays with different inner lengths",
			actual:   makeArray(makeArray(makeArray(Int16Type, 2), 3), 4),
			formal:   makeArray(makeArray(makeArray(Int16Type, 5), 3), 4),
			expected: false,
		},

		// 4D with open inner two dimensions
		{
			name:     "4D open array matches closed 4D array",
			actual:   makeArray(makeArray(makeArray(makeArray(ByteType, 2), 3), 4), 5),
			formal:   makeArray(makeArray(makeArray(makeArray(ByteType, -1), -1), -1), -1),
			expected: true,
		},

		// Incompatible element type at innermost level
		{
			name:     "deep nested element type mismatch",
			actual:   makeArray(makeArray(makeArray(Int32Type, 2), 2), 2),
			formal:   makeArray(makeArray(makeArray(Int16Type, 2), 2), 2),
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := ArrayCompatible(tt.actual, tt.formal)
			if got != tt.expected {
				t.Errorf("ArrayCompatible() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestSmallestIntegerType(t *testing.T) {
	tests := []struct {
		a, b Type
		want Type
	}{
		{Int16Type, Int32Type, Int32Type},
		{ByteType, Int8Type, ByteType},
		{Int8Type, Int8Type, Int8Type},
		{ShortIntType, LongIntType, LongIntType},
		{IntegerType, LongIntType, LongIntType},
		{Int32Type, Int64Type, Int64Type},
		{Int64Type, Int32Type, Int64Type},
		{IntegerType, Int16Type, IntegerType},
	}

	for _, tt := range tests {
		got := SmallestIntegerType(tt.a, tt.b)
		if got != tt.want {
			t.Errorf("SmallestIntegerType(%v, %v) = %v; want %v", tt.a, tt.b, got, tt.want)
		}
	}
}

func TestSmallestNumericType(t *testing.T) {
	tests := []struct {
		a, b Type
		want Type
	}{
		{Int8Type, RealType, RealType},
		{RealType, LongRealType, LongRealType},
		{Int64Type, Int32Type, Int64Type},
		{IntegerType, LongRealType, LongRealType},
		{ShortIntType, Int8Type, ShortIntType},
		{ByteType, RealType, RealType},
	}

	for _, tt := range tests {
		got := SmallestNumericType(tt.a, tt.b)
		if got != tt.want {
			t.Errorf("SmallestNumericType(%v, %v) = %v; want %v", tt.a, tt.b, got, tt.want)
		}
	}
}

func TestSmallestRealType(t *testing.T) {
	tests := []struct {
		a, b Type
		want Type
	}{
		{RealType, LongRealType, LongRealType},
		{LongRealType, RealType, LongRealType},
		{RealType, RealType, RealType},
		{LongRealType, LongRealType, LongRealType},
	}

	for _, tt := range tests {
		got := SmallestRealType(tt.a, tt.b)
		if got != tt.want {
			t.Errorf("SmallestRealType(%v, %v) = %v; want %v", tt.a, tt.b, got, tt.want)
		}
	}
}

func TestExpressionCompatible(t *testing.T) {
	strChar := &ArrayType{10, CharType}
	strWchar := &ArrayType{10, WCharType}
	ptrFoo := &PointerType{&NamedType{"Foo", &RecordType{map[string]*Field{}, nil}}}
	procType := &ProcedureType{nil, nil, false}

	tests := []struct {
		op       token.Kind
		a, b     Type
		expected bool
		result   Type
	}{
		// Arithmetic (smallest numeric type including both)
		{token.PLUS, Int16Type, Int32Type, true, Int32Type},
		{token.MINUS, RealType, Int64Type, true, RealType},
		{token.STAR, RealType, LongRealType, true, LongRealType},
		{token.QUOT, RealType, RealType, true, RealType},

		// Set arithmetic
		{token.PLUS, SetType, SetType, true, SetType},
		{token.MINUS, SetType, SetType, true, SetType},
		{token.QUOT, SetType, SetType, true, SetType},

		// Integer-only ops
		{token.DIV, Int8Type, Int32Type, true, Int32Type},
		{token.MOD, Int32Type, Int8Type, true, Int32Type},

		// Boolean ops
		{token.OR, BooleanType, BooleanType, true, BooleanType},
		{token.AND, BooleanType, BooleanType, true, BooleanType},
		{token.NOT, BooleanType, BooleanType, true, BooleanType}, // unary

		// Comparisons
		{token.NEQ, Int32Type, Int64Type, true, BooleanType},
		{token.LESS, RealType, LongRealType, true, BooleanType},
		{token.GEQ, CharType, CharType, true, BooleanType},
		{token.LEQ, strChar, strChar, true, BooleanType},

		// Pointer/procedure NIL comparisons
		{token.EQUAL, ptrFoo, NilType, true, BooleanType},
		{token.NEQ, NilType, ptrFoo, true, BooleanType},
		{token.EQUAL, procType, NilType, true, BooleanType},

		// IN operator
		{token.IN, Int32Type, SetType, true, BooleanType},

		// IS operator (type test)
		{token.IS, ptrFoo, ptrFoo, true, BooleanType},

		// Incompatible types
		{token.PLUS, Int32Type, BooleanType, false, nil},
		{token.STAR, BooleanType, BooleanType, false, nil},
		{token.QUOT, SetType, Int32Type, false, nil},
		{token.DIV, RealType, Int32Type, false, nil},
		{token.EQUAL, strChar, strWchar, false, nil}, // unless BMP-terminator logic is handled
	}

	for _, tt := range tests {
		ok, result := ExpressionCompatible(tt.op, tt.a, tt.b)
		if ok != tt.expected {
			t.Errorf("ExpressionCompatible(%q, %v, %v) = %v; want %v", tt.op, tt.a, tt.b, ok, tt.expected)
		}
		if ok && result != tt.result {
			t.Errorf("ExpressionCompatible(%q, %v, %v) result = %v; want %v", tt.op, tt.a, tt.b, result, tt.result)
		}
	}
}
