package types

import (
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Type interface {
	String() string
	Width() int
	Alignment() int
	Equals(Type) bool
}

func TypeIncludes(a, b Type) bool {
	if a == b {
		return true
	}

	switch a {
	case Int64Type, LongIntType:
		return b == Int32Type || b == Int16Type || b == Int8Type || b == ByteType ||
			b == IntegerType || b == ShortIntType || b == Int64Type || b == LongIntType
	case Int32Type, IntegerType:
		return b == Int16Type || b == Int8Type || b == ByteType || b == ShortIntType ||
			b == Int32Type || b == IntegerType
	case Int16Type, ShortIntType:
		return b == Int8Type || b == ByteType || b == Int16Type || b == ShortIntType
	case LongRealType:
		// LONGREAL (64-bit double, 53-bit mantissa) can exactly represent INT32 and smaller.
		return b == RealType || b == Int32Type || b == IntegerType || b == Int16Type ||
			b == ShortIntType || b == Int8Type || b == ByteType
	case RealType:
		// REAL (32-bit float, 24-bit mantissa) can exactly represent INT16 and smaller.
		return b == Int16Type || b == ShortIntType || b == Int8Type || b == ByteType
	case WCharType:
		return b == CharType
	case CharType:
		s, ok := b.(*StringType)
		return ok && s.Length == 1
	}

	return false
}

func SameType(Ta, Tb Type) bool {
	if Ta == nil || Tb == nil {
		return false
	}

	if Ta == Tb {
		return true
	}

	if namedA, ok := Ta.(*NamedType); ok {
		if namedB, ok := Tb.(*NamedType); ok {
			return namedA.Name == namedB.Name
		}
	}

	return Identical(Underlying(Ta), Underlying(Tb))
}

func EqualType(Ta, Tb Type) bool {
	if SameType(Ta, Tb) {
		return true
	}

	switch at := Ta.(type) {
	case *ArrayType:
		bt, ok := Tb.(*ArrayType)
		if !ok {
			return false
		}

		// either array is not open
		if !at.IsOpen() || !bt.IsOpen() {
			return false
		}

		// Open arrays
		return EqualType(at.Elem, bt.Elem)
	case *PointerType:
		bt, ok := Tb.(*PointerType)
		if !ok {
			return false
		}

		return EqualType(at.Base, bt.Base)
	case *ProcedureType:
		bt, ok := Tb.(*ProcedureType)
		if !ok {
			return false
		}
		return FormalParamsListMatch(at.Params, bt.Params) && ResultTypeMatch(at.Result, bt.Result)
	default:

	}

	return false
}

func FormalParamsListMatch(Pa, Pb []*FormalParam) bool {
	if len(Pa) != len(Pb) {
		return false
	}

	for i := range Pa {
		if Pa[i].Kind != Pb[i].Kind {
			return false
		}
		if !EqualType(Pa[i].Type, Pb[i].Type) {
			return false
		}
	}

	return true
}

func ResultTypeMatch(Ta, Tb Type) bool {
	// Both procedures are commands (no return value) — they match.
	if Ta == nil && Tb == nil {
		return true
	}
	return SameType(Ta, Tb)
}

func AssignmentCompatible(exprType, varType Type) bool {
	if exprType == nil || varType == nil {
		return false
	}

	exprType = Underlying(exprType)
	varType = Underlying(varType)

	// 1. Same type
	if SameType(exprType, varType) {
		return true
	}

	// 2. Te and Tv are numeric or character types and Tv includes Te
	if IsNumeric(exprType) && IsNumeric(varType) && TypeIncludes(varType, exprType) {
		return true
	}
	if IsChar(exprType) && IsChar(varType) && TypeIncludes(varType, exprType) {
		return true
	}

	// 3. Tv is a SET and Te is INT32 or smaller
	if IsSet(varType) && IsInteger(exprType) && TypeIncludes(Int32Type, exprType) {
		return true
	}

	// 4. Tv is BYTE, Te is Latin-1 character
	if IsByte(varType) && IsLatin1Char(exprType) {
		return true
	}

	// 5. Tv is integer, Te is enumeration
	if IsInteger(varType) && IsEnum(exprType) {
		return true
	}

	// 6. Te and Tv are record types and Te is a subtype of Tv
	if et, ok := exprType.(*RecordType); ok {
		if vt, ok2 := varType.(*RecordType); ok2 && IsExtensionOf(et, vt) {
			return true
		}
	}

	// 7. Te and Tv are pointer types, and base types match or are subtype
	if ep, ok := exprType.(*PointerType); ok {
		if vp, ok2 := varType.(*PointerType); ok2 {
			if SameType(ep.Base, vp.Base) || IsExtensionOf(ep.Base, vp.Base) {
				return true
			}
		}
	}

	// 8. Tv is pointer or procedure type and expr is NIL
	if (IsPointer(varType) || IsProcedure(varType)) && IsNil(exprType) {
		return true
	}

	// 9. Te is open array, Tv is array with equal base type
	if eo, ok := exprType.(*ArrayType); ok && eo.IsOpen() {
		if va, ok2 := varType.(*ArrayType); ok2 && !va.IsOpen() {
			if EqualType(eo.Elem, va.Elem) {
				return true
			}
		}
	}

	// 10. Tv is array of WCHAR, Te is suitable string or array, and STRLEN(Te) < LEN(Tv)
	if IsArrayOf(varType, WCharType) && IsCharArrayOrString(exprType) {
		TvLen := varType.(*ArrayType).Length

		var TeLen int
		if str, isStr := exprType.(*StringType); isStr {
			TeLen = str.Length
		}

		if arr, isArr := exprType.(*ArrayType); isArr {
			TeLen = arr.Length
		}

		return TeLen < TvLen
	}

	// 11. Tv is array of CHAR, Te is Latin-1 string or array, and STRLEN(Te) < LEN(Tv)
	if IsArrayOf(varType, CharType) && IsLatin1CharArrayOrString(exprType) {
		TvLen := varType.(*ArrayType).Length

		var TeLen int
		if str, isStr := exprType.(*StringType); isStr {
			TeLen = str.Length
		}

		if arr, isArr := exprType.(*ArrayType); isArr {
			TeLen = arr.Length
		}

		return TeLen < TvLen
	}

	// 12. Tv is procedure type and e is a procedure name with matching signature
	if ptv, ok := varType.(*ProcedureType); ok {
		if pte, ok2 := exprType.(*ProcedureType); ok2 {
			return EqualType(ptv, pte)
		}
	}

	return false
}

func ParameterCompatible(actual Type, formal *FormalParam) bool {
	tf := formal.Type // formal parameter type
	ta := actual      // actual parameter type

	// 1. Equal types
	if EqualType(ta, tf) {
		return true
	}

	switch formal.Kind {
	case "value", "VALUE", "": // value is default
		// 2. Value parameters can accept assignment-compatible expressions
		return AssignmentCompatible(ta, tf)

	case "VAR", "IN", "OUT":
		// 3. Must be the same type
		if SameType(ta, tf) {
			return true
		}
		// ... or record type extension
		if IsRecord(tf) && IsRecord(ta) && IsExtensionOf(ta, tf) {
			return true
		}
		return false

	default:
		// Unknown kind (should be caught elsewhere)
		return false
	}
}

func ArrayCompatible(actual, formal Type) bool {
	// Case 1: Equal types
	if EqualType(actual, formal) {
		return true
	}

	// Case 2: Open array matching any array, element types must be array compatible
	formalArr, okF := formal.(*ArrayType)
	actualArr, okA := actual.(*ArrayType)

	if okF && formalArr.IsOpen() {
		// Case 5: BYTE string
		if formalArr.Elem == ByteType && IsLatin1OrByteString(actual) {
			return true
		}

		// Case 4: WCHAR string
		if formalArr.Elem == WCharType && IsUnicodeOrLatin1String(actual) {
			return true
		}

		// Case 3: CHAR string
		if formalArr.Elem == CharType && IsLatin1OrByteString(actual) {
			return true
		}

		if okA {
			return ArrayCompatible(actualArr.Elem, formalArr.Elem)
		}
	}

	return false
}

func ExpressionCompatible(op token.Kind, lhs, rhs Type) (bool, Type) {
	// Unwrap named types before any checks so aliases behave identically to
	// their underlying basic types throughout this function.
	lhs = Underlying(lhs)
	rhs = Underlying(rhs)

	switch op {
	case token.PLUS, token.MINUS, token.STAR, token.QUOT:
		if IsNumeric(lhs) && IsNumeric(rhs) && op != token.QUOT {
			return true, SmallestNumericType(lhs, rhs)
		}

		// QUOT (/) is only defined for REAL operands (and SET, handled below).
		// Using DIV for integer division is required by the spec.
		if IsReal(lhs) && IsReal(rhs) && op == token.QUOT {
			return true, SmallestRealType(lhs, rhs)
		}

		if IsSet(lhs) && IsSet(rhs) {
			return true, lhs // SET op SET → SET
		}
	case token.DIV, token.MOD:
		if IsInteger(lhs) && IsInteger(rhs) {
			return true, SmallestIntegerType(lhs, rhs)
		}
	case token.OR, token.AND, token.NOT:
		if IsBoolean(lhs) && IsBoolean(rhs) {
			return true, BooleanType
		}
	case token.EQUAL, token.NEQ, token.LESS, token.LEQ, token.GEQ, token.GREAT:
		if IsNumeric(lhs) && IsNumeric(rhs) {
			return true, BooleanType
		}

		if IsEnum(lhs) && IsEnum(rhs) {
			return true, BooleanType
		}

		// BOOLEAN may only be compared for equality/inequality, not ordered.
		if IsBoolean(lhs) && IsBoolean(rhs) {
			if op == token.EQUAL || op == token.NEQ {
				return true, BooleanType
			}
			return false, nil
		}

		if IsSet(lhs) && IsSet(rhs) {
			return true, BooleanType
		}

		if IsPointer(lhs) && IsPointer(rhs) {
			return true, BooleanType
		}
		if IsProcedure(lhs) && IsProcedure(rhs) {
			return true, BooleanType
		}
		if IsNil(lhs) && (IsPointer(rhs) || IsProcedure(rhs)) {
			return true, BooleanType
		}
		if IsNil(rhs) && (IsPointer(lhs) || IsProcedure(lhs)) {
			return true, BooleanType
		}

		if IsNil(lhs) && IsNil(rhs) {
			return true, BooleanType
		}

		if IsChar(lhs) && IsChar(rhs) {
			return true, BooleanType
		}

		// Latin-1 (CHAR) arrays and string literals
		if IsLatin1CharArrayOrString(lhs) && IsLatin1CharArrayOrString(rhs) {
			return true, BooleanType
		}

		// WCHAR arrays
		if IsWCharArrayOrString(lhs) && IsWCharArrayOrString(rhs) {
			return true, BooleanType
		}
	case token.IN:
		if IsInteger(lhs) && IsSet(rhs) {
			return true, BooleanType
		}
	case token.IS:
		// lhs: variable (pointer to record or VAR-param record), rhs: type name
		if IsPointer(lhs) && IsPointer(rhs) {
			return true, BooleanType
		}
		// VAR parameter of record type: v IS T
		if IsRecord(lhs) && IsRecord(rhs) {
			return true, BooleanType
		}
	}
	return false, nil
}

// Underlying returns the canonical type by unwrapping named types.
func Underlying(t Type) Type {
	for {
		if named, ok := t.(*NamedType); ok {
			t = named.Def
		} else {
			return t
		}
	}
}

// Identical returns true if two types are structurally identical.
func Identical(a, b Type) bool {
	if a == nil || b == nil {
		return false
	}
	if a == b {
		return true
	}
	switch a := a.(type) {
	case *BasicType:
		bb, ok := b.(*BasicType)
		// Two basic types are identical iff they have the same kind, or are
		// recognized Oberon type aliases (INTEGER≡INT32, SHORTINT≡INT16,
		// LONGINT≡INT64).  TypeIncludes must NOT be used here because it
		// expresses subsumption (e.g. WCHAR includes CHAR), not identity.
		return ok && (a.Kind == bb.Kind || sameAliasKind(a.Kind, bb.Kind))
	case *ArrayType:
		ab, ok := b.(*ArrayType)
		if !ok {
			return false
		}
		// Two open arrays are identical iff their element types are identical.
		if a.IsOpen() && ab.IsOpen() {
			return Identical(a.Elem, ab.Elem)
		}
		// Two fixed arrays are identical iff same length and element type.
		return !a.IsOpen() && !ab.IsOpen() && a.Length == ab.Length && Identical(a.Elem, ab.Elem)
	case *PointerType:
		pb, ok := b.(*PointerType)
		return ok && Identical(a.Base, pb.Base)
	case *StringType:
		str, ok := b.(*StringType)
		if ok {
			return a.Length == str.Length
		}
		// A length-1 string literal is identical to CHAR or WCHAR.
		return a.Length == 1 && (b == CharType || b == WCharType)
	// Add more composite types as needed
	default:
		return false
	}
}
