package types

import (
	"github.com/anthonyabeo/obx/src/syntax/token"
)

var PredefinedTypes = map[token.Kind]Type{
	token.BYTE:     ByteType,
	token.INT8:     Int8Type,
	token.INT16:    Int16Type,
	token.INT32:    Int32Type,
	token.INT64:    Int64Type,
	token.SHORTINT: ShortIntType,
	token.INTEGER:  IntegerType,
	token.LONGINT:  LongIntType,
	token.REAL:     RealType,
	token.LONGREAL: LongRealType,
	token.CHAR:     CharType,
	token.WCHAR:    WCharType,
	token.SET:      SetType,
	token.BOOLEAN:  BooleanType,
}

var integerTypeRank = map[BasicKind]int{
	INT8:     0,
	BYTE:     1,
	INT16:    2,
	INT32:    3,
	INT64:    4,
	SHORTINT: 2,
	INTEGER:  3,
	LONGINT:  4,
}

var numericTypeRank = map[BasicKind]int{
	INT8:     0,
	BYTE:     1,
	INT16:    2,
	INT32:    3,
	INT64:    4,
	SHORTINT: 2,
	INTEGER:  3,
	LONGINT:  4,
	REAL:     5,
	LONGREAL: 6,
}

var realTypeRank = map[BasicKind]int{
	REAL:     0,
	LONGREAL: 1,
}

func SmallestIntegerType(a, b Type) Type {
	ra, ok1 := integerTypeRank[UnderlyingBasic(a).Kind]
	rb, ok2 := integerTypeRank[UnderlyingBasic(b).Kind]
	if ok1 && ok2 {
		if ra < rb {
			return b
		}
		return a
	}
	return nil // not both integer types
}

func SmallestNumericType(a, b Type) Type {
	ra, ok1 := numericTypeRank[UnderlyingBasic(a).Kind]
	rb, ok2 := numericTypeRank[UnderlyingBasic(b).Kind]
	if ok1 && ok2 {
		if ra < rb {
			return b
		}
		return a
	}
	return nil
}

func SmallestRealType(a, b Type) Type {
	ra, ok1 := realTypeRank[UnderlyingBasic(a).Kind]
	rb, ok2 := realTypeRank[UnderlyingBasic(b).Kind]
	if ok1 && ok2 {
		if ra < rb {
			return b
		}
		return a
	}
	return nil
}

func UnderlyingBasic(t Type) *BasicType {
	switch tt := t.(type) {
	case *BasicType:
		return tt
	case *NamedType:
		return UnderlyingBasic(tt.Def)
	default:
		return nil
	}
}

func IsBoolean(ty Type) bool {
	return ty == BooleanType
}

func IsInteger(ty Type) bool {
	switch ty {
	case ByteType, Int8Type, Int16Type, ShortIntType, IntegerType, Int32Type, LongIntType, Int64Type:
		return true
	}
	return false
}

func IsReal(ty Type) bool {
	return ty == RealType || ty == LongRealType
}

func IsChar(ty Type) bool {
	return ty == CharType || ty == WCharType
}

func IsNumeric(ty Type) bool {
	return IsInteger(ty) || IsReal(ty)
}

func IsLatin1Char(t Type) bool {
	return t == CharType
}

func IsSet(t Type) bool {
	return t == SetType
}

func IsEnum(t Type) bool {
	_, ok := t.(*EnumType)
	return ok
}

func IsPointer(t Type) bool {
	_, ok := t.(*PointerType)
	return ok
}

func IsRecord(t Type) bool {
	_, ok := t.(*RecordType)
	return ok
}

func IsByte(t Type) bool {
	return t == ByteType
}

func IsProcedure(t Type) bool {
	_, ok := t.(*ProcedureType)
	return ok
}

func IsNil(t Type) bool {
	return t == NilType
}

func IsUnknownType(ty Type) bool {
	b, ok := ty.(*BasicType)
	if ok && b.Kind == UNKNOWN {
		return true
	}

	return false
}

func IsExtensionOf(sub, sup Type) bool {
	subRec, ok1 := sub.(*RecordType)
	supRec, ok2 := sup.(*RecordType)
	if !ok1 || !ok2 {
		return false
	}
	for r := subRec; r != nil; r = r.Base {
		if SameType(r, supRec) {
			return true
		}
	}
	return false
}

func IsArrayOf(t Type, elem Type) bool {
	a, ok := t.(*ArrayType)
	return ok && EqualType(a.Base, elem)
}

func IsCharArrayOrString(t Type) bool {
	if a, ok := t.(*ArrayType); ok {
		return a.Base == CharType || a.Base == WCharType
	}

	_, ok := t.(*StringType)

	return ok
}

func IsLatin1CharArrayOrString(t Type) bool {
	if a, ok := t.(*ArrayType); ok {
		return a.Base == CharType
	}

	_, ok := t.(*StringType)

	return ok
}

func IsLatin1OrByteString(t Type) bool {
	arr, ok := t.(*ArrayType)
	return ok && (arr.Base == CharType || arr.Base == ByteType)
}

func IsUnicodeOrLatin1String(t Type) bool {
	arr, ok := t.(*ArrayType)
	return ok && (arr.Base == WCharType || arr.Base == CharType || arr.Base == ByteType)
}
