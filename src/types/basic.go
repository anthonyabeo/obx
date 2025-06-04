package types

type BasicKind int

const (
	UNKNOWN BasicKind = iota
	BYTE
	INT8
	INT16
	INT32
	INT64
	SHORTINT
	INTEGER
	LONGINT
	REAL
	LONGREAL
	BOOLEAN
	CHAR
	WCHAR
	SET
	NIL
)

var (
	ByteType     = &BasicType{BYTE}
	Int8Type     = &BasicType{INT8}
	Int16Type    = &BasicType{INT16}
	Int32Type    = &BasicType{INT32}
	Int64Type    = &BasicType{INT64}
	ShortIntType = &BasicType{SHORTINT}
	IntegerType  = &BasicType{INTEGER}
	LongIntType  = &BasicType{LONGINT}
	RealType     = &BasicType{REAL}
	LongRealType = &BasicType{LONGREAL}
	CharType     = &BasicType{CHAR}
	WCharType    = &BasicType{WCHAR}
	BooleanType  = &BasicType{BOOLEAN}
	NilType      = &BasicType{NIL}
	SetType      = &BasicType{SET}

	UnknownType = &BasicType{UNKNOWN}
)

type BasicType struct {
	Kind BasicKind
}

func (b *BasicType) String() string {
	return basicKindToString[b.Kind]
}

func (b *BasicType) Equals(other Type) bool {
	o, ok := other.(*BasicType)
	return ok && b.Kind == o.Kind
}

var basicKindToString = map[BasicKind]string{
	BYTE:     "byte",
	INT8:     "int8",
	INT16:    "int16",
	INT32:    "int32",
	INT64:    "int64",
	SHORTINT: "shortint",
	INTEGER:  "integer",
	LONGINT:  "longint",
	REAL:     "real",
	LONGREAL: "longreal",
	BOOLEAN:  "bool",
	CHAR:     "char",
	WCHAR:    "wchar",
	SET:      "set",
}
