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
	ByteType     = &BasicType{BYTE, 1, 1}
	Int8Type     = &BasicType{INT8, 1, 1}
	Int16Type    = &BasicType{INT16, 2, 2}
	Int32Type    = &BasicType{INT32, 4, 4}
	Int64Type    = &BasicType{INT64, 8, 8}
	RealType     = &BasicType{REAL, 4, 4}
	LongRealType = &BasicType{LONGREAL, 8, 8}
	CharType     = &BasicType{CHAR, 1, 1}
	WCharType    = &BasicType{WCHAR, 2, 2}
	BooleanType  = &BasicType{BOOLEAN, 1, 1}
	NilType      = &BasicType{NIL, 8, 8}
	SetType      = &BasicType{SET, 4, 4}

	UnknownType = &BasicType{UNKNOWN, 0, 0}
)

type BasicType struct {
	Kind  BasicKind
	Size  int
	Align int
}

func (b *BasicType) Width() int { return b.Size }

func (b *BasicType) Alignment() int {
	panic("Not implemented")
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
	UNKNOWN:  "unknown",
}
