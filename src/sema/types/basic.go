package types

import "fmt"

// BasicKind describes the kind of basic type.
type BasicKind int

const (
	Invalid BasicKind = iota // type is invalid

	// predeclared types
	Bool
	Byte
	Char
	Int8
	Int16
	WChar
	Int32
	Int64
	Real
	LReal
	Set

	SInt = Int16
	Int  = Int32
	LInt = Int64
)

// BasicInfo is a set of flags describing properties of a basic type.
type BasicInfo int

// Properties of basic types.
const (
	IsBoolean BasicInfo = 1 << iota
	IsInteger
	IsUnsigned
	IsReal
	IsString
	IsChar
	IsWChar
	IsSet

	IsOrdered   = IsInteger | IsReal | IsString
	IsNumeric   = IsInteger | IsReal
	IsConstType = IsBoolean | IsNumeric | IsString | IsChar | IsWChar | IsSet
)

// A Basic represents a basic type.
type Basic struct {
	kind BasicKind
	info BasicInfo
	name string
}

func NewBasicType(kind BasicKind, info BasicInfo, name string) *Basic {
	return &Basic{kind: kind, info: info, name: name}
}

// Kind returns the kind of basic type b.
func (b *Basic) Kind() BasicKind { return b.kind }

// Info returns information about properties of basic type b.
func (b *Basic) Info() BasicInfo { return b.info }

// Name returns the name of basic type b.
func (b *Basic) Name() string { return b.name }

func (b *Basic) Underlying() Type { return b }
func (b *Basic) String() string   { return b.name }
func (b *Basic) Width() int {
	switch b.kind {
	case Byte, Int8, Bool:
		return 1
	case Int16, WChar:
		return 2
	case Int, Real, Set:
		return 4
	case Int64, LReal:
		return 8
	default:
		panic(fmt.Sprintf("'%s' is not a basic type", b.name))
	}
}
