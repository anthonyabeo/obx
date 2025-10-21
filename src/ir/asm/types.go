package asm

import "fmt"

type Type interface {
	fmt.Stringer
	Size() int
}

type TypeKind int

const (
	invalid TypeKind = iota
	I8
	I16
	I32
	I64
	U8
	U16
	U32
	U64
	F16
	F32
	F64
	Ptr
)

type BasicType struct {
	Kind  TypeKind
	Width int
}

func (t BasicType) Size() int { return t.Width }
func (t BasicType) String() string {
	switch t.Kind {
	case I8:
		return "i8"
	case I16:
		return "i16"
	case I32:
		return "i32"
	case I64:
		return "i64"
	case U8:
		return "u8"
	case U16:
		return "u16"
	case U32:
		return "u32"
	case U64:
		return "u64"
	case F16:
		return "f16"
	case F32:
		return "f32"
	case F64:
		return "f64"
	case Ptr:
		return "ptr"
	default:
		return "invalid"
	}
}

type ArrayType struct {
	Element Type
	Width   int
}

func (a ArrayType) String() string {
	return fmt.Sprintf("[%d x %s]", a.Width, a.Element.String())
}

func (a ArrayType) Size() int { return a.Width }
