package asm

type Type uint8

const (
	invalid Type = iota
	i8
	i16
	i32
	i64
	u8
	u16
	u32
	u64
	f16
	f32
	f64
	ptr
)

func (t Type) String() string {
	switch t {
	case i8:
		return "i8"
	case i16:
		return "i16"
	case i32:
		return "i32"
	case i64:
		return "i64"
	case u8:
		return "u8"
	case u16:
		return "u16"
	case u32:
		return "u32"
	case u64:
		return "u64"
	case f16:
		return "f16"
	case f32:
		return "f32"
	case f64:
		return "f64"
	case ptr:
		return "ptr"
	default:
		return "invalid"
	}
}
