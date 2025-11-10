package mir

import (
	"github.com/anthonyabeo/obx/src/ir/asm"
)

// Helpers for creating IntegerLit constants of various sizes
func Int64Lit(val uint64) *IntegerLit {
	return &IntegerLit{
		LitValue: val,
		Signed:   true,
		Bits:     64,
		Typ:      Int64Type,
	}
}

func UInt64Lit(val uint64) *IntegerLit {
	return &IntegerLit{
		LitValue: val,
		Signed:   false,
		Bits:     64,
		Typ:      UInt64Type,
	}
}

func Int8Lit(val uint64) *IntegerLit {
	return &IntegerLit{
		LitValue: val,
		Signed:   true,
		Bits:     8,
		Typ:      Int8Type,
	}
}

func UInt8Lit(val uint64) *IntegerLit {
	return &IntegerLit{
		LitValue: val,
		Signed:   false,
		Bits:     8,
		Typ:      UInt8Type,
	}
}

func Int32Lit(val uint64) *IntegerLit {
	return &IntegerLit{
		LitValue: val,
		Signed:   true,
		Bits:     32,
		Typ:      Int32Type,
	}
}

func UInt32Lit(val uint64) *IntegerLit {
	return &IntegerLit{
		LitValue: val,
		Signed:   false,
		Bits:     32,
		Typ:      UInt32Type,
	}
}

func ToAsmType(ty Type) asm.Type {
	switch ty := ty.(type) {
	case *IntegerType:
		if ty.Signed {
			switch ty.Bits {
			case 64:
				return &asm.BasicType{Kind: asm.I64, Width: ty.Width()}
			case 32:
				return &asm.BasicType{Kind: asm.I32, Width: ty.Width()}
			case 16:
				return &asm.BasicType{Kind: asm.I16, Width: ty.Width()}
			case 8:
				return &asm.BasicType{Kind: asm.I8, Width: ty.Width()}
			default:
				panic("unsupported integer size")
			}
		} else {
			switch ty.Bits {
			case 64:
				return &asm.BasicType{Kind: asm.U64, Width: ty.Width()}
			case 32:
				return &asm.BasicType{Kind: asm.U32, Width: ty.Width()}
			case 16:
				return &asm.BasicType{Kind: asm.U16, Width: ty.Width()}
			case 8:
				return &asm.BasicType{Kind: asm.U8, Width: ty.Width()}
			default:
				panic("unsupported integer size")
			}
		}
	case *FloatType:
		switch ty.Bits {
		case 64:
			return &asm.BasicType{Kind: asm.F64, Width: ty.Width()}
		case 32:
			return &asm.BasicType{Kind: asm.F32, Width: ty.Width()}
		default:
			panic("unsupported float size")
		}
	case *ArrayType:
		return &asm.ArrayType{Element: ToAsmType(ty.Elem), Width: ty.Width()}
	case *VoidType:
		return nil
	case *StringType:
		return &asm.StringType{Width: ty.Width()}
	default:
		panic("unsupported mir type to asm type conversion")
	}
}
