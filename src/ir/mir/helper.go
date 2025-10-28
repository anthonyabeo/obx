package mir

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/asm"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
)

func (g *Generator) genType(ty types.Type) Type {
	if ty == nil {
		return Void
	}

	switch ty := ty.(type) {
	case *types.BasicType:
		switch ty.Kind {
		case types.BYTE, types.CHAR:
			return UInt8Type
		case types.INT8:
			return Int8Type
		case types.INT16, types.SHORTINT:
			return Int16Type
		case types.INT32, types.INTEGER:
			return Int32Type
		case types.INT64, types.LONGINT:
			return Int64Type
		case types.REAL:
			return Float32Type
		case types.LONGREAL:
			return Float64Type
		case types.WCHAR:
			return UInt16Type
		case types.BOOLEAN:
			return Int1Type
		case types.SET:
			return UInt32Type
		}
	case *types.ArrayType:
		return &ArrayType{
			Len:  ty.Length,
			Elem: g.genType(ty.Elem),
		}
	case *types.StringType:
		return &StringType{Length: ty.Length}
	case types.EnumType:
	case *types.RecordType:
	case *types.ProcedureType:
	case *types.PointerType:
	case *types.NamedType:
	}

	return Void
}

func (g *Generator) genOp(op token.Kind) InstrOp {
	switch op {
	case token.PLUS:
		return ADD
	case token.MINUS:
		return SUB
	case token.STAR:
		return MUL
	case token.DIV:
		return DIV
	case token.MOD:
		return REM
	case token.NOT:
		return NOT
	case token.EQUAL:
		return EQ
	case token.NEQ:
		return NE
	case token.LESS:
		return LT
	case token.LEQ:
		return LE
	case token.GREAT:
		return GT
	case token.GEQ:
		return GE
	case token.OR:
		return OR

	default:
		panic("unknown operator " + op.String())
	}
}

func isMem(value Value) Value {
	if mem, isMem := value.(*Mem); isMem {
		return mem
	}

	if global, isGlobal := value.(*Global); isGlobal {
		return global
	}

	return nil
}

// ensure a value operand (Temp or Constant). If it's a Global/Mem/etc,
// insert a load and return a new Temp holding the value.
func (b *Builder) ensureValue(value Value) Value {
	switch v := value.(type) {
	case *Temp, *Local, *Param, Constant:
		return v
	case *Global, *Mem:
		t := b.NewTemp(v.Type())
		b.Emit(&LoadInst{Target: t, Addr: v})
		return t
	default:
		panic(fmt.Sprintf("unsupported operand type in ensureValue: %T", value))
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
