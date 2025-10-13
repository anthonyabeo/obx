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
	case types.StringType:
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

func MirTypeToAsmType(ty Type) asm.Type {
	switch ty := ty.(type) {
	case *IntegerType:
		if ty.Signed {
			switch ty.Bits {
			case 64:
				return asm.I64
			case 32:
				return asm.I32
			case 16:
				return asm.I16
			case 8:
				return asm.I8
			default:
				panic("unsupported integer size")
			}
		} else {
			switch ty.Bits {
			case 64:
				return asm.U64
			case 32:
				return asm.U32
			case 16:
				return asm.U16
			case 8:
				return asm.U8
			default:
				panic("unsupported integer size")
			}
		}
	case *FloatType:
		switch ty.Bits {
		case 64:
			return asm.F64
		case 32:
			return asm.F32
		default:
			panic("unsupported float size")
		}
	default:
		panic("unsupported mir type to asm type conversion")
	}
}
