package mir

import (
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
