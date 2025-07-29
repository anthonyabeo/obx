package mir

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir"
	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
)

func (g *Generator) genExprs(exprs []hir.Expr) []Operand {
	var ops []Operand
	for _, e := range exprs {
		ops = append(ops, g.genOperand(e))
	}
	return ops
}

func (g *Generator) genBinaryOp(op token.Kind, lhs, rhs Operand) Operand {
	switch op {
	case token.PLUS:
		var oper ir.OpCode

		if isFloatType(lhs.Type()) && isFloatType(rhs.Type()) {
			oper = ir.FAdd
		} else {
			oper = ir.Add
		}

		t := g.newTemp(lhs.Type())
		g.emit(&AssignInst{Target: t, Value: &Binary{
			Op:    oper,
			Left:  lhs,
			Right: rhs,
			Typ:   lhs.Type(),
		}})

		return t
	case token.MINUS:
		var oper ir.OpCode
		if isFloatType(lhs.Type()) && isFloatType(rhs.Type()) {
			oper = ir.FSub
		} else {
			oper = ir.Sub
		}

		t := g.newTemp(lhs.Type())
		g.emit(&AssignInst{Target: t, Value: &Binary{
			Op:    oper,
			Left:  lhs,
			Right: rhs,
			Typ:   lhs.Type(),
		}})

		return t
	case token.STAR:
		var oper ir.OpCode
		if isFloatType(lhs.Type()) && isFloatType(rhs.Type()) {
			oper = ir.FMul
		} else {
			oper = ir.Mul
		}

		t := g.newTemp(lhs.Type())
		g.emit(&AssignInst{Target: t, Value: &Binary{
			Op:    oper,
			Left:  lhs,
			Right: rhs,
			Typ:   lhs.Type(),
		}})

		return t
	case token.DIV:
		var oper ir.OpCode

		if isFloatType(lhs.Type()) && isFloatType(rhs.Type()) {
			oper = ir.FDiv
		} else if isUInt(lhs.Type()) && isUInt(rhs.Type()) {
			oper = ir.UDiv
		} else {
			oper = ir.SDiv
		}

		t := g.newTemp(lhs.Type())
		g.emit(&AssignInst{Target: t, Value: &Binary{
			Op:    oper,
			Left:  lhs,
			Right: rhs,
			Typ:   lhs.Type(),
		}})

		return t
	case token.MOD:
		var oper ir.OpCode
		if isFloatType(lhs.Type()) && isFloatType(rhs.Type()) {
			oper = ir.FRem
		} else if isUInt(lhs.Type()) && isUInt(rhs.Type()) {
			oper = ir.URem
		} else {
			oper = ir.SRem
		}

		t := g.newTemp(lhs.Type())
		g.emit(&AssignInst{Target: t, Value: &Binary{
			Op:    oper,
			Left:  lhs,
			Right: rhs,
			Typ:   lhs.Type(),
		}})

		return t
	case token.AND:
		t := g.newTemp(Int1Type)
		g.emit(&AssignInst{Target: t, Value: &Binary{
			Op:    ir.And,
			Left:  lhs,
			Right: rhs,
			Typ:   Int1Type,
		}})

		return t
	case token.OR:
		t := g.newTemp(Int1Type)
		g.emit(&AssignInst{Target: t, Value: &Binary{
			Op:    ir.Or,
			Left:  lhs,
			Right: rhs,
			Typ:   Int1Type,
		}})

		return t
	case token.LESS:
		t := g.newTemp(Int1Type)
		g.emit(&AssignInst{
			Target: t,
			Value: &Binary{
				Op:    ir.ICmpLt,
				Left:  lhs,
				Right: rhs,
				Typ:   Int1Type,
			},
		})

		return t
	case token.GREAT:
		t := g.newTemp(Int1Type)
		g.emit(&AssignInst{
			Target: t,
			Value: &Binary{
				Op:    ir.ICmpGt,
				Left:  lhs,
				Right: rhs,
				Typ:   Int1Type,
			},
		})

		return t
	case token.EQUAL:
		t := g.newTemp(Int1Type)
		g.emit(&AssignInst{
			Target: t,
			Value: &Binary{
				Op:    ir.ICmpEq,
				Left:  lhs,
				Right: rhs,
				Typ:   Int1Type,
			},
		})

		return t
	case token.LEQ:
		t1 := g.newTemp(Int1Type)
		g.emit(&AssignInst{Target: t1, Value: &Binary{
			Op:    ir.ICmpLt,
			Left:  lhs,
			Right: rhs,
			Typ:   Int1Type,
		}})

		t2 := g.newTemp(Int1Type)
		g.emit(&AssignInst{Target: t2, Value: &Binary{
			Op:    ir.ICmpEq,
			Left:  lhs,
			Right: rhs,
			Typ:   Int1Type,
		}})

		t3 := g.newTemp(Int1Type)
		g.emit(&AssignInst{Target: t3, Value: &Binary{
			Op:    ir.Or,
			Left:  t1,
			Right: t2,
			Typ:   Int1Type,
		}})

		return t3
	case token.GEQ:
		t1 := g.newTemp(Int1Type)
		g.emit(&AssignInst{Target: t1, Value: &Binary{
			Op:    ir.ICmpGt,
			Left:  lhs,
			Right: rhs,
			Typ:   Int1Type,
		}})

		t2 := g.newTemp(Int1Type)
		g.emit(&AssignInst{Target: t2, Value: &Binary{
			Op:    ir.ICmpEq,
			Left:  lhs,
			Right: rhs,
			Typ:   Int1Type,
		}})

		t3 := g.newTemp(Int1Type)
		g.emit(&AssignInst{Target: t3, Value: &Binary{
			Op:    ir.Or,
			Left:  t1,
			Right: t2,
			Typ:   Int1Type,
		}})

		return t3
	case token.NEQ:
		t := g.newTemp(Int1Type)
		g.emit(&AssignInst{
			Target: t,
			Value: &Binary{
				Op:    ir.ICmpEq,
				Left:  lhs,
				Right: rhs,
				Typ:   Int1Type,
			},
		})

		t1 := g.newTemp(Int1Type)

		g.emit(&AssignInst{Target: t1, Value: &Binary{
			Op:    ir.Xor,
			Left:  True,
			Right: t,
			Typ:   Int1Type,
		}})

		return t1
		// TODO complete this
	default:
		panic(fmt.Sprintf("unknown operator type %v", op))
	}
}

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
		case types.INT16:
			return Int16Type
		case types.INT32:
			return Int32Type
		case types.INT64:
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

func isUInt(t Type) bool {
	ty, ok := t.(*IntegerType)
	return ok && !ty.Signed
}

func isSInt(t Type) bool {
	ty, ok := t.(*IntegerType)
	return ok && ty.Signed
}

func isFloat32(t Type) bool {
	f, ok := t.(*FloatType)
	return ok && f.Bits == 32
}

func isFloat64(t Type) bool {
	f, ok := t.(*FloatType)
	return ok && f.Bits == 64
}

func isFloatType(t Type) bool {
	return isFloatType(t) || isFloatType(t)
}

func isIntType(t Type) bool {
	return isUInt(t) || isSInt(t)
}

func isNumericType(t Type) bool {
	return isIntType(t) || isFloatType(t)
}
