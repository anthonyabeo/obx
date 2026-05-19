package miniropt

import (
	"math"
	"reflect"

	"github.com/anthonyabeo/obx/src/ir/minir"
)

// ── shared helpers used across fold.go, simplify.go, and strength.go ─────────

func applyScalarValue(
	ins minir.Instr,
	def *minir.Temp,
	v minir.Value,
	subst map[*minir.Temp]minir.Value,
	toDelete map[minir.Instr]bool,
) bool {
	if def == nil || v == nil {
		return false
	}

	subst[def] = v
	toDelete[ins] = true
	return true
}

func sameInstr(a, b minir.Instr) bool {
	ba, ok := a.(*minir.BinaryInst)
	if !ok {
		return a == b
	}
	bb, ok := b.(*minir.BinaryInst)
	if !ok {
		return false
	}
	return ba.Op == bb.Op && ba.Dst == bb.Dst && sameValue(ba.Left, bb.Left) && sameValue(ba.Right, bb.Right)
}

func sameValue(a, b minir.Value) bool {
	switch x := a.(type) {
	case *minir.Temp:
		y, ok := b.(*minir.Temp)
		return ok && x == y
	case *minir.IntegerConst:
		y, ok := b.(*minir.IntegerConst)
		return ok && x.Signed == y.Signed && x.BitWidth == y.BitWidth && unsignedIntValue(x) == unsignedIntValue(y) && (x.Type() == nil || y.Type() == nil || x.Type().Equal(y.Type()))
	case *minir.FloatConst:
		y, ok := b.(*minir.FloatConst)
		return ok && math.Float64bits(x.Value) == math.Float64bits(y.Value) && (x.Type() == nil || y.Type() == nil || x.Type().Equal(y.Type()))
	case *minir.StringConst:
		y, ok := b.(*minir.StringConst)
		return ok && x.Value == y.Value && (x.Type() == nil || y.Type() == nil || x.Type().Equal(y.Type()))
	case *minir.NilConst:
		_, ok := b.(*minir.NilConst)
		return ok
	case *minir.AggregateConst:
		y, ok := b.(*minir.AggregateConst)
		return ok && reflect.DeepEqual(x.Val, y.Val) && (x.Type() == nil || y.Type() == nil || x.Type().Equal(y.Type()))
	default:
		return a == b
	}
}

func isZeroValue(v minir.Value) bool {
	switch c := v.(type) {
	case *minir.IntegerConst:
		return unsignedIntValue(c) == 0
	case *minir.FloatConst:
		return c.Value == 0
	}
	return false
}

func isOneValue(v minir.Value) bool {
	switch c := v.(type) {
	case *minir.IntegerConst:
		return unsignedIntValue(c) == 1
	case *minir.FloatConst:
		return c.Value == 1
	}
	return false
}

func isAllOnesValue(v minir.Value) bool {
	c, ok := v.(*minir.IntegerConst)
	if !ok {
		return false
	}
	w := c.BitWidth
	if w <= 0 || w >= 64 {
		return unsignedIntValue(c) == ^uint64(0)
	}
	mask := (uint64(1) << uint(w)) - 1
	return unsignedIntValue(c) == mask
}

func constBool(v bool) minir.Value { return minir.ConstBool("", v) }

func zeroValueForType(ty minir.Type) minir.Value {
	switch {
	case ty == nil:
		return minir.ConstInt("", 0, minir.I32())
	case isBoolType(ty):
		return minir.ConstBool("", false)
	case minir.IsUnsignedType(ty):
		return minir.ConstUint("", 0, ty)
	case minir.IsIntType(ty):
		return minir.ConstInt("", 0, ty)
	case ty.Equal(minir.F32()):
		return minir.ConstFloat32("", 0)
	case ty.Equal(minir.F64()):
		return minir.ConstFloat64("", 0)
	default:
		return minir.ConstInt("", 0, minir.I32())
	}
}

func constIntLike(ty minir.Type, bits uint64) minir.Value {
	if ty == nil {
		ty = minir.I32()
	}
	if isUnsignedType(ty) {
		return minir.ConstUint("", bits, ty)
	}
	if isBoolType(ty) {
		return minir.ConstBool("", bits&1 == 1)
	}
	return minir.ConstInt("", signExtend(bits, minir.IntBitWidth(ty)), ty)
}

func constFloatLike(ty minir.Type, v float64) minir.Value {
	if ty != nil && ty.Equal(minir.F32()) {
		return minir.ConstFloat32("", float32(v))
	}
	return minir.ConstFloat64("", v)
}

func unsignedIntValue(c *minir.IntegerConst) uint64 {
	if c == nil {
		return 0
	}
	return wrapToWidth(c.Value, c.BitWidth)
}

func signedIntValue(c *minir.IntegerConst) int64 {
	if c == nil {
		return 0
	}
	return signExtend(unsignedIntValue(c), c.BitWidth)
}

func wrapToWidth(v uint64, width int) uint64 {
	if width <= 0 || width >= 64 {
		return v
	}
	mask := (uint64(1) << uint(width)) - 1
	return v & mask
}

func signExtend(v uint64, width int) int64 {
	if width <= 0 || width >= 64 {
		return int64(v)
	}
	mask := (uint64(1) << uint(width)) - 1
	v &= mask
	if v&(uint64(1)<<uint(width-1)) != 0 {
		return int64(v | ^mask)
	}
	return int64(v)
}

func isBoolType(ty minir.Type) bool { return minir.IntBitWidth(ty) == 1 }

func isUnsignedType(ty minir.Type) bool { return minir.IsUnsignedType(ty) }

func valueType(dst *minir.Temp, v minir.Value) minir.Type {
	if dst != nil && dst.Type() != nil {
		return dst.Type()
	}
	if v != nil {
		return v.Type()
	}
	return nil
}
