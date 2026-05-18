package lower

import (
	"fmt"
	"math"
	"strconv"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/minir"
	"github.com/anthonyabeo/obx/src/sema/types"
)

// builtinLowering maps lowercase predeclared-function names to their inline
// lowering functions. It is populated by init() and consulted by lowerCallExpr
// and lowerCallStmt before falling through to a generic CallInst.
var builtinLowering map[string]func(*Lowerer, *desugar.FuncCall) minir.Value

func init() {
	builtinLowering = map[string]func(*Lowerer, *desugar.FuncCall) minir.Value{
		// predeclared functions
		"abs":     builtinAbs,
		"cap":     builtinCap,
		"bitand":  builtinBitAnd,
		"bitasr":  builtinBitASR,
		"bitor":   builtinBitOr,
		"bitxor":  builtinBitXor,
		"bitnot":  builtinBitNot,
		"bits":    builtinBits,
		"bitshl":  builtinBitSHL,
		"bitshr":  builtinBitSHR,
		"cast":    builtinCast,
		"chr":     builtinChr,
		"default": builtinDefault,
		"floor":   builtinFloor,
		"flt":     builtinFlt,
		"ldcmd":   builtinLdCmd,
		"ldmod":   builtinLdMod,
		"len":     builtinLen,
		"long":    builtinLong,
		"max":     builtinMax,
		"min":     builtinMin,
		"odd":     builtinOdd,
		"ord":     builtinOrd,
		"short":   builtinShort,
		"size":    builtinSize,
		"strlen":  builtinStrLen,
		"wchar":   builtinWChar,
		"ash":     builtinASH,
		"asr":     builtinASR,
		"entier":  builtinEntier,
		"lsl":     builtinLSL,
		"ror":     builtinROR,

		// predeclared procedures
		"assert": builtinAssert,
		"bytes":  builtinBytes,
		"dec":    builtinDec,
		"excl":   builtinExcl,
		"halt":   builtinHalt,
		"inc":    builtinInc,
		"incl":   builtinIncl,
		"new":    builtinNew,
		"number": builtinNumber,
		"pcall":  builtinPCall,
		"raise":  builtinRaise,
		"copy":   builtinCopy,
		"pack":   builtinPack,
		"unpk":   builtinUnpk,
	}
}

// ── helpers ───────────────────────────────────────────────────────────────────

// typeArgType extracts the minir.Type from a type-denotation argument (TypeRef).
// Returns nil when the argument is not a TypeRef.
func typeArgType(expr desugar.Expr) minir.Type {
	if tr, ok := expr.(*desugar.TypeRef); ok {
		return LowerType(tr.UnderType)
	}
	return nil
}

// isFloat reports whether the minir Type is a floating-point type.
func isFloat(ty minir.Type) bool { return ty == minir.F32() || ty == minir.F64() }

// nextSignedType returns the smallest signed integer type that is strictly
// wider than n bits.  Used when a signed and an unsigned type of equal width
// are combined: neither can safely represent the full range of the other, so
// we widen to the next signed type.
//
//	 8-bit  → i16
//	16-bit  → i32
//	32-bit  → i64
//	64-bit  → i64  (no wider signed type available; best effort)
func nextSignedType(bits int) minir.Type {
	switch {
	case bits <= 8:
		return minir.I16()
	case bits <= 16:
		return minir.I32()
	default:
		return minir.I64()
	}
}

// dominantIntType returns the promotion target for two integer types,
// implementing Oberon-style implicit integer promotion:
//
//  1. The type with the larger bit width wins.
//  2. For equal bit widths with the same signedness, either type is
//     returned unchanged (both signed → a; both unsigned → a).
//  3. For equal bit widths with mixed signedness (e.g. i8 vs u8), the value
//     of the unsigned type may not fit in the signed type (e.g. 255 > 127),
//     so we promote to the next larger signed type (i8+u8 → i16, i16+u16 →
//     i32, i32+u32 → i64).
//
// Returns nil when either argument is nil or not an integer type.
func dominantIntType(a, b minir.Type) minir.Type {
	wa, wb := minir.IntBitWidth(a), minir.IntBitWidth(b)
	if wa == 0 || wb == 0 {
		return nil
	}
	if wa > wb {
		return a
	}
	if wb > wa {
		return b
	}
	// Equal widths.
	aSign, bSign := minir.IsSignedIntType(a), minir.IsSignedIntType(b)
	if aSign == bSign {
		return a // same signedness — either will do, prefer a
	}
	// Mixed signedness at equal width: promote to next larger signed type.
	return nextSignedType(wa)
}

// coerceToType returns val re-typed to ty without emitting any CastInst.
//
//   - Temp operands:     shallow-copy with the new Ty annotation;
//     the original SSA definition is unmodified.
//   - Constant operands: compile-time fold via CoerceConst.
//
// *Temp is checked before Constant because *Temp satisfies the Constant
// interface (it carries a Name() method).  If the Constant arm ran first,
// CoerceConst would receive a *Temp, return nil, and coerceToType would fall
// through returning the value unchanged instead of retyping it.
//
// Use this for implicit integer promotion in binary operations.
// Use sameType for explicit language-level conversions (LONG, SHORT, CAST…).
func coerceToType(val minir.Value, ty minir.Type) minir.Value {
	if val == nil || ty == nil {
		return val
	}
	if vty := val.Type(); vty != nil && vty.Equal(ty) {
		return val
	}
	// *Temp must be checked first (see doc comment above).
	if t, ok := val.(*minir.Temp); ok {
		retyped := *t // shallow copy; same ID preserves SSA def-use chains
		retyped.Ty = minir.NormalizeType(ty)
		return &retyped
	}
	if c, ok := val.(minir.Constant); ok {
		if coerced := minir.CoerceConst(c, ty); coerced != nil {
			return coerced
		}
		return val // CoerceConst couldn't fold it; leave unchanged
	}
	return val
}

// castOp returns the correct CastInst opcode for src → dst.
func castOp(src, dst minir.Type) string {
	srcFloat := isFloat(src)
	dstFloat := isFloat(dst)
	switch {
	case srcFloat && dstFloat:
		if minir.BitWidthOf(dst) > minir.BitWidthOf(src) {
			return "fpext"
		}
		return "fptrunc"
	case !srcFloat && dstFloat:
		if minir.IsUnsignedType(src) {
			return "uitofp"
		}
		return "sitofp"
	case srcFloat && !dstFloat:
		if minir.IsUnsignedType(dst) {
			return "fptoui"
		}
		return "fptosi"
	default: // int → int
		if minir.BitWidthOf(dst) > minir.BitWidthOf(src) {
			if minir.IsUnsignedType(src) {
				return "zext"
			}
			return "sext"
		}
		return "trunc"
	}
}

// sameType returns val cast to ty, emitting the appropriate CastInst.
// Use only for explicit language-level conversions (LONG, SHORT, CAST, FLT…).
// For implicit operand alignment use coerceToType.
func (l *Lowerer) sameType(val minir.Value, ty minir.Type) minir.Value {
	if val == nil || ty == nil {
		return val
	}
	vty := val.Type()
	if vty != nil && vty.Equal(ty) {
		return val
	}
	// Constants are always folded at compile time even for explicit casts.
	if c, ok := val.(minir.Constant); ok {
		if coerced := minir.CoerceConst(c, ty); coerced != nil {
			return coerced
		}
	}
	dst := NewAnonTemp(ty)
	l.emit(&minir.CastInst{Dst: dst, Op: castOp(vty, ty), Src: val})
	return dst
}

// maxConst returns the maximum constant value for a sema BasicType.
func maxConst(semaType types.Type, ty minir.Type) minir.Value {
	bt, ok := semaType.(*types.BasicType)
	if !ok {
		return minir.NewConst("0", int64(0), ty)
	}
	switch bt.Kind {
	case types.BYTE:
		return minir.NewConst("255", int64(255), ty)
	case types.INT8:
		return minir.NewConst("127", int64(127), ty)
	case types.INT16, types.SHORTINT:
		return minir.NewConst("32767", int64(32767), ty)
	case types.INT32, types.INTEGER:
		return minir.NewConst("2147483647", int64(math.MaxInt32), ty)
	case types.INT64, types.LONGINT:
		return minir.NewConst("9223372036854775807", int64(math.MaxInt64), ty)
	case types.REAL:
		return minir.NewConst("3.4028235e+38", float64(math.MaxFloat32), ty)
	case types.LONGREAL:
		return minir.NewConst("1.7976931348623157e+308", float64(math.MaxFloat64), ty)
	case types.SET:
		return minir.NewConst("4294967295", int64(math.MaxUint32), ty)
	}
	return minir.NewConst("0", int64(0), ty)
}

// minConst returns the minimum constant value for a sema BasicType.
func minConst(semaType types.Type, ty minir.Type) minir.Value {
	bt, ok := semaType.(*types.BasicType)
	if !ok {
		return minir.NewConst("0", int64(0), ty)
	}
	switch bt.Kind {
	case types.BYTE:
		return minir.NewConst("0", int64(0), ty)
	case types.INT8:
		return minir.NewConst("-128", int64(-128), ty)
	case types.INT16, types.SHORTINT:
		return minir.NewConst("-32768", int64(-32768), ty)
	case types.INT32, types.INTEGER:
		return minir.NewConst("-2147483648", int64(math.MinInt32), ty)
	case types.INT64, types.LONGINT:
		return minir.NewConst("-9223372036854775808", int64(math.MinInt64), ty)
	case types.REAL:
		return minir.NewConst("-3.4028235e+38", float64(-math.MaxFloat32), ty)
	case types.LONGREAL:
		return minir.NewConst("-1.7976931348623157e+308", float64(-math.MaxFloat64), ty)
	case types.SET:
		return minir.NewConst("0", int64(0), ty)
	}
	return minir.NewConst("0", int64(0), ty)
}

// ── predeclared functions ─────────────────────────────────────────────────────

func builtinAbs(l *Lowerer, call *desugar.FuncCall) minir.Value {
	arg := l.lowerValue(call.Args[0])
	ty := arg.Type()
	if isFloat(ty) {
		return builtinAbsReal(l, arg)
	}
	return builtinAbsInt(l, arg)
}

func builtinAbsInt(l *Lowerer, arg minir.Value) minir.Value {
	ty := arg.Type()
	bits := int64(31)
	if ty == minir.I64() {
		bits = 63
	}
	shiftAmt := minir.NewConst(fmt.Sprintf("%d", bits), bits, ty)
	mask := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: mask, Op: "ashr", Left: arg, Right: shiftAmt})
	t1 := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: t1, Op: "xor", Left: arg, Right: mask})
	result := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: result, Op: "sub", Left: t1, Right: mask})
	return result
}

func builtinAbsReal(l *Lowerer, arg minir.Value) minir.Value {
	ty := arg.Type()
	zero := minir.NewConst("0.0", 0.0, ty)
	cmp := NewAnonTemp(minir.I1())
	l.emit(&minir.FCmpInst{ICmpInst: minir.ICmpInst{Dst: cmp, Pred: "lt", Left: arg, Right: zero}})

	negLabel := l.newLabel("abs.neg")
	posLabel := l.newLabel("abs.pos")
	endLabel := l.newLabel("abs.end")

	cbr := &minir.CondBrInst{Cond: cmp, TrueLabel: negLabel, FalseLabel: posLabel}
	l.emit(cbr)
	l.curBlock.Term = cbr

	negBlk := l.newBlock(negLabel)
	l.fn.Blocks[negBlk.ID] = negBlk
	l.switchTo(negBlk)
	negVal := NewAnonTemp(ty)
	l.emit(&minir.UnaryInst{Dst: negVal, Op: "fneg", Src: arg})
	negJmp := &minir.JumpInst{Target: endLabel}
	l.emit(negJmp)
	negBlk.Term = negJmp

	posBlk := l.newBlock(posLabel)
	l.fn.Blocks[posBlk.ID] = posBlk
	l.switchTo(posBlk)
	posJmp := &minir.JumpInst{Target: endLabel}
	l.emit(posJmp)
	posBlk.Term = posJmp

	endBlk := l.newBlock(endLabel)
	l.fn.Blocks[endBlk.ID] = endBlk
	l.switchTo(endBlk)

	result := NewAnonTemp(ty)
	l.emit(&minir.PhiInst{
		Dst: result,
		Args: []minir.PhiArm{
			{BlockLabel: negLabel, Val: negVal},
			{BlockLabel: posLabel, Val: arg},
		},
	})
	return result
}

func builtinCap(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	mask := minir.NewConst("223", int64(223), x.Type())
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "and", Left: x, Right: mask})
	return dst
}

func builtinBitAnd(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "and", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinBitASR(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "ashr", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinBitOr(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "or", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinBitXor(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "xor", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinBitNot(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.UnaryInst{Dst: dst, Op: "not", Src: x})
	return dst
}

func builtinBits(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(minir.I32())
	l.emit(&minir.CastInst{Dst: dst, Op: "bitcast", Src: x})
	return dst
}

func builtinBitSHL(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "shl", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinBitSHR(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "lshr", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinCast(l *Lowerer, call *desugar.FuncCall) minir.Value {
	targetTy := LowerType(call.Args[0].Type())
	if targetTy == nil {
		targetTy = minir.I32()
	}
	x := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(targetTy)
	l.emit(&minir.CastInst{Dst: dst, Op: "bitcast", Src: x})
	return dst
}

func builtinChr(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	mask := minir.NewConst("0xFF", int64(0xFF), x.Type())
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "and", Left: x, Right: mask})
	return dst
}

func builtinDefault(l *Lowerer, call *desugar.FuncCall) minir.Value {
	ty := LowerType(call.Args[0].Type())
	if ty == nil {
		ty = minir.I32()
	}
	if isFloat(ty) {
		return minir.NewConst("0.0", 0.0, ty)
	}
	return minir.NewConst("0", int64(0), ty)
}

func builtinFloor(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(minir.I64())
	l.emit(&minir.CallInst{Dst: dst, Callee: "__obx_floor", Args: []minir.Value{x}})
	return dst
}

func builtinFlt(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(minir.F32())
	l.emit(&minir.CastInst{Dst: dst, Op: "sitofp", Src: x})
	return dst
}

func builtinLdCmd(l *Lowerer, call *desugar.FuncCall) minir.Value {
	modVal := l.lowerValue(call.Args[0])
	nameVal := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(minir.Ptr(minir.I32()))
	l.emit(&minir.CallInst{Dst: dst, Callee: "__obx_ldcmd", Args: []minir.Value{modVal, nameVal}})
	return dst
}

func builtinLdMod(l *Lowerer, call *desugar.FuncCall) minir.Value {
	nameVal := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(minir.Ptr(minir.I32()))
	l.emit(&minir.CallInst{Dst: dst, Callee: "__obx_ldmod", Args: []minir.Value{nameVal}})
	return dst
}

func builtinLen(l *Lowerer, call *desugar.FuncCall) minir.Value {
	semaType := call.Args[0].Type()
	// Unwrap named types
	if nt, ok := semaType.(*types.NamedType); ok {
		semaType = nt.Def
	}
	dim := 0
	if len(call.Args) > 1 {
		if lit, ok := call.Args[1].(*desugar.Literal); ok {
			dim, _ = strconv.Atoi(lit.Value)
		}
	}

	if at, ok := semaType.(*types.ArrayType); ok {
		dims := at.Dimensions()
		if !at.IsOpen() && dim < len(dims) && dims[dim] >= 0 {
			// Fixed array: return the length as a constant
			return minir.NewConst(fmt.Sprintf("%d", dims[dim]), int64(dims[dim]), minir.I32())
		}
	}
	// Open / dynamic array: delegate to runtime
	addr := l.lowerAddr(call.Args[0])
	dst := NewAnonTemp(minir.I32())
	l.emit(&minir.CallInst{Dst: dst, Callee: "__obx_len", Args: []minir.Value{addr}})
	return dst
}

func builtinLong(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	ty := x.Type()
	if ty == minir.F32() {
		dst := NewAnonTemp(minir.F64())
		l.emit(&minir.CastInst{Dst: dst, Op: "fpext", Src: x})
		return dst
	}
	dst := NewAnonTemp(minir.I64())
	l.emit(&minir.CastInst{Dst: dst, Op: "sext", Src: x})
	return dst
}

func builtinMax(l *Lowerer, call *desugar.FuncCall) minir.Value {
	if len(call.Args) == 1 {
		// MAX(T) — type-denotation form
		semaType := call.Args[0].Type()
		ty := LowerType(semaType)
		if ty == nil {
			ty = minir.I32()
		}
		return maxConst(semaType, ty)
	}
	// MAX(x, y) — branchless two-arg form
	return builtinMaxTwo(l, call.Args[0], call.Args[1])
}

func builtinMaxTwo(l *Lowerer, xExpr, yExpr desugar.Expr) minir.Value {
	x := l.lowerValue(xExpr)
	y := l.lowerValue(yExpr)
	ty := x.Type()
	y = l.sameType(y, ty)
	bits := int64(31)
	if ty == minir.I64() {
		bits = 63
	}
	d := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: d, Op: "sub", Left: x, Right: y})
	m := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: m, Op: "ashr", Left: d, Right: minir.NewConst(fmt.Sprintf("%d", bits), bits, ty)})
	t := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: t, Op: "and", Left: m, Right: d})
	result := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: result, Op: "sub", Left: x, Right: t})
	return result
}

func builtinMin(l *Lowerer, call *desugar.FuncCall) minir.Value {
	if len(call.Args) == 1 {
		// MIN(T) — type-denotation form
		semaType := call.Args[0].Type()
		ty := LowerType(semaType)
		if ty == nil {
			ty = minir.I32()
		}
		return minConst(semaType, ty)
	}
	// MIN(x, y) — branchless two-arg form
	return builtinMinTwo(l, call.Args[0], call.Args[1])
}

func builtinMinTwo(l *Lowerer, xExpr, yExpr desugar.Expr) minir.Value {
	x := l.lowerValue(xExpr)
	y := l.lowerValue(yExpr)
	ty := x.Type()
	y = l.sameType(y, ty)
	bits := int64(31)
	if ty == minir.I64() {
		bits = 63
	}
	d := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: d, Op: "sub", Left: x, Right: y})
	m := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: m, Op: "ashr", Left: d, Right: minir.NewConst(fmt.Sprintf("%d", bits), bits, ty)})
	t := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: t, Op: "and", Left: m, Right: d})
	result := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: result, Op: "add", Left: y, Right: t})
	return result
}

func builtinOdd(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	one := minir.NewConst("1", int64(1), x.Type())
	anded := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: anded, Op: "and", Left: x, Right: one})
	zero := minir.NewConst("0", int64(0), x.Type())
	result := NewAnonTemp(minir.I1())
	l.emit(&minir.ICmpInst{Dst: result, Pred: "ne", Left: anded, Right: zero})
	return result
}

func builtinOrd(l *Lowerer, call *desugar.FuncCall) minir.Value {
	return l.lowerValue(call.Args[0])
}

func builtinShort(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	ty := x.Type()
	if ty == minir.F64() {
		dst := NewAnonTemp(minir.F32())
		l.emit(&minir.CastInst{Dst: dst, Op: "fptrunc", Src: x})
		return dst
	}
	// integer: truncate to i32
	dst := NewAnonTemp(minir.I32())
	l.emit(&minir.CastInst{Dst: dst, Op: "trunc", Src: x})
	return dst
}

func builtinSize(l *Lowerer, call *desugar.FuncCall) minir.Value {
	width := call.Args[0].Type().Width()
	return minir.NewConst(fmt.Sprintf("%d", width), int64(width), minir.I32())
}

func builtinStrLen(l *Lowerer, call *desugar.FuncCall) minir.Value {
	addr := l.lowerAddr(call.Args[0])
	dst := NewAnonTemp(minir.I32())
	l.emit(&minir.CallInst{Dst: dst, Callee: "__obx_strlen", Args: []minir.Value{addr}})
	return dst
}

func builtinWChar(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	mask := minir.NewConst("0xFFFF", int64(0xFFFF), x.Type())
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "and", Left: x, Right: mask})
	return dst
}

// ASH(x, n): arithmetic shift; left if n>0, right by |n| if n<0.
func builtinASH(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	n := l.lowerValue(call.Args[1])
	ty := x.Type()
	n = l.sameType(n, ty) // ensure shift-amount type matches

	zero := minir.NewConst("0", int64(0), ty)
	t0 := NewAnonTemp(minir.I1())
	l.emit(&minir.ICmpInst{Dst: t0, Pred: "sgt", Left: n, Right: zero})

	leftLabel := l.newLabel("ash.left")
	rightLabel := l.newLabel("ash.right")
	joinLabel := l.newLabel("ash.join")

	cbr := &minir.CondBrInst{Cond: t0, TrueLabel: leftLabel, FalseLabel: rightLabel}
	l.emit(cbr)
	l.curBlock.Term = cbr

	// left-shift block
	blkLeft := l.newBlock(leftLabel)
	l.fn.Blocks[blkLeft.ID] = blkLeft
	l.switchTo(blkLeft)
	leftResult := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: leftResult, Op: "shl", Left: x, Right: n})
	leftJmp := &minir.JumpInst{Target: joinLabel}
	l.emit(leftJmp)
	blkLeft.Term = leftJmp

	// right-shift block (n is negative; negate to get |n|)
	blkRight := l.newBlock(rightLabel)
	l.fn.Blocks[blkRight.ID] = blkRight
	l.switchTo(blkRight)
	absN := NewAnonTemp(ty)
	l.emit(&minir.UnaryInst{Dst: absN, Op: "neg", Src: n})
	rightResult := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: rightResult, Op: "ashr", Left: x, Right: absN})
	rightJmp := &minir.JumpInst{Target: joinLabel}
	l.emit(rightJmp)
	blkRight.Term = rightJmp

	// join block with phi
	blkJoin := l.newBlock(joinLabel)
	l.fn.Blocks[blkJoin.ID] = blkJoin
	l.switchTo(blkJoin)
	result := NewAnonTemp(ty)
	l.emit(&minir.PhiInst{
		Dst: result,
		Args: []minir.PhiArm{
			{BlockLabel: leftLabel, Val: leftResult},
			{BlockLabel: rightLabel, Val: rightResult},
		},
	})
	return result
}

func builtinASR(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	n := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "ashr", Left: x, Right: l.sameType(n, x.Type())})
	return dst
}

func builtinEntier(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(minir.I64())
	l.emit(&minir.CallInst{Dst: dst, Callee: "__obx_floor", Args: []minir.Value{x}})
	return dst
}

func builtinLSL(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	n := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&minir.BinaryInst{Dst: dst, Op: "shl", Left: x, Right: l.sameType(n, x.Type())})
	return dst
}

func builtinROR(l *Lowerer, call *desugar.FuncCall) minir.Value {
	x := l.lowerValue(call.Args[0])
	n := l.lowerValue(call.Args[1])
	ty := x.Type()
	n = l.sameType(n, ty)

	bitWidth := int64(32)
	if ty == minir.I64() {
		bitWidth = 64
	}
	bwConst := minir.NewConst(fmt.Sprintf("%d", bitWidth), bitWidth, ty)
	bwMinus1 := minir.NewConst(fmt.Sprintf("%d", bitWidth-1), bitWidth-1, ty)

	nMod := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: nMod, Op: "and", Left: n, Right: bwMinus1})
	right := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: right, Op: "lshr", Left: x, Right: nMod})
	wMinusN := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: wMinusN, Op: "sub", Left: bwConst, Right: nMod})
	left := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: left, Op: "shl", Left: x, Right: wMinusN})
	result := NewAnonTemp(ty)
	l.emit(&minir.BinaryInst{Dst: result, Op: "or", Left: left, Right: right})
	return result
}

// ── predeclared procedures ────────────────────────────────────────────────────

func builtinAssert(l *Lowerer, call *desugar.FuncCall) minir.Value {
	cond := l.lowerValue(call.Args[0])
	condTemp := l.ensureTemp(cond, minir.I1())

	var code minir.Value
	if len(call.Args) > 1 {
		code = l.lowerValue(call.Args[1])
	} else {
		code = minir.NewConst("1", int64(1), minir.I32())
	}
	codeTemp := l.ensureTemp(code, minir.I32())

	passLabel := l.newLabel("assert.pass")
	failLabel := l.newLabel("assert.fail")

	cbr := &minir.CondBrInst{Cond: condTemp, TrueLabel: passLabel, FalseLabel: failLabel}
	l.emit(cbr)
	l.curBlock.Term = cbr

	failBlk := l.newBlock(failLabel)
	l.fn.Blocks[failBlk.ID] = failBlk
	l.switchTo(failBlk)
	halt := &minir.HaltInst{Code: codeTemp}
	l.emit(halt)
	failBlk.Term = halt
	if l.fn != nil && l.fn.Exit != nil {
		failBlk.AddSucc(l.fn.Exit)
		l.fn.Exit.AddPred(failBlk)
	}

	passBlk := l.newBlock(passLabel)
	l.fn.Blocks[passBlk.ID] = passBlk
	l.switchTo(passBlk)
	return nil
}

func builtinBytes(l *Lowerer, call *desugar.FuncCall) minir.Value {
	dst := l.lowerAddr(call.Args[0])
	src := l.lowerAddr(call.Args[1])
	n := l.lowerValue(call.Args[2])
	l.emit(&minir.CallInst{Callee: "__obx_bytes", Args: []minir.Value{dst, src, n}})
	return nil
}

func builtinDec(l *Lowerer, call *desugar.FuncCall) minir.Value {
	addr := l.lowerAddr(call.Args[0])
	val := NewAnonTemp(LowerType(call.Args[0].Type()))
	if val.Ty == nil {
		val.Ty = minir.I32()
	}
	l.emit(&minir.LoadInst{Dst: val, Addr: addr})
	var amt minir.Value
	if len(call.Args) == 1 {
		amt = minir.NewConst("1", int64(1), val.Ty)
	} else {
		amt = l.sameType(l.lowerValue(call.Args[1]), val.Ty)
	}
	result := NewAnonTemp(val.Ty)
	l.emit(&minir.BinaryInst{Dst: result, Op: "sub", Left: val, Right: amt})
	l.emit(&minir.StoreInst{Val: result, Addr: addr})
	return nil
}

func builtinExcl(l *Lowerer, call *desugar.FuncCall) minir.Value {
	addr := l.lowerAddr(call.Args[0])
	v := NewAnonTemp(minir.I32())
	l.emit(&minir.LoadInst{Dst: v, Addr: addr})
	x := l.sameType(l.lowerValue(call.Args[1]), minir.I32())
	one := minir.NewConst("1", int64(1), minir.I32())
	bit := NewAnonTemp(minir.I32())
	l.emit(&minir.BinaryInst{Dst: bit, Op: "shl", Left: one, Right: x})
	mask := NewAnonTemp(minir.I32())
	l.emit(&minir.UnaryInst{Dst: mask, Op: "not", Src: bit})
	newV := NewAnonTemp(minir.I32())
	l.emit(&minir.BinaryInst{Dst: newV, Op: "and", Left: v, Right: mask})
	l.emit(&minir.StoreInst{Val: newV, Addr: addr})
	return nil
}

func builtinHalt(l *Lowerer, call *desugar.FuncCall) minir.Value {
	code := l.lowerValue(call.Args[0])
	codeTemp := l.ensureTemp(code, minir.I32())
	// Emit immediate Halt in-place.
	halt := &minir.HaltInst{Code: codeTemp}
	l.emit(halt)
	l.curBlock.Term = halt
	// Add an explicit CFG edge to the canonical exit so visualizations and
	// later passes can see that this block leads to the function epilogue.
	if l.fn != nil && l.fn.Exit != nil {
		l.curBlock.AddSucc(l.fn.Exit)
		l.fn.Exit.AddPred(l.curBlock)
	}
	// Switch to orphan dead block so subsequent code doesn't hit the terminated block.
	dead := l.newBlock(l.newLabel("dead"))
	l.switchTo(dead)
	return nil
}

func builtinInc(l *Lowerer, call *desugar.FuncCall) minir.Value {
	addr := l.lowerAddr(call.Args[0])
	val := NewAnonTemp(LowerType(call.Args[0].Type()))
	if val.Ty == nil {
		val.Ty = minir.I32()
	}
	l.emit(&minir.LoadInst{Dst: val, Addr: addr})
	var amt minir.Value
	if len(call.Args) == 1 {
		amt = minir.NewConst("1", int64(1), val.Ty)
	} else {
		amt = l.sameType(l.lowerValue(call.Args[1]), val.Ty)
	}
	result := NewAnonTemp(val.Ty)
	l.emit(&minir.BinaryInst{Dst: result, Op: "add", Left: val, Right: amt})
	l.emit(&minir.StoreInst{Val: result, Addr: addr})
	return nil
}

func builtinIncl(l *Lowerer, call *desugar.FuncCall) minir.Value {
	addr := l.lowerAddr(call.Args[0])
	v := NewAnonTemp(minir.I32())
	l.emit(&minir.LoadInst{Dst: v, Addr: addr})
	x := l.sameType(l.lowerValue(call.Args[1]), minir.I32())
	one := minir.NewConst("1", int64(1), minir.I32())
	bit := NewAnonTemp(minir.I32())
	l.emit(&minir.BinaryInst{Dst: bit, Op: "shl", Left: one, Right: x})
	newV := NewAnonTemp(minir.I32())
	l.emit(&minir.BinaryInst{Dst: newV, Op: "or", Left: v, Right: bit})
	l.emit(&minir.StoreInst{Val: newV, Addr: addr})
	return nil
}

func builtinNew(l *Lowerer, call *desugar.FuncCall) minir.Value {
	addr := l.lowerAddr(call.Args[0])

	var ptr *minir.Temp
	if len(call.Args) > 1 {
		// NEW(p, dim0, dim1, ...) — open-array allocation
		var dims []minir.Value
		for _, a := range call.Args[1:] {
			dims = append(dims, l.lowerValue(a))
		}
		ptr = NewAnonTemp(minir.Ptr(minir.I32()))
		l.emit(&minir.CallInst{Dst: ptr, Callee: "__obx_alloc_open", Args: dims})
	} else {
		// NEW(p) — fixed-size allocation
		width := call.Args[0].Type().Width()
		if width <= 0 {
			width = 4
		}
		sizeConst := minir.NewConst(fmt.Sprintf("%d", width), int64(width), minir.I32())
		ptr = NewAnonTemp(minir.Ptr(minir.I32()))
		l.emit(&minir.CallInst{Dst: ptr, Callee: "__obx_alloc", Args: []minir.Value{sizeConst}})
	}
	l.emit(&minir.StoreInst{Val: ptr, Addr: addr})
	return nil
}

func builtinNumber(l *Lowerer, call *desugar.FuncCall) minir.Value {
	dst := l.lowerAddr(call.Args[0])
	src := l.lowerAddr(call.Args[1])
	width := call.Args[0].Type().Width()
	if width <= 0 {
		width = 4
	}
	sizeConst := minir.NewConst(fmt.Sprintf("%d", width), int64(width), minir.I32())
	l.emit(&minir.CallInst{Callee: "__obx_bytes", Args: []minir.Value{dst, src, sizeConst}})
	return nil
}

func builtinPCall(l *Lowerer, call *desugar.FuncCall) minir.Value {
	callee := l.lowerValue(call.Args[0])
	var calleeName string
	switch v := callee.(type) {
	case *minir.Temp:
		calleeName = v.Name()
		if calleeName == "" {
			calleeName = fmt.Sprintf("%%t%d", v.ID)
		}
	case minir.Constant:
		calleeName = v.String()
	default:
		calleeName = callee.String()
	}
	var args []minir.Value
	for _, a := range call.Args[1:] {
		args = append(args, l.lowerValue(a))
	}
	l.emit(&minir.CallInst{Callee: calleeName, Args: args})
	return nil
}

func builtinRaise(l *Lowerer, call *desugar.FuncCall) minir.Value {
	return builtinHalt(l, call)
}

func builtinCopy(l *Lowerer, call *desugar.FuncCall) minir.Value {
	src := l.lowerAddr(call.Args[0])
	dst := l.lowerAddr(call.Args[1])
	l.emit(&minir.CallInst{Callee: "__obx_copy", Args: []minir.Value{src, dst}})
	return nil
}

func builtinPack(l *Lowerer, call *desugar.FuncCall) minir.Value {
	xAddr := l.lowerAddr(call.Args[0])
	xVal := NewAnonTemp(minir.F64())
	l.emit(&minir.LoadInst{Dst: xVal, Addr: xAddr})
	n := l.lowerValue(call.Args[1])
	result := NewAnonTemp(minir.F64())
	l.emit(&minir.CallInst{Dst: result, Callee: "__obx_pack", Args: []minir.Value{xVal, n}})
	l.emit(&minir.StoreInst{Val: result, Addr: xAddr})
	return nil
}

func builtinUnpk(l *Lowerer, call *desugar.FuncCall) minir.Value {
	xAddr := l.lowerAddr(call.Args[0])
	eAddr := l.lowerAddr(call.Args[1])
	xVal := NewAnonTemp(minir.F64())
	l.emit(&minir.LoadInst{Dst: xVal, Addr: xAddr})
	mantissa := NewAnonTemp(minir.F64())
	l.emit(&minir.CallInst{Dst: mantissa, Callee: "__obx_unpack_m", Args: []minir.Value{xVal}})
	exponent := NewAnonTemp(minir.I32())
	l.emit(&minir.CallInst{Dst: exponent, Callee: "__obx_unpack_e", Args: []minir.Value{xVal}})
	l.emit(&minir.StoreInst{Val: mantissa, Addr: xAddr})
	l.emit(&minir.StoreInst{Val: exponent, Addr: eAddr})
	return nil
}
