package minir

import (
	"fmt"
	"math"
	"strconv"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/sema/types"
)

// builtinLowering maps lowercase predeclared-function names to their inline
// lowering functions. It is populated by init() and consulted by lowerCallExpr
// and lowerCallStmt before falling through to a generic CallInst.
var builtinLowering map[string]func(*Lowerer, *desugar.FuncCall) Value

func init() {
	builtinLowering = map[string]func(*Lowerer, *desugar.FuncCall) Value{
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
func typeArgType(expr desugar.Expr) Type {
	if tr, ok := expr.(*desugar.TypeRef); ok {
		return LowerType(tr.UnderType)
	}
	return nil
}

// isFloat reports whether the minir Type is a floating-point type.
func isFloat(ty Type) bool { return ty == F32() || ty == F64() }

// nextSignedType returns the smallest signed integer type that is strictly
// wider than n bits.  Used when a signed and an unsigned type of equal width
// are combined: neither can safely represent the full range of the other, so
// we widen to the next signed type.
//
//	 8-bit  → i16
//	16-bit  → i32
//	32-bit  → i64
//	64-bit  → i64  (no wider signed type available; best effort)
func nextSignedType(bits int) Type {
	switch {
	case bits <= 8:
		return I16()
	case bits <= 16:
		return I32()
	default:
		return I64()
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
func dominantIntType(a, b Type) Type {
	wa, wb := IntBitWidth(a), IntBitWidth(b)
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
	aSign, bSign := IsSignedIntType(a), IsSignedIntType(b)
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
func coerceToType(val Value, ty Type) Value {
	if val == nil || ty == nil {
		return val
	}
	if vty := val.Type(); vty != nil && vty.Equal(ty) {
		return val
	}
	// *Temp must be checked first (see doc comment above).
	if t, ok := val.(*Temp); ok {
		retyped := *t // shallow copy; same ID preserves SSA def-use chains
		retyped.Ty = NormalizeType(ty)
		return &retyped
	}
	if c, ok := val.(Constant); ok {
		if coerced := CoerceConst(c, ty); coerced != nil {
			return coerced
		}
		return val // CoerceConst couldn't fold it; leave unchanged
	}
	return val
}

// castOp returns the correct CastInst opcode for src → dst.
func castOp(src, dst Type) string {
	srcFloat := isFloat(src)
	dstFloat := isFloat(dst)
	switch {
	case srcFloat && dstFloat:
		if BitWidthOf(dst) > BitWidthOf(src) {
			return "fpext"
		}
		return "fptrunc"
	case !srcFloat && dstFloat:
		if IsUnsignedType(src) {
			return "uitofp"
		}
		return "sitofp"
	case srcFloat && !dstFloat:
		if IsUnsignedType(dst) {
			return "fptoui"
		}
		return "fptosi"
	default: // int → int
		if BitWidthOf(dst) > BitWidthOf(src) {
			if IsUnsignedType(src) {
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
func (l *Lowerer) sameType(val Value, ty Type) Value {
	if val == nil || ty == nil {
		return val
	}
	vty := val.Type()
	if vty != nil && vty.Equal(ty) {
		return val
	}
	// Constants are always folded at compile time even for explicit casts.
	if c, ok := val.(Constant); ok {
		if coerced := CoerceConst(c, ty); coerced != nil {
			return coerced
		}
	}
	dst := NewAnonTemp(ty)
	l.emit(&CastInst{Dst: dst, Op: castOp(vty, ty), Src: val})
	return dst
}

// maxConst returns the maximum constant value for a sema BasicType.
func maxConst(semaType types.Type, ty Type) Value {
	bt, ok := semaType.(*types.BasicType)
	if !ok {
		return NewConst("0", int64(0), ty)
	}
	switch bt.Kind {
	case types.BYTE:
		return NewConst("255", int64(255), ty)
	case types.INT8:
		return NewConst("127", int64(127), ty)
	case types.INT16, types.SHORTINT:
		return NewConst("32767", int64(32767), ty)
	case types.INT32, types.INTEGER:
		return NewConst("2147483647", int64(math.MaxInt32), ty)
	case types.INT64, types.LONGINT:
		return NewConst("9223372036854775807", int64(math.MaxInt64), ty)
	case types.REAL:
		return NewConst("3.4028235e+38", float64(math.MaxFloat32), ty)
	case types.LONGREAL:
		return NewConst("1.7976931348623157e+308", float64(math.MaxFloat64), ty)
	case types.SET:
		return NewConst("4294967295", int64(math.MaxUint32), ty)
	}
	return NewConst("0", int64(0), ty)
}

// minConst returns the minimum constant value for a sema BasicType.
func minConst(semaType types.Type, ty Type) Value {
	bt, ok := semaType.(*types.BasicType)
	if !ok {
		return NewConst("0", int64(0), ty)
	}
	switch bt.Kind {
	case types.BYTE:
		return NewConst("0", int64(0), ty)
	case types.INT8:
		return NewConst("-128", int64(-128), ty)
	case types.INT16, types.SHORTINT:
		return NewConst("-32768", int64(-32768), ty)
	case types.INT32, types.INTEGER:
		return NewConst("-2147483648", int64(math.MinInt32), ty)
	case types.INT64, types.LONGINT:
		return NewConst("-9223372036854775808", int64(math.MinInt64), ty)
	case types.REAL:
		return NewConst("-3.4028235e+38", float64(-math.MaxFloat32), ty)
	case types.LONGREAL:
		return NewConst("-1.7976931348623157e+308", float64(-math.MaxFloat64), ty)
	case types.SET:
		return NewConst("0", int64(0), ty)
	}
	return NewConst("0", int64(0), ty)
}

// ── predeclared functions ─────────────────────────────────────────────────────

func builtinAbs(l *Lowerer, call *desugar.FuncCall) Value {
	arg := l.lowerValue(call.Args[0])
	ty := arg.Type()
	if isFloat(ty) {
		return builtinAbsReal(l, arg)
	}
	return builtinAbsInt(l, arg)
}

func builtinAbsInt(l *Lowerer, arg Value) Value {
	ty := arg.Type()
	bits := int64(31)
	if ty == I64() {
		bits = 63
	}
	shiftAmt := NewConst(fmt.Sprintf("%d", bits), bits, ty)
	mask := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: mask, Op: "ashr", Left: arg, Right: shiftAmt})
	t1 := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: t1, Op: "xor", Left: arg, Right: mask})
	result := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: result, Op: "sub", Left: t1, Right: mask})
	return result
}

func builtinAbsReal(l *Lowerer, arg Value) Value {
	ty := arg.Type()
	zero := NewConst("0.0", 0.0, ty)
	cmp := NewAnonTemp(I1())
	l.emit(&FCmpInst{ICmpInst: ICmpInst{Dst: cmp, Pred: "lt", Left: arg, Right: zero}})

	negLabel := l.newLabel("abs.neg")
	posLabel := l.newLabel("abs.pos")
	endLabel := l.newLabel("abs.end")

	cbr := &CondBrInst{Cond: cmp, TrueLabel: negLabel, FalseLabel: posLabel}
	l.emit(cbr)
	l.curBlock.Term = cbr

	negBlk := l.newBlock(negLabel)
	l.fn.Blocks[negBlk.ID] = negBlk
	l.switchTo(negBlk)
	negVal := NewAnonTemp(ty)
	l.emit(&UnaryInst{Dst: negVal, Op: "fneg", Src: arg})
	negJmp := &JumpInst{Target: endLabel}
	l.emit(negJmp)
	negBlk.Term = negJmp

	posBlk := l.newBlock(posLabel)
	l.fn.Blocks[posBlk.ID] = posBlk
	l.switchTo(posBlk)
	posJmp := &JumpInst{Target: endLabel}
	l.emit(posJmp)
	posBlk.Term = posJmp

	endBlk := l.newBlock(endLabel)
	l.fn.Blocks[endBlk.ID] = endBlk
	l.switchTo(endBlk)

	result := NewAnonTemp(ty)
	l.emit(&PhiInst{
		Dst: result,
		Args: []PhiArm{
			{BlockLabel: negLabel, Val: negVal},
			{BlockLabel: posLabel, Val: arg},
		},
	})
	return result
}

func builtinCap(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	mask := NewConst("223", int64(223), x.Type())
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "and", Left: x, Right: mask})
	return dst
}

func builtinBitAnd(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "and", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinBitASR(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "ashr", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinBitOr(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "or", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinBitXor(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "xor", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinBitNot(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(x.Type())
	l.emit(&UnaryInst{Dst: dst, Op: "not", Src: x})
	return dst
}

func builtinBits(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(I32())
	l.emit(&CastInst{Dst: dst, Op: "bitcast", Src: x})
	return dst
}

func builtinBitSHL(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "shl", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinBitSHR(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	y := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "lshr", Left: x, Right: l.sameType(y, x.Type())})
	return dst
}

func builtinCast(l *Lowerer, call *desugar.FuncCall) Value {
	targetTy := LowerType(call.Args[0].Type())
	if targetTy == nil {
		targetTy = I32()
	}
	x := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(targetTy)
	l.emit(&CastInst{Dst: dst, Op: "bitcast", Src: x})
	return dst
}

func builtinChr(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	mask := NewConst("0xFF", int64(0xFF), x.Type())
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "and", Left: x, Right: mask})
	return dst
}

func builtinDefault(l *Lowerer, call *desugar.FuncCall) Value {
	ty := LowerType(call.Args[0].Type())
	if ty == nil {
		ty = I32()
	}
	if isFloat(ty) {
		return NewConst("0.0", 0.0, ty)
	}
	return NewConst("0", int64(0), ty)
}

// inlineFloor emits the floor operation inline without a runtime call.
//
// Strategy:
//  1. Constant argument: fold via math.Floor at compile time.
//  2. Runtime argument:
//     trunc  = fptosi x          ; truncate toward zero
//     truncF = sitofp trunc      ; convert back to float for comparison
//     cmp    = fcmp lt x, truncF ; true when x < trunc (negative non-integer)
//     if cmp → result = trunc - 1
//     else   → result = trunc
//
// dstTy must be I32() or I64().
func inlineFloor(l *Lowerer, x Value, dstTy Type) Value {
	// ── compile-time constant fold ──────────────────────────────────────────
	if fc, ok := x.(*FloatConst); ok {
		floored := math.Floor(fc.Value)
		v := int64(floored)
		return NewConst(fmt.Sprintf("%d", v), v, dstTy)
	}

	srcTy := x.Type() // f32 or f64

	// ── runtime: truncate-toward-zero then adjust for negative non-integers ─
	trunc := NewAnonTemp(dstTy)
	l.emit(&CastInst{Dst: trunc, Op: "fptosi", Src: x})

	// Convert back to float so we can compare with the original.
	truncF := NewAnonTemp(srcTy)
	l.emit(&CastInst{Dst: truncF, Op: "sitofp", Src: trunc})

	// x < truncF  ⟺  fptosi went the wrong direction (x was negative + fractional).
	cmp := NewAnonTemp(I1())
	l.emit(&FCmpInst{ICmpInst: ICmpInst{Dst: cmp, Pred: "lt", Left: x, Right: truncF}})

	adjLabel := l.newLabel("floor.adj")
	noAdjLabel := l.newLabel("floor.noadj")
	doneLabel := l.newLabel("floor.done")

	cbr := &CondBrInst{Cond: cmp, TrueLabel: adjLabel, FalseLabel: noAdjLabel}
	l.emit(cbr)
	l.curBlock.Term = cbr

	// Adjustment block: trunc - 1
	adjBlk := l.newBlock(adjLabel)
	l.fn.Blocks[adjBlk.ID] = adjBlk
	l.switchTo(adjBlk)
	one := NewConst("1", int64(1), dstTy)
	adjVal := NewAnonTemp(dstTy)
	l.emit(&BinaryInst{Dst: adjVal, Op: "sub", Left: trunc, Right: one})
	adjJmp := &JumpInst{Target: doneLabel}
	l.emit(adjJmp)
	adjBlk.Term = adjJmp

	// No-adjustment block: pass trunc through unchanged.
	noAdjBlk := l.newBlock(noAdjLabel)
	l.fn.Blocks[noAdjBlk.ID] = noAdjBlk
	l.switchTo(noAdjBlk)
	noAdjJmp := &JumpInst{Target: doneLabel}
	l.emit(noAdjJmp)
	noAdjBlk.Term = noAdjJmp

	// Done: select result via phi.
	doneBlk := l.newBlock(doneLabel)
	l.fn.Blocks[doneBlk.ID] = doneBlk
	l.switchTo(doneBlk)
	result := NewAnonTemp(dstTy)
	l.emit(&PhiInst{
		Dst: result,
		Args: []PhiArm{
			{BlockLabel: adjLabel, Val: adjVal},
			{BlockLabel: noAdjLabel, Val: trunc},
		},
	})
	return result
}

func builtinFloor(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	// REAL (f32) → INT32;  LONGREAL (f64) → INT64  (Oberon+ spec).
	dstTy := I32()
	if x.Type() == F64() {
		dstTy = I64()
	}
	return inlineFloor(l, x, dstTy)
}

func builtinFlt(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(F32())
	l.emit(&CastInst{Dst: dst, Op: "sitofp", Src: x})
	return dst
}

func builtinLdCmd(l *Lowerer, call *desugar.FuncCall) Value {
	modVal := l.lowerValue(call.Args[0])
	nameVal := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(Ptr(I32()))
	l.emit(&CallInst{Dst: dst, Callee: "__obx_ldcmd", Args: []Value{modVal, nameVal}})
	return dst
}

func builtinLdMod(l *Lowerer, call *desugar.FuncCall) Value {
	nameVal := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(Ptr(I32()))
	l.emit(&CallInst{Dst: dst, Callee: "__obx_ldmod", Args: []Value{nameVal}})
	return dst
}

func builtinLen(l *Lowerer, call *desugar.FuncCall) Value {
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
			// Fixed array: return the length as a constant.
			return NewConst(fmt.Sprintf("%d", dims[dim]), int64(dims[dim]), I32())
		}
		// Open array: load dimension slot [dim] from the dope-vector header.
		// Memory layout (per-allocation):
		//   [dim0 : dimType] [dim1 : dimType] ... [dimN-1 : dimType] [data ...]
		// where dimType is i32 (32-bit target) or i64 (64-bit target).
		dt := l.dimType()
		base := l.lowerAddr(call.Args[0])

		// Re-type the base pointer as ptr(dimType) so the GEP arithmetic uses
		// word-sized strides into the header.
		dimBase := l.newAddrTemp("", dt)
		l.emit(&CastInst{Dst: dimBase, Op: "bitcast", Src: base})

		// GEP to the requested dimension slot.
		slotAddr := l.newAddrTemp("", dt)
		l.emit(&GEPInst{Dst: slotAddr, Base: dimBase, ElemType: dt, Offsets: []int{dim}})

		// Load the dimension value.
		loaded := NewAnonTemp(dt)
		l.emit(&LoadInst{Dst: loaded, Addr: slotAddr})

		// Oberon LEN always returns INTEGER (i32); truncate on 64-bit targets.
		if dt == I64() {
			r32 := NewAnonTemp(I32())
			l.emit(&CastInst{Dst: r32, Op: "trunc", Src: loaded})
			return r32
		}
		return loaded
	}
	// TODO emit diagnostic for non-array type — should not occur after sema, but guard gracefully.
	// Non-array type — should not occur after sema, but guard gracefully.
	return NewConst("0", int64(0), I32())
}

func builtinLong(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	ty := x.Type()
	if ty == F32() {
		dst := NewAnonTemp(F64())
		l.emit(&CastInst{Dst: dst, Op: "fpext", Src: x})
		return dst
	}
	dst := NewAnonTemp(I64())
	l.emit(&CastInst{Dst: dst, Op: "sext", Src: x})
	return dst
}

func builtinMax(l *Lowerer, call *desugar.FuncCall) Value {
	if len(call.Args) == 1 {
		// MAX(T) — type-denotation form
		semaType := call.Args[0].Type()
		ty := LowerType(semaType)
		if ty == nil {
			ty = I32()
		}
		return maxConst(semaType, ty)
	}
	// MAX(x, y) — branchless two-arg form
	return builtinMaxTwo(l, call.Args[0], call.Args[1])
}

func builtinMaxTwo(l *Lowerer, xExpr, yExpr desugar.Expr) Value {
	x := l.lowerValue(xExpr)
	y := l.lowerValue(yExpr)
	ty := x.Type()
	y = l.sameType(y, ty)
	bits := int64(31)
	if ty == I64() {
		bits = 63
	}
	d := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: d, Op: "sub", Left: x, Right: y})
	m := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: m, Op: "ashr", Left: d, Right: NewConst(fmt.Sprintf("%d", bits), bits, ty)})
	t := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: t, Op: "and", Left: m, Right: d})
	result := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: result, Op: "sub", Left: x, Right: t})
	return result
}

func builtinMin(l *Lowerer, call *desugar.FuncCall) Value {
	if len(call.Args) == 1 {
		// MIN(T) — type-denotation form
		semaType := call.Args[0].Type()
		ty := LowerType(semaType)
		if ty == nil {
			ty = I32()
		}
		return minConst(semaType, ty)
	}
	// MIN(x, y) — branchless two-arg form
	return builtinMinTwo(l, call.Args[0], call.Args[1])
}

func builtinMinTwo(l *Lowerer, xExpr, yExpr desugar.Expr) Value {
	x := l.lowerValue(xExpr)
	y := l.lowerValue(yExpr)
	ty := x.Type()
	y = l.sameType(y, ty)
	bits := int64(31)
	if ty == I64() {
		bits = 63
	}
	d := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: d, Op: "sub", Left: x, Right: y})
	m := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: m, Op: "ashr", Left: d, Right: NewConst(fmt.Sprintf("%d", bits), bits, ty)})
	t := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: t, Op: "and", Left: m, Right: d})
	result := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: result, Op: "add", Left: y, Right: t})
	return result
}

func builtinOdd(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	one := NewConst("1", int64(1), x.Type())
	anded := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: anded, Op: "and", Left: x, Right: one})
	zero := NewConst("0", int64(0), x.Type())
	result := NewAnonTemp(I1())
	l.emit(&ICmpInst{Dst: result, Pred: "ne", Left: anded, Right: zero})
	return result
}

func builtinOrd(l *Lowerer, call *desugar.FuncCall) Value {
	return l.lowerValue(call.Args[0])
}

func builtinShort(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	ty := x.Type()
	if ty == F64() {
		dst := NewAnonTemp(F32())
		l.emit(&CastInst{Dst: dst, Op: "fptrunc", Src: x})
		return dst
	}
	// integer: truncate to i32
	dst := NewAnonTemp(I32())
	l.emit(&CastInst{Dst: dst, Op: "trunc", Src: x})
	return dst
}

func builtinSize(l *Lowerer, call *desugar.FuncCall) Value {
	width := call.Args[0].Type().Width()
	return NewConst(fmt.Sprintf("%d", width), int64(width), I32())
}

func builtinStrLen(l *Lowerer, call *desugar.FuncCall) Value {
	// STRLEN(s): dynamic length of the string excluding the terminating 0X
	// (Oberon+ spec).  Delegates to the C standard-library strlen, which is
	// named "strlen" on every supported OS (POSIX libc and Windows msvcrt).
	//
	// LEN(array_of_char) returns the allocated *capacity*, which may be larger
	// than the actual string content.  STRLEN must therefore scan at runtime
	// regardless of whether the array is open or fixed-size.
	//
	// strlen returns size_t; we truncate to i32 (Oberon INTEGER) on 64-bit
	// targets since Oberon array lengths are always INTEGER-ranged.
	//
	// The caller is responsible for null termination; the semantic analyser
	// enforces that the argument is ARRAY OF CHAR / WCHAR or a string literal.
	addr := l.lowerAddr(call.Args[0])
	dt := l.dimType()
	raw := NewAnonTemp(dt)
	l.emit(&CallInst{Dst: raw, Callee: l.strlenFuncName(), Args: []Value{addr}})
	if dt == I64() {
		dst := NewAnonTemp(I32())
		l.emit(&CastInst{Dst: dst, Op: "trunc", Src: raw})
		return dst
	}
	return raw
}

func builtinWChar(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	mask := NewConst("0xFFFF", int64(0xFFFF), x.Type())
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "and", Left: x, Right: mask})
	return dst
}

// ASH(x, n): arithmetic shift; left if n>0, right by |n| if n<0.
func builtinASH(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	n := l.lowerValue(call.Args[1])
	ty := x.Type()
	n = l.sameType(n, ty) // ensure shift-amount type matches

	zero := NewConst("0", int64(0), ty)
	t0 := NewAnonTemp(I1())
	l.emit(&ICmpInst{Dst: t0, Pred: "sgt", Left: n, Right: zero})

	leftLabel := l.newLabel("ash.left")
	rightLabel := l.newLabel("ash.right")
	joinLabel := l.newLabel("ash.join")

	cbr := &CondBrInst{Cond: t0, TrueLabel: leftLabel, FalseLabel: rightLabel}
	l.emit(cbr)
	l.curBlock.Term = cbr

	// left-shift block
	blkLeft := l.newBlock(leftLabel)
	l.fn.Blocks[blkLeft.ID] = blkLeft
	l.switchTo(blkLeft)
	leftResult := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: leftResult, Op: "shl", Left: x, Right: n})
	leftJmp := &JumpInst{Target: joinLabel}
	l.emit(leftJmp)
	blkLeft.Term = leftJmp

	// right-shift block (n is negative; negate to get |n|)
	blkRight := l.newBlock(rightLabel)
	l.fn.Blocks[blkRight.ID] = blkRight
	l.switchTo(blkRight)
	absN := NewAnonTemp(ty)
	l.emit(&UnaryInst{Dst: absN, Op: "neg", Src: n})
	rightResult := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: rightResult, Op: "ashr", Left: x, Right: absN})
	rightJmp := &JumpInst{Target: joinLabel}
	l.emit(rightJmp)
	blkRight.Term = rightJmp

	// join block with phi
	blkJoin := l.newBlock(joinLabel)
	l.fn.Blocks[blkJoin.ID] = blkJoin
	l.switchTo(blkJoin)
	result := NewAnonTemp(ty)
	l.emit(&PhiInst{
		Dst: result,
		Args: []PhiArm{
			{BlockLabel: leftLabel, Val: leftResult},
			{BlockLabel: rightLabel, Val: rightResult},
		},
	})
	return result
}

func builtinASR(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	n := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "ashr", Left: x, Right: l.sameType(n, x.Type())})
	return dst
}

func builtinEntier(l *Lowerer, call *desugar.FuncCall) Value {
	// ENTIER is the classic Oberon name for floor; always returns INT64.
	x := l.lowerValue(call.Args[0])
	return inlineFloor(l, x, I64())
}

func builtinLSL(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	n := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(x.Type())
	l.emit(&BinaryInst{Dst: dst, Op: "shl", Left: x, Right: l.sameType(n, x.Type())})
	return dst
}

func builtinROR(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	n := l.lowerValue(call.Args[1])
	ty := x.Type()
	n = l.sameType(n, ty)

	bitWidth := int64(32)
	if ty == I64() {
		bitWidth = 64
	}
	bwConst := NewConst(fmt.Sprintf("%d", bitWidth), bitWidth, ty)
	bwMinus1 := NewConst(fmt.Sprintf("%d", bitWidth-1), bitWidth-1, ty)

	nMod := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: nMod, Op: "and", Left: n, Right: bwMinus1})
	right := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: right, Op: "lshr", Left: x, Right: nMod})
	wMinusN := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: wMinusN, Op: "sub", Left: bwConst, Right: nMod})
	left := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: left, Op: "shl", Left: x, Right: wMinusN})
	result := NewAnonTemp(ty)
	l.emit(&BinaryInst{Dst: result, Op: "or", Left: left, Right: right})
	return result
}

// ── predeclared procedures ────────────────────────────────────────────────────

func builtinAssert(l *Lowerer, call *desugar.FuncCall) Value {
	cond := l.lowerValue(call.Args[0])
	condTemp := l.ensureTemp(cond, I1())

	var code Value
	if len(call.Args) > 1 {
		code = l.lowerValue(call.Args[1])
	} else {
		code = NewConst("1", int64(1), I32())
	}
	codeTemp := l.ensureTemp(code, I32())

	passLabel := l.newLabel("assert.pass")
	failLabel := l.newLabel("assert.fail")

	cbr := &CondBrInst{Cond: condTemp, TrueLabel: passLabel, FalseLabel: failLabel}
	l.emit(cbr)
	l.curBlock.Term = cbr

	failBlk := l.newBlock(failLabel)
	l.fn.Blocks[failBlk.ID] = failBlk
	l.switchTo(failBlk)
	halt := &HaltInst{Code: codeTemp}
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

func builtinBytes(l *Lowerer, call *desugar.FuncCall) Value {
	// BYTES(a, n): store the raw memory representation of n into array a.
	// The desugar layer passes three arguments: (a, n, SIZE(n)) where the
	// third is the byte count already computed at the call site.
	//
	// Spec requirement: if LEN(a) < SIZE(n) the program halts.
	//
	// Implementation:
	//   1. Bounds check (compile-time when a is a fixed array, runtime otherwise).
	//   2. memcpy(dst=&a[0], src=&n, count=SIZE(n))
	dst := l.lowerAddr(call.Args[0])
	src := l.lowerAddr(call.Args[1])

	// copySize is SIZE(n) — already computed by the desugar layer as args[2].
	copySize := l.lowerValue(call.Args[2])
	dt := l.dimType()
	copySizeDt := l.sameType(copySize, dt)

	// ── bounds check ────────────────────────────────────────────────────────
	// Determine LEN(a): compile-time constant for fixed arrays, dope-vector
	// load for open arrays.
	var aLen Value
	semaType := call.Args[0].Type()
	if nt, ok := semaType.(*types.NamedType); ok {
		semaType = nt.Def
	}
	if at, ok := semaType.(*types.ArrayType); ok && !at.IsOpen() {
		dims := at.Dimensions()
		if len(dims) > 0 && dims[0] >= 0 {
			aLen = NewConst(fmt.Sprintf("%d", dims[0]), int64(dims[0]), dt)
		}
	}
	if aLen == nil {
		// Open array: load LEN from dope-vector slot 0.
		dimBase := l.newAddrTemp("", dt)
		l.emit(&CastInst{Dst: dimBase, Op: "bitcast", Src: dst})
		slotAddr := l.newAddrTemp("", dt)
		l.emit(&GEPInst{Dst: slotAddr, Base: dimBase, ElemType: dt, Offsets: []int{0}})
		loaded := NewAnonTemp(dt)
		l.emit(&LoadInst{Dst: loaded, Addr: slotAddr})
		aLen = loaded
	}
	aLenDt := l.sameType(aLen, dt)

	// Compile-time fold: if both values are constants we can decide statically.
	aLenConst, aLenIsConst := aLen.(Constant)
	copySizeConst, copyIsConst := copySize.(Constant)
	if aLenIsConst && copyIsConst {
		aLenV, _ := AsInt64(aLenConst)
		copyV, _ := AsInt64(copySizeConst)
		if aLenV < copyV {
			// Unconditional halt — spec says program halts.
			haltCode := NewConst("1", int64(1), I32())
			haltTemp := l.ensureTemp(haltCode, I32())
			halt := &HaltInst{Code: haltTemp}
			l.emit(halt)
			l.curBlock.Term = halt
			if l.fn != nil && l.fn.Exit != nil {
				l.curBlock.AddSucc(l.fn.Exit)
				l.fn.Exit.AddPred(l.curBlock)
			}
			dead := l.newBlock(l.newLabel("dead"))
			l.switchTo(dead)
			return nil
		}
		// aLen >= copySize: bounds check always passes — fall through to memcpy.
	} else {
		// Runtime bounds check: halt if aLen < copySize.
		cmp := NewAnonTemp(I1())
		l.emit(&ICmpInst{Dst: cmp, Pred: "lt", Left: aLenDt, Right: copySizeDt})

		haltLabel := l.newLabel("bytes.halt")
		okLabel := l.newLabel("bytes.ok")

		cbr := &CondBrInst{Cond: cmp, TrueLabel: haltLabel, FalseLabel: okLabel}
		l.emit(cbr)
		l.curBlock.Term = cbr

		haltBlk := l.newBlock(haltLabel)
		l.fn.Blocks[haltBlk.ID] = haltBlk
		l.switchTo(haltBlk)
		haltCode := l.ensureTemp(NewConst("1", int64(1), I32()), I32())
		halt := &HaltInst{Code: haltCode}
		l.emit(halt)
		haltBlk.Term = halt
		if l.fn != nil && l.fn.Exit != nil {
			haltBlk.AddSucc(l.fn.Exit)
			l.fn.Exit.AddPred(haltBlk)
		}

		okBlk := l.newBlock(okLabel)
		l.fn.Blocks[okBlk.ID] = okBlk
		l.switchTo(okBlk)
	}

	// ── memcpy ──────────────────────────────────────────────────────────────
	l.emit(&CallInst{Callee: l.memcpyFuncName(), Args: []Value{dst, src, copySizeDt}})
	return nil
}

func builtinDec(l *Lowerer, call *desugar.FuncCall) Value {
	addr := l.lowerAddr(call.Args[0])
	val := NewAnonTemp(LowerType(call.Args[0].Type()))
	if val.Ty == nil {
		val.Ty = I32()
	}
	l.emit(&LoadInst{Dst: val, Addr: addr})
	var amt Value
	if len(call.Args) == 1 {
		amt = NewConst("1", int64(1), val.Ty)
	} else {
		amt = l.sameType(l.lowerValue(call.Args[1]), val.Ty)
	}
	result := NewAnonTemp(val.Ty)
	l.emit(&BinaryInst{Dst: result, Op: "sub", Left: val, Right: amt})
	l.emit(&StoreInst{Val: result, Addr: addr})
	return nil
}

func builtinExcl(l *Lowerer, call *desugar.FuncCall) Value {
	addr := l.lowerAddr(call.Args[0])
	v := NewAnonTemp(I32())
	l.emit(&LoadInst{Dst: v, Addr: addr})
	x := l.sameType(l.lowerValue(call.Args[1]), I32())
	one := NewConst("1", int64(1), I32())
	bit := NewAnonTemp(I32())
	l.emit(&BinaryInst{Dst: bit, Op: "shl", Left: one, Right: x})
	mask := NewAnonTemp(I32())
	l.emit(&UnaryInst{Dst: mask, Op: "not", Src: bit})
	newV := NewAnonTemp(I32())
	l.emit(&BinaryInst{Dst: newV, Op: "and", Left: v, Right: mask})
	l.emit(&StoreInst{Val: newV, Addr: addr})
	return nil
}

func builtinHalt(l *Lowerer, call *desugar.FuncCall) Value {
	code := l.lowerValue(call.Args[0])
	codeTemp := l.ensureTemp(code, I32())
	// Emit immediate Halt in-place.
	halt := &HaltInst{Code: codeTemp}
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

func builtinInc(l *Lowerer, call *desugar.FuncCall) Value {
	addr := l.lowerAddr(call.Args[0])
	val := NewAnonTemp(LowerType(call.Args[0].Type()))
	if val.Ty == nil {
		val.Ty = I32()
	}
	l.emit(&LoadInst{Dst: val, Addr: addr})
	var amt Value
	if len(call.Args) == 1 {
		amt = NewConst("1", int64(1), val.Ty)
	} else {
		amt = l.sameType(l.lowerValue(call.Args[1]), val.Ty)
	}
	result := NewAnonTemp(val.Ty)
	l.emit(&BinaryInst{Dst: result, Op: "add", Left: val, Right: amt})
	l.emit(&StoreInst{Val: result, Addr: addr})
	return nil
}

func builtinIncl(l *Lowerer, call *desugar.FuncCall) Value {
	addr := l.lowerAddr(call.Args[0])
	v := NewAnonTemp(U32())
	l.emit(&LoadInst{Dst: v, Addr: addr})
	x := l.sameType(l.lowerValue(call.Args[1]), U32())
	one := NewConst("1", int64(1), U32())
	bit := NewAnonTemp(U32())
	l.emit(&BinaryInst{Dst: bit, Op: "shl", Left: one, Right: x})
	newV := NewAnonTemp(U32())
	l.emit(&BinaryInst{Dst: newV, Op: "or", Left: v, Right: bit})
	l.emit(&StoreInst{Val: newV, Addr: addr})
	return nil
}

func builtinNew(l *Lowerer, call *desugar.FuncCall) Value {
	addr := l.lowerAddr(call.Args[0])

	var ptr *Temp
	if len(call.Args) > 1 {
		// NEW(v, x0, ..., xn) — v is POINTER TO open array.
		//
		// Allocates a single contiguous block with the dope-vector header
		// followed immediately by the data:
		//
		//   [x0 : dimType] [x1 : dimType] ... [xN-1 : dimType]
		//   [elem0] [elem1] ... [elem(x0*x1*...*xN-1 - 1)]
		//
		// totalSize = N*wordSize + x0*x1*...*xN-1 * sizeof(elem)
		//
		// After allocation each xi is stored into its header slot so that
		// LEN(v, i) can read it back with a single load.
		dims := make([]Value, 0, len(call.Args)-1)
		for _, a := range call.Args[1:] {
			dims = append(dims, l.lowerValue(a))
		}
		ndims := len(dims)
		dt := l.dimType()
		ws := l.wordSize()

		// Determine the element size by drilling through the pointer and
		// open-array chain in the sema types down to the innermost element.
		semaType := call.Args[0].Type()
		if nt, ok := semaType.(*types.NamedType); ok {
			semaType = nt.Def
		}
		if pt, ok := semaType.(*types.PointerType); ok {
			semaType = pt.Base
		}
		if nt, ok := semaType.(*types.NamedType); ok {
			semaType = nt.Def
		}
		// Walk past all open-array wrappers to the concrete element type.
		for {
			at, ok := semaType.(*types.ArrayType)
			if !ok || !at.IsOpen() {
				break
			}
			semaType = at.Elem
		}
		elemWidth := int64(semaType.Width())
		if elemWidth <= 0 {
			elemWidth = ws
		}

		// Compute data size: prod(xi) * elemWidth using dimType arithmetic.
		prod := l.sameType(dims[0], dt)
		for _, d := range dims[1:] {
			tmp := NewAnonTemp(dt)
			l.emit(&BinaryInst{Dst: tmp, Op: "mul", Left: prod, Right: l.sameType(d, dt)})
			prod = tmp
		}
		elemWidthConst := NewConst(fmt.Sprintf("%d", elemWidth), elemWidth, dt)
		dataBytes := NewAnonTemp(dt)
		l.emit(&BinaryInst{Dst: dataBytes, Op: "mul", Left: prod, Right: elemWidthConst})

		// Add the header size: ndims * wordSize.
		headerBytes := int64(ndims) * ws
		headerConst := NewConst(fmt.Sprintf("%d", headerBytes), headerBytes, dt)
		totalSize := NewAnonTemp(dt)
		l.emit(&BinaryInst{Dst: totalSize, Op: "add", Left: dataBytes, Right: headerConst})

		// Allocate the block via the target-OS allocator.
		ptr = NewAnonTemp(Ptr(I32()))
		l.emit(&CallInst{Dst: ptr, Callee: l.allocFuncName(), Args: []Value{totalSize}})

		// Write each dimension xi into slot i of the header.
		dimBase := l.newAddrTemp("", dt)
		l.emit(&CastInst{Dst: dimBase, Op: "bitcast", Src: ptr})
		for i, d := range dims {
			slotAddr := l.newAddrTemp("", dt)
			l.emit(&GEPInst{Dst: slotAddr, Base: dimBase, ElemType: dt, Offsets: []int{i}})
			l.emit(&StoreInst{Val: l.sameType(d, dt), Addr: slotAddr})
		}
	} else {
		// NEW(v) — v is POINTER TO RECORD or fixed-size array.
		// Allocate exactly sizeof(*v) bytes; no dope-vector header needed.
		semaType := call.Args[0].Type()
		if nt, ok := semaType.(*types.NamedType); ok {
			semaType = nt.Def
		}
		if pt, ok := semaType.(*types.PointerType); ok {
			semaType = pt.Base
		}
		width := semaType.Width()
		if width <= 0 {
			width = int(l.wordSize())
		}
		sizeConst := NewConst(fmt.Sprintf("%d", width), int64(width), l.dimType())
		ptr = NewAnonTemp(Ptr(I32()))
		l.emit(&CallInst{Dst: ptr, Callee: l.allocFuncName(), Args: []Value{sizeConst}})
	}

	l.emit(&StoreInst{Val: ptr, Addr: addr})
	return nil
}

func builtinNumber(l *Lowerer, call *desugar.FuncCall) Value {
	// NUMBER(v, a): reinterpret the raw bytes of array a as the type of v.
	// Copies exactly SIZE(v) bytes from &a[0] into &v via memcpy.
	// No bounds check: the semantic analyser guarantees SIZE(a) >= SIZE(v).
	dst := l.lowerAddr(call.Args[0])
	src := l.lowerAddr(call.Args[1])
	width := call.Args[0].Type().Width()
	if width <= 0 {
		width = int(l.wordSize())
	}
	dt := l.dimType()
	sizeConst := NewConst(fmt.Sprintf("%d", width), int64(width), dt)
	l.emit(&CallInst{Callee: l.memcpyFuncName(), Args: []Value{dst, src, sizeConst}})
	return nil
}

func builtinPCall(l *Lowerer, call *desugar.FuncCall) Value {
	callee := l.lowerValue(call.Args[0])
	var calleeName string
	switch v := callee.(type) {
	case *Temp:
		calleeName = v.Name()
		if calleeName == "" {
			calleeName = fmt.Sprintf("%%t%d", v.ID)
		}
	case Constant:
		calleeName = v.String()
	default:
		calleeName = callee.String()
	}
	var args []Value
	for _, a := range call.Args[1:] {
		args = append(args, l.lowerValue(a))
	}
	l.emit(&CallInst{Callee: calleeName, Args: args})
	return nil
}

func builtinRaise(l *Lowerer, call *desugar.FuncCall) Value {
	return builtinHalt(l, call)
}

func builtinCopy(l *Lowerer, call *desugar.FuncCall) Value {
	// COPY(x, v): v := x — copies the CHAR array / string x into v.
	// Implemented as memcpy(dataDst, dataSrc, copySize) where:
	//   • Fixed arrays: data pointer = base address, copySize = SIZE(v).
	//   • Open arrays:  data pointer = base + NDims*wordSize (skip dope-vector
	//     header); copySize = LEN(v) * sizeof(elem).
	src := l.lowerAddr(call.Args[0]) // &x
	dst := l.lowerAddr(call.Args[1]) // &v

	dt := l.dimType()

	var dataSrc Value = src
	var dataDst Value = dst
	var copySize Value

	// ── destination size / data pointer ────────────────────────────────────
	dstSema := call.Args[1].Type()
	if nt, ok := dstSema.(*types.NamedType); ok {
		dstSema = nt.Def
	}
	if at, ok := dstSema.(*types.ArrayType); ok && at.IsOpen() {
		// Open ARRAY OF CHAR: skip the NDims-word dope-vector header.
		ndims, innerSema := countOpenDims(at)
		elemWidth := int64(innerSema.Width())
		if elemWidth <= 0 {
			elemWidth = 1 // CHAR is always 1 byte
		}
		// Data pointer: GEP past NDims dim-type slots.
		dataStartDst := l.newAddrTemp("", l.dimType())
		l.emit(&GEPInst{Dst: dataStartDst, Base: dst, ElemType: dt, Offsets: []int{ndims}})
		dataDst = dataStartDst

		// Copy size: LEN(v) * sizeof(elem).
		dimBase := l.newAddrTemp("", dt)
		l.emit(&CastInst{Dst: dimBase, Op: "bitcast", Src: dst})
		slotAddr := l.newAddrTemp("", dt)
		l.emit(&GEPInst{Dst: slotAddr, Base: dimBase, ElemType: dt, Offsets: []int{0}})
		lenV := NewAnonTemp(dt)
		l.emit(&LoadInst{Dst: lenV, Addr: slotAddr})
		if elemWidth != 1 {
			ewConst := NewConst(fmt.Sprintf("%d", elemWidth), elemWidth, dt)
			cs := NewAnonTemp(dt)
			l.emit(&BinaryInst{Dst: cs, Op: "mul", Left: lenV, Right: ewConst})
			copySize = cs
		} else {
			copySize = lenV
		}
	} else {
		// Fixed array: copySize = WIDTH(v).
		width := dstSema.Width()
		if width <= 0 {
			width = int(l.wordSize())
		}
		copySize = NewConst(fmt.Sprintf("%d", width), int64(width), dt)
	}

	// ── source data pointer (skip header when x is also open) ──────────────
	srcSema := call.Args[0].Type()
	if nt, ok := srcSema.(*types.NamedType); ok {
		srcSema = nt.Def
	}
	if srcAt, ok := srcSema.(*types.ArrayType); ok && srcAt.IsOpen() {
		ndims, _ := countOpenDims(srcAt)
		dataStartSrc := l.newAddrTemp("", l.dimType())
		l.emit(&GEPInst{Dst: dataStartSrc, Base: src, ElemType: dt, Offsets: []int{ndims}})
		dataSrc = dataStartSrc
	}

	l.emit(&CallInst{Callee: l.memcpyFuncName(), Args: []Value{dataDst, dataSrc, copySize}})
	return nil
}

func builtinPack(l *Lowerer, call *desugar.FuncCall) Value {
	// PACK(x, n): x := x * 2^n.
	// Delegates to C scalbn(x, n) which implements exactly this operation.
	// scalbn is in libm (POSIX) / msvcrt (Windows) under the same name.
	xAddr := l.lowerAddr(call.Args[0])
	xVal := NewAnonTemp(F64())
	l.emit(&LoadInst{Dst: xVal, Addr: xAddr})
	n := l.sameType(l.lowerValue(call.Args[1]), I32())

	packed := NewAnonTemp(F64())
	l.emit(&CallInst{Dst: packed, Callee: l.scalbnFuncName(), Args: []Value{xVal, n}})
	l.emit(&StoreInst{Val: packed, Addr: xAddr})
	return nil
}

func builtinUnpk(l *Lowerer, call *desugar.FuncCall) Value {
	// UNPK(x, n): normalise x to mantissa ∈ [1.0, 2.0) and store the binary
	// exponent in n, such that  x_in = mantissa * 2^n.
	//
	// Uses C frexp(x, &exp) which returns mantissa ∈ [0.5, 1.0) and exp such
	// that  x = mantissa * 2^exp.  Oberon's [1.0, 2.0) range is obtained by:
	//   oberon_mantissa = frexp_mantissa * 2.0
	//   oberon_exp      = frexp_exp - 1
	//
	// frexp is in libm (POSIX) / msvcrt (Windows) under the same name.
	xAddr := l.lowerAddr(call.Args[0])
	nAddr := l.lowerAddr(call.Args[1])

	xVal := NewAnonTemp(F64())
	l.emit(&LoadInst{Dst: xVal, Addr: xAddr})

	// Allocate a stack slot for frexp's int* output parameter.
	expSlot := l.newAddrTemp("", I32())
	l.emit(&AllocaInst{Dst: expSlot, AllocType: I32()})

	// Call frexp(x, &expSlot) → raw mantissa ∈ [0.5, 1.0).
	rawMantissa := NewAnonTemp(F64())
	l.emit(&CallInst{Dst: rawMantissa, Callee: l.frexpFuncName(), Args: []Value{xVal, expSlot}})

	// Load the raw exponent.
	rawExp := NewAnonTemp(I32())
	l.emit(&LoadInst{Dst: rawExp, Addr: expSlot})

	// Adjust to Oberon range: mantissa *= 2, exp -= 1.
	two := NewConst("2.0", 2.0, F64())
	obMantissa := NewAnonTemp(F64())
	l.emit(&BinaryInst{Dst: obMantissa, Op: "fmul", Left: rawMantissa, Right: two})

	one := NewConst("1", int64(1), I32())
	obExp := NewAnonTemp(I32())
	l.emit(&BinaryInst{Dst: obExp, Op: "sub", Left: rawExp, Right: one})

	// Write results back.
	l.emit(&StoreInst{Val: obMantissa, Addr: xAddr})
	l.emit(&StoreInst{Val: obExp, Addr: nAddr})
	return nil
}
