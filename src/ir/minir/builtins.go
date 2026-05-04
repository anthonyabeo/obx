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
func isFloat(ty Type) bool { return ty == primF32 || ty == primF64 }

// sameType returns val cast to ty if val.Type() != ty, else val unchanged.
// Emits a sext/trunc/fpext/fptrunc CastInst as needed.
func (l *Lowerer) sameType(val Value, ty Type) Value {
	if val.Type().Equal(ty) {
		return val
	}
	dst := NewAnonTemp(ty)
	var op string
	switch {
	case isFloat(ty) && !isFloat(val.Type()):
		op = "sitofp"
	case !isFloat(ty) && isFloat(val.Type()):
		op = "fptosi"
	case isFloat(ty) && isFloat(val.Type()):
		if ty == primF64 {
			op = "fpext"
		} else {
			op = "fptrunc"
		}
	default:
		if ty == primI64 {
			op = "sext"
		} else {
			op = "trunc"
		}
	}
	l.emit(&CastInst{Dst: dst, Op: op, Src: val})
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
	if ty == primI64 {
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
	cmp := NewAnonTemp(primI1)
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
	dst := NewAnonTemp(primI32)
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
		targetTy = primI32
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
		ty = primI32
	}
	if isFloat(ty) {
		return NewConst("0.0", 0.0, ty)
	}
	return NewConst("0", int64(0), ty)
}

func builtinFloor(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(primI64)
	l.emit(&CallInst{Dst: dst, Callee: "__obx_floor", Args: []Value{x}})
	return dst
}

func builtinFlt(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(primF32)
	l.emit(&CastInst{Dst: dst, Op: "sitofp", Src: x})
	return dst
}

func builtinLdCmd(l *Lowerer, call *desugar.FuncCall) Value {
	modVal := l.lowerValue(call.Args[0])
	nameVal := l.lowerValue(call.Args[1])
	dst := NewAnonTemp(Ptr(primI32))
	l.emit(&CallInst{Dst: dst, Callee: "__obx_ldcmd", Args: []Value{modVal, nameVal}})
	return dst
}

func builtinLdMod(l *Lowerer, call *desugar.FuncCall) Value {
	nameVal := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(Ptr(primI32))
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
			// Fixed array: return the length as a constant
			return NewConst(fmt.Sprintf("%d", dims[dim]), int64(dims[dim]), primI32)
		}
	}
	// Open / dynamic array: delegate to runtime
	addr := l.lowerAddr(call.Args[0])
	dst := NewAnonTemp(primI32)
	l.emit(&CallInst{Dst: dst, Callee: "__obx_len", Args: []Value{addr}})
	return dst
}

func builtinLong(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	ty := x.Type()
	if ty == primF32 {
		dst := NewAnonTemp(primF64)
		l.emit(&CastInst{Dst: dst, Op: "fpext", Src: x})
		return dst
	}
	dst := NewAnonTemp(primI64)
	l.emit(&CastInst{Dst: dst, Op: "sext", Src: x})
	return dst
}

func builtinMax(l *Lowerer, call *desugar.FuncCall) Value {
	if len(call.Args) == 1 {
		// MAX(T) — type-denotation form
		semaType := call.Args[0].Type()
		ty := LowerType(semaType)
		if ty == nil {
			ty = primI32
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
	if ty == primI64 {
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
			ty = primI32
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
	if ty == primI64 {
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
	result := NewAnonTemp(primI1)
	l.emit(&ICmpInst{Dst: result, Pred: "ne", Left: anded, Right: zero})
	return result
}

func builtinOrd(l *Lowerer, call *desugar.FuncCall) Value {
	return l.lowerValue(call.Args[0])
}

func builtinShort(l *Lowerer, call *desugar.FuncCall) Value {
	x := l.lowerValue(call.Args[0])
	ty := x.Type()
	if ty == primF64 {
		dst := NewAnonTemp(primF32)
		l.emit(&CastInst{Dst: dst, Op: "fptrunc", Src: x})
		return dst
	}
	// integer: truncate to i32
	dst := NewAnonTemp(primI32)
	l.emit(&CastInst{Dst: dst, Op: "trunc", Src: x})
	return dst
}

func builtinSize(l *Lowerer, call *desugar.FuncCall) Value {
	width := call.Args[0].Type().Width()
	return NewConst(fmt.Sprintf("%d", width), int64(width), primI32)
}

func builtinStrLen(l *Lowerer, call *desugar.FuncCall) Value {
	addr := l.lowerAddr(call.Args[0])
	dst := NewAnonTemp(primI32)
	l.emit(&CallInst{Dst: dst, Callee: "__obx_strlen", Args: []Value{addr}})
	return dst
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
	t0 := NewAnonTemp(primI1)
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
	x := l.lowerValue(call.Args[0])
	dst := NewAnonTemp(primI64)
	l.emit(&CallInst{Dst: dst, Callee: "__obx_floor", Args: []Value{x}})
	return dst
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
	if ty == primI64 {
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
	condTemp := l.ensureTemp(cond, primI1)

	var code Value
	if len(call.Args) > 1 {
		code = l.lowerValue(call.Args[1])
	} else {
		code = NewConst("1", int64(1), primI32)
	}
	codeTemp := l.ensureTemp(code, primI32)

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
	dst := l.lowerAddr(call.Args[0])
	src := l.lowerAddr(call.Args[1])
	n := l.lowerValue(call.Args[2])
	l.emit(&CallInst{Callee: "__obx_bytes", Args: []Value{dst, src, n}})
	return nil
}

func builtinDec(l *Lowerer, call *desugar.FuncCall) Value {
	addr := l.lowerAddr(call.Args[0])
	val := NewAnonTemp(LowerType(call.Args[0].Type()))
	if val.Ty == nil {
		val.Ty = primI32
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
	v := NewAnonTemp(primI32)
	l.emit(&LoadInst{Dst: v, Addr: addr})
	x := l.sameType(l.lowerValue(call.Args[1]), primI32)
	one := NewConst("1", int64(1), primI32)
	bit := NewAnonTemp(primI32)
	l.emit(&BinaryInst{Dst: bit, Op: "shl", Left: one, Right: x})
	mask := NewAnonTemp(primI32)
	l.emit(&UnaryInst{Dst: mask, Op: "not", Src: bit})
	newV := NewAnonTemp(primI32)
	l.emit(&BinaryInst{Dst: newV, Op: "and", Left: v, Right: mask})
	l.emit(&StoreInst{Val: newV, Addr: addr})
	return nil
}

func builtinHalt(l *Lowerer, call *desugar.FuncCall) Value {
	code := l.lowerValue(call.Args[0])
	codeTemp := l.ensureTemp(code, primI32)
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
		val.Ty = primI32
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
	v := NewAnonTemp(primI32)
	l.emit(&LoadInst{Dst: v, Addr: addr})
	x := l.sameType(l.lowerValue(call.Args[1]), primI32)
	one := NewConst("1", int64(1), primI32)
	bit := NewAnonTemp(primI32)
	l.emit(&BinaryInst{Dst: bit, Op: "shl", Left: one, Right: x})
	newV := NewAnonTemp(primI32)
	l.emit(&BinaryInst{Dst: newV, Op: "or", Left: v, Right: bit})
	l.emit(&StoreInst{Val: newV, Addr: addr})
	return nil
}

func builtinNew(l *Lowerer, call *desugar.FuncCall) Value {
	addr := l.lowerAddr(call.Args[0])

	var ptr *Temp
	if len(call.Args) > 1 {
		// NEW(p, dim0, dim1, ...) — open-array allocation
		var dims []Value
		for _, a := range call.Args[1:] {
			dims = append(dims, l.lowerValue(a))
		}
		ptr = NewAnonTemp(Ptr(primI32))
		l.emit(&CallInst{Dst: ptr, Callee: "__obx_alloc_open", Args: dims})
	} else {
		// NEW(p) — fixed-size allocation
		width := call.Args[0].Type().Width()
		if width <= 0 {
			width = 4
		}
		sizeConst := NewConst(fmt.Sprintf("%d", width), int64(width), primI32)
		ptr = NewAnonTemp(Ptr(primI32))
		l.emit(&CallInst{Dst: ptr, Callee: "__obx_alloc", Args: []Value{sizeConst}})
	}
	l.emit(&StoreInst{Val: ptr, Addr: addr})
	return nil
}

func builtinNumber(l *Lowerer, call *desugar.FuncCall) Value {
	dst := l.lowerAddr(call.Args[0])
	src := l.lowerAddr(call.Args[1])
	width := call.Args[0].Type().Width()
	if width <= 0 {
		width = 4
	}
	sizeConst := NewConst(fmt.Sprintf("%d", width), int64(width), primI32)
	l.emit(&CallInst{Callee: "__obx_bytes", Args: []Value{dst, src, sizeConst}})
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
	case *Constant:
		calleeName = fmt.Sprintf("%v", v.Val)
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
	src := l.lowerAddr(call.Args[0])
	dst := l.lowerAddr(call.Args[1])
	l.emit(&CallInst{Callee: "__obx_copy", Args: []Value{src, dst}})
	return nil
}

func builtinPack(l *Lowerer, call *desugar.FuncCall) Value {
	xAddr := l.lowerAddr(call.Args[0])
	xVal := NewAnonTemp(primF64)
	l.emit(&LoadInst{Dst: xVal, Addr: xAddr})
	n := l.lowerValue(call.Args[1])
	result := NewAnonTemp(primF64)
	l.emit(&CallInst{Dst: result, Callee: "__obx_pack", Args: []Value{xVal, n}})
	l.emit(&StoreInst{Val: result, Addr: xAddr})
	return nil
}

func builtinUnpk(l *Lowerer, call *desugar.FuncCall) Value {
	xAddr := l.lowerAddr(call.Args[0])
	eAddr := l.lowerAddr(call.Args[1])
	xVal := NewAnonTemp(primF64)
	l.emit(&LoadInst{Dst: xVal, Addr: xAddr})
	mantissa := NewAnonTemp(primF64)
	l.emit(&CallInst{Dst: mantissa, Callee: "__obx_unpack_m", Args: []Value{xVal}})
	exponent := NewAnonTemp(primI32)
	l.emit(&CallInst{Dst: exponent, Callee: "__obx_unpack_e", Args: []Value{xVal}})
	l.emit(&StoreInst{Val: mantissa, Addr: xAddr})
	l.emit(&StoreInst{Val: exponent, Addr: eAddr})
	return nil
}
