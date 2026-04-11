package obxir

import (
	"fmt"
	"math"
	"strconv"

	"github.com/anthonyabeo/obx/src/ir/desugar"
)

var builtinLowering map[string]func(*IRBuilder, *Function, *desugar.FuncCall) Value

func init() {
	builtinLowering = map[string]func(*IRBuilder, *Function, *desugar.FuncCall) Value{
		"printf": lowerPrintfBuiltin,

		// predeclared functions
		"abs":     lowerAbsBuiltin,
		"cap":     lowerCapBuiltin,
		"bitand":  lowerBitAndBuiltin,
		"bitasr":  lowerBitASRBuiltin,
		"bitor":   lowerBitOrBuiltin,
		"bitxor":  lowerBitXorBuiltin,
		"bitnot":  lowerBitNotBuiltin,
		"bits":    lowerBitsBuiltin,
		"bitshl":  lowerBitSHLBuiltin,
		"bitshr":  lowerBitSHRBuiltin,
		"cast":    lowerCastBuiltin,
		"chr":     lowerCHRBuiltin,
		"default": lowerDefaultBuiltin,
		"floor":   lowerFloorBuiltin,
		"flt":     lowerFLTBuiltin,
		"ldcmd":   lowerLDCMDBuiltin,
		"ldmod":   lowerLDMODBuiltin,
		"len":     lowerLenBuiltin,
		"long":    lowerLongBuiltin,
		"max":     lowerMaxBuiltin,
		"min":     lowerMinBuiltin,
		"odd":     lowerOddBuiltin,
		"ord":     lowerOrdBuiltin,
		"short":   lowerShortBuiltin,
		"size":    lowerSizeBuiltin,
		"strlen":  lowerStrLenBuiltin,
		"wchar":   lowerWCHARBuiltin,
		"ash":     lowerASHBuiltin,
		"asr":     lowerASRBuiltin,
		"entier":  lowerENTIERBuiltin,
		"lsl":     lowerLSLBuiltin,
		"ror":     lowerRORBuiltin,

		// predeclared procedures
		"assert": lowerAssertBuiltin,
		"bytes":  lowerBytesBuiltin,
		"dec":    lowerDecBuiltin,
		"excl":   lowerExclBuiltin,
		"halt":   lowerHaltBuiltin,
		"inc":    lowerIncBuiltin,
		"incl":   lowerInclBuiltin,
		"new":    lowerNewBuiltin,
		"number": lowerNumberBuiltin,
		"pcall":  lowerPcallBuiltin,
		"raise":  lowerRaiseBuiltin,
		"copy":   lowerCopyBuiltin,
		"pack":   lowerPackBuiltin,
		"unpk":   lowerUnpackBuiltin,
	}
}

// ─── Predeclared procedures ───────────────────────────────────────────────

func lowerAssertBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	cond := b.ensureValue(call.Args[0])
	var code Value
	if len(call.Args) > 1 {
		code = b.ensureValue(call.Args[1])
	} else {
		code = UInt64Lit(1)
	}

	blkPass := b.NewBlock(b.NewLabel("assert.pass"))
	blkFail := b.NewBlock(b.NewLabel("assert.fail"))
	b.SetTerm(&CondBrInst{Cond: cond, TrueLabel: blkPass.Label, FalseLabel: blkFail.Label})

	b.SetBlock(blkFail)
	b.SetTerm(&HaltInst{Code: code})

	b.SetBlock(blkPass)
	return code
}

func lowerBytesBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	dst := b.ensureAddr(call.Args[0])
	src := b.ensureAddr(call.Args[1])
	n := b.ensureValue(call.Args[2])
	b.Emit(&CallInst{Callee: "__obx_bytes", Args: []Value{dst, src, n}})
	return nil
}

func lowerDecBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	addr := b.ensureAddr(call.Args[0])
	x := b.ensureValue(call.Args[0])
	var amt Value
	if len(call.Args) == 1 {
		amt = Int64Lit(1)
	} else {
		amt = b.ensureValue(call.Args[1])
	}
	res := b.NewTemp(x.Type())
	b.Emit(&BinaryInst{Target: res, Op: SUB, Left: x, Right: amt})
	b.emitAssign(addr, res)
	return res
}

func lowerExclBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	vVal := b.ensureValue(call.Args[0])
	xVal := b.ensureValue(call.Args[1])
	bit := b.NewTemp(vVal.Type())
	b.Emit(&BinaryInst{Op: LSHL, Target: bit, Left: UInt64Lit(1), Right: xVal})
	mask := b.NewTemp(xVal.Type())
	b.Emit(&UnaryInst{Op: NOT, Target: mask, Operand: bit})
	newV := b.NewTemp(vVal.Type())
	b.Emit(&BinaryInst{Op: AND, Target: newV, Left: vVal, Right: mask})
	return newV
}

func lowerHaltBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	n := b.ensureValue(call.Args[0])
	b.SetTerm(&HaltInst{Code: n})
	return n
}

func lowerIncBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	addr := b.ensureAddr(call.Args[0])
	x := b.ensureValue(call.Args[0])
	var amt Value
	if len(call.Args) == 1 {
		amt = Int64Lit(1)
	} else {
		amt = b.ensureValue(call.Args[1])
	}
	res := b.NewTemp(x.Type())
	b.Emit(&BinaryInst{Target: res, Op: ADD, Left: x, Right: amt})
	b.emitAssign(addr, res)
	return res
}

func lowerInclBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	vVal := b.ensureValue(call.Args[0])
	xVal := b.ensureValue(call.Args[1])
	bit := b.NewTemp(vVal.Type())
	b.Emit(&BinaryInst{Op: LSHL, Target: bit, Left: UInt64Lit(1), Right: xVal})
	newV := b.NewTemp(vVal.Type())
	b.Emit(&BinaryInst{Op: OR, Target: newV, Left: vVal, Right: bit})
	return newV
}

func lowerNumberBuiltin(_ *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	panic(fmt.Sprintf("NUMBER: EnumType element count not available at IR level (arg type: %T)", call.Args[0].Type()))
}

func lowerPcallBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	proc := b.ensureValue(call.Args[0])
	var args []Value
	for _, a := range call.Args[1:] {
		args = append(args, b.ensureValue(a))
	}
	ret := b.NewTemp(Void)
	b.Emit(&CallInst{Target: ret, Callee: proc.Name(), Args: args})
	return ret
}

func lowerRaiseBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	n := b.ensureValue(call.Args[0])
	b.SetTerm(&HaltInst{Code: n})
	return n
}

func lowerCopyBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	src := b.ensureAddr(call.Args[0])
	dst := b.ensureAddr(call.Args[1])
	b.Emit(&CallInst{Callee: "__obx_copy", Args: []Value{src, dst}})
	return nil
}

func lowerPackBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xAddr := b.ensureAddr(call.Args[0])
	xVal := b.NewTemp(Float64Type)
	b.Emit(&LoadInst{Target: xVal, Addr: xAddr})
	n := b.ensureValue(call.Args[1])
	result := b.NewTemp(Float64Type)
	b.Emit(&CallInst{Target: result, Callee: "__obx_pack", Args: []Value{xVal, n}})
	b.emitAssign(xAddr, result)
	return result
}

func lowerUnpackBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xAddr := b.ensureAddr(call.Args[0])
	eAddr := b.ensureAddr(call.Args[1])
	xVal := b.NewTemp(Float64Type)
	b.Emit(&LoadInst{Target: xVal, Addr: xAddr})
	mantissa := b.NewTemp(Float64Type)
	exponent := b.NewTemp(Int32Type)
	b.Emit(&CallInst{Target: mantissa, Callee: "__obx_unpack_m", Args: []Value{xVal}})
	b.Emit(&CallInst{Target: exponent, Callee: "__obx_unpack_e", Args: []Value{xVal}})
	b.emitAssign(xAddr, mantissa)
	b.emitAssign(eAddr, exponent)
	return mantissa
}

// ─── Predeclared functions ────────────────────────────────────────────────

func lowerLDCMDBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	mod := b.ensureValue(call.Args[0])
	name := b.ensureValue(call.Args[1])
	res := b.NewTemp(PointerTo(UInt8Type))
	b.Emit(&CallInst{Target: res, Callee: "__obx_ldcmd", Args: []Value{mod, name}})
	return res
}

func lowerLDMODBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	name := b.ensureValue(call.Args[0])
	res := b.NewTemp(PointerTo(UInt8Type))
	b.Emit(&CallInst{Target: res, Callee: "__obx_ldmod", Args: []Value{name}})
	return res
}

func lowerASHBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	x := b.ensureValue(call.Args[0])
	n := b.ensureValue(call.Args[1])
	result := b.NewTemp(x.Type())

	blkLeft := b.NewBlock(b.NewLabel("ash.left"))
	blkRight := b.NewBlock(b.NewLabel("ash.right"))
	blkJoin := b.NewBlock(b.NewLabel("ash.join"))

	t0 := b.NewTemp(Int1Type)
	b.Emit(&ICmpInst{Target: t0, Op: GT, Left: n, Right: Int32Lit(0)})
	b.SetTerm(&CondBrInst{Cond: t0, TrueLabel: blkLeft.Label, FalseLabel: blkRight.Label})

	b.SetBlock(blkLeft)
	b.Emit(&BinaryInst{Op: LSHL, Target: result, Left: x, Right: n})
	b.SetTerm(&JumpInst{Target: blkJoin.Label})

	b.SetBlock(blkRight)
	tAbs := b.NewPrefixTemp("abs", n.Type())
	b.Emit(&UnaryInst{Target: tAbs, Op: NEG, Operand: n})
	b.Emit(&BinaryInst{Target: result, Op: ASHR, Left: x, Right: tAbs})
	b.SetTerm(&JumpInst{Target: blkJoin.Label})

	b.SetBlock(blkJoin)
	return result
}

func lowerASRBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	x := b.ensureValue(call.Args[0])
	n := b.ensureValue(call.Args[1])
	res := b.NewTemp(x.Type())
	b.Emit(&BinaryInst{Target: res, Op: ASHR, Left: x, Right: n})
	return res
}

func lowerENTIERBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	x := b.ensureValue(call.Args[0])
	res := b.NewTemp(Int64Type)
	b.Emit(&CallInst{Target: res, Callee: "floor", Args: []Value{x}})
	return res
}

func lowerLSLBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	x := b.ensureValue(call.Args[0])
	n := b.ensureValue(call.Args[1])
	res := b.NewTemp(x.Type())
	b.Emit(&BinaryInst{Target: res, Op: LSHL, Left: x, Right: n})
	return res
}

func lowerRORBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	x := b.ensureValue(call.Args[0])
	n := b.ensureValue(call.Args[1])
	bitWidth := b.ctx.TargetMachineWordSize

	result := b.NewTemp(x.Type())
	nMod := b.NewTemp(x.Type())
	right := b.NewTemp(x.Type())
	left := b.NewTemp(x.Type())

	b.Emit(&BinaryInst{Op: AND, Target: nMod, Left: n, Right: UInt64Lit(bitWidth - 1)})
	b.Emit(&BinaryInst{Op: LSHR, Target: right, Left: x, Right: nMod})

	wMinusN := b.NewTemp(x.Type())
	b.Emit(&BinaryInst{Op: SUB, Target: wMinusN, Left: UInt64Lit(bitWidth), Right: nMod})
	b.Emit(&BinaryInst{Op: LSHL, Target: left, Left: x, Right: wMinusN})
	b.Emit(&BinaryInst{Op: OR, Target: result, Left: left, Right: right})

	return result
}

func lowerCastBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	targetType := b.lowerType(call.Args[0].Type())
	x := b.ensureValue(call.Args[1])
	res := b.NewTemp(targetType)
	b.Emit(&MoveInst{Target: res, Value: x})
	return res
}

func lowerCHRBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	x := b.ensureValue(call.Args[0])
	target := b.NewTemp(UInt8Type)
	b.Emit(&BinaryInst{Target: target, Op: AND, Left: x, Right: UInt64Lit(0xFF)})
	return target
}

func lowerDefaultBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	ty := b.lowerType(call.Args[0].Type())
	res := b.NewTemp(ty)
	switch ty.(type) {
	case *FloatType:
		b.Emit(&MoveInst{Target: res, Value: Float64Lit(0.0)})
	default:
		b.Emit(&MoveInst{Target: res, Value: Int64Lit(0)})
	}
	return res
}

func lowerFloorBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	x := b.ensureValue(call.Args[0])
	res := b.NewTemp(Int64Type)
	b.Emit(&CallInst{Target: res, Callee: "floor", Args: []Value{x}})
	return res
}

func lowerFLTBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	x := b.ensureValue(call.Args[0])
	res := b.NewTemp(Float32Type)
	b.Emit(&MoveInst{Target: res, Value: x})
	return res
}

func lowerLenBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	x := b.ensureAddr(call.Args[0])

	dim := 0
	if len(call.Args) > 1 {
		if lit, ok := call.Args[1].(*desugar.Literal); ok {
			d, _ := strconv.Atoi(lit.Value)
			dim = d
		}
	}

	arrTy := x.Type()
	if pt, ok := arrTy.(*PointerType); ok {
		t := b.NewTemp(pt.Ref)
		b.Emit(&LoadInst{Target: t, Addr: x})
		x = t
		arrTy = pt.Ref
	}

	switch ty := arrTy.(type) {
	case *ArrayType:
		if ty.IsOpen() {
			addr := b.NewTemp(Int64Type)
			b.Emit(&BinaryInst{
				Target: addr, Op: ADD, Left: x,
				Right: Int64Lit(uint64(dim) * b.ctx.TargetMachineWordSize),
			})
			length := b.NewTemp(Int32Type)
			b.Emit(&LoadInst{Target: length, Addr: &Mem{Base: addr}})
			return length
		}
		dims := ty.Dimensions()
		if dim < len(dims) {
			res := b.NewTemp(Int32Type)
			b.Emit(&MoveInst{Target: res, Value: Int32Lit(int64(dims[dim]))})
			return res
		}
		panic(fmt.Sprintf("LEN: dimension %d out of range", dim))
	default:
		panic(fmt.Sprintf("LEN: argument of type %T is not an array", arrTy))
	}
}

func lowerLongBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	switch xTyp := xVal.Type().(type) {
	case *IntegerType:
		if xTyp.Signed {
			bwSrc := xTyp.Bits
			bwDst := bwSrc * 2
			shift := Int32Lit(int64(bwDst - bwSrc))
			t := b.NewTemp(UInt64Type)
			out := b.NewTemp(UInt64Type)
			b.Emit(&BinaryInst{Target: t, Op: LSHL, Left: xVal, Right: shift})
			b.Emit(&BinaryInst{Target: out, Op: ASHR, Left: t, Right: shift})
			return out
		}
		bwSrc := xTyp.Bits
		mask := uint64((1 << bwSrc) - 1)
		out := b.NewTemp(UInt64Type)
		b.Emit(&BinaryInst{Target: out, Op: AND, Left: xVal, Right: UInt64Lit(mask)})
		return out
	case *FloatType:
		out := b.NewTemp(Float64Type)
		b.Emit(&MoveInst{Target: out, Value: xVal})
		return out
	default:
		panic("unsupported type for LONG")
	}
}

func lowerMaxBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	if len(call.Args) == 1 {
		v := b.ensureValue(call.Args[0])
		tv, ok := v.(*TypeValue)
		if !ok {
			panic("MAX: argument is not a type")
		}
		return lowerPredeclaredMAXType(tv.Ty, b)
	}
	return lowerPredeclaredMAXInt(call.Args[0], call.Args[1], b)
}

func lowerPredeclaredMAXType(typ Type, b *IRBuilder) Value {
	temp := b.NewTemp(typ)
	switch ty := typ.(type) {
	case *IntegerType:
		var maxVal uint64
		if ty.Signed {
			maxVal = (uint64(1) << uint(ty.Bits-1)) - 1
		} else {
			maxVal = (uint64(1) << uint(ty.Bits)) - 1
		}
		b.Emit(&MoveInst{Target: temp, Value: Int64Lit(maxVal)})
	case *Set:
		b.Emit(&MoveInst{Target: temp, Value: Int64Lit(uint64(math.MaxUint32))})
	case *FloatType:
		var maxVal float64
		if ty.Bits == 32 {
			maxVal = 3.4028235e+38
		} else {
			maxVal = 1.7976931348623157e+308
		}
		b.Emit(&MoveInst{Target: temp, Value: Float64Lit(maxVal)})
	default:
		panic("unsupported type for MAX")
	}
	return temp
}

func lowerPredeclaredMAXInt(xExpr, yExpr desugar.Expr, b *IRBuilder) Value {
	x := b.ensureValue(xExpr)
	y := b.ensureValue(yExpr)
	d := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Op: SUB, Target: d, Left: x, Right: y})
	m := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Op: ASHR, Target: m, Left: d, Right: Int64Lit((b.ctx.TargetMachineWordSize * 8) - 1)})
	t := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Op: AND, Target: t, Left: m, Right: d})
	z := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Op: SUB, Target: z, Left: x, Right: t})
	return z
}

func lowerMinBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	if len(call.Args) == 1 {
		v := b.ensureValue(call.Args[0])
		tv, ok := v.(*TypeValue)
		if !ok {
			panic("MIN: argument is not a type")
		}
		return lowerPredeclaredMINType(tv.Ty, b)
	}
	return lowerMINInts(call.Args[0], call.Args[1], b)
}

func lowerPredeclaredMINType(typ Type, b *IRBuilder) Value {
	temp := b.NewTemp(typ)
	switch ty := typ.(type) {
	case *IntegerType:
		var minVal int64
		if ty.Signed {
			minVal = int64(-1) << uint(ty.Bits-1)
		}
		b.Emit(&MoveInst{Target: temp, Value: Int64Lit(uint64(minVal))})
	case *Set:
		b.Emit(&MoveInst{Target: temp, Value: Int64Lit(0)})
	case *FloatType:
		var minVal float64
		if ty.Bits == 32 {
			minVal = -3.4028235e+38
		} else {
			minVal = -1.7976931348623157e+308
		}
		b.Emit(&MoveInst{Target: temp, Value: Float64Lit(minVal)})
	default:
		panic("unsupported type for MIN")
	}
	return temp
}

func lowerMINInts(xExpr, yExpr desugar.Expr, b *IRBuilder) Value {
	x := b.ensureValue(xExpr)
	y := b.ensureValue(yExpr)
	d := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Op: SUB, Target: d, Left: x, Right: y})
	m := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Op: ASHR, Target: m, Left: d, Right: Int64Lit((b.ctx.TargetMachineWordSize * 8) - 1)})
	t := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Op: AND, Target: t, Left: m, Right: d})
	z := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Op: ADD, Target: z, Left: y, Right: t})
	return z
}

func lowerOddBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	t := b.NewTemp(xVal.Type())
	b.Emit(&BinaryInst{Op: AND, Target: t, Left: xVal, Right: UInt64Lit(1)})
	result := b.NewTemp(Int1Type)
	b.Emit(&ICmpInst{Op: NE, Target: result, Left: t, Right: UInt64Lit(0)})
	return result
}

func lowerOrdBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	return b.ensureValue(call.Args[0])
}

func lowerShortBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	switch xTyp := xVal.Type().(type) {
	case *IntegerType:
		bwDst := xTyp.Bits / 2
		mask := uint64((uint64(1) << uint(bwDst)) - 1)
		out := b.NewTemp(&IntegerType{Bits: bwDst, Signed: xTyp.Signed})
		b.Emit(&BinaryInst{Target: out, Op: AND, Left: xVal, Right: UInt64Lit(mask)})
		return out
	case *FloatType:
		out := b.NewTemp(Float32Type)
		b.Emit(&MoveInst{Target: out, Value: xVal})
		return out
	default:
		panic("unsupported type for SHORT")
	}
}

func lowerSizeBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	tVal := b.ensureValue(call.Args[0])
	tv, ok := tVal.(*TypeValue)
	if !ok {
		panic("SIZE: argument is not a type")
	}
	sizeTemp := b.NewTemp(Int64Type)
	b.Emit(&MoveInst{Target: sizeTemp, Value: Int64Lit(uint64(tv.Ty.Width()))})
	return sizeTemp
}

func lowerWCHARBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	x := b.ensureValue(call.Args[0])
	target := b.NewTemp(UInt16Type)
	b.Emit(&BinaryInst{Target: target, Op: AND, Left: x, Right: UInt64Lit(0xFFFF)})
	return target
}

func lowerStrLenBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	addr := b.ensureAddr(call.Args[0])
	length := b.NewTemp(Int32Type)
	b.Emit(&LoadInst{Target: length, Addr: addr})
	b.Emit(&BinaryInst{Target: length, Op: ADD, Left: length, Right: Int32Lit(-1)})
	return length
}

func lowerBitSHRBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])
	res := b.NewTemp(xVal.Type())
	b.Emit(&BinaryInst{Target: res, Op: LSHR, Left: xVal, Right: yVal})
	return res
}

func lowerBitSHLBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])
	res := b.NewTemp(xVal.Type())
	b.Emit(&BinaryInst{Target: res, Op: LSHL, Left: xVal, Right: yVal})
	return res
}

func lowerBitsBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	res := b.NewTemp(SetType)
	b.Emit(&MoveInst{Target: res, Value: xVal})
	return res
}

func lowerBitASRBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])
	res := b.NewTemp(xVal.Type())
	b.Emit(&BinaryInst{Target: res, Op: ASHR, Left: xVal, Right: yVal})
	return res
}

func lowerBitNotBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	res := b.NewTemp(xVal.Type())
	b.Emit(&UnaryInst{Target: res, Op: NOT, Operand: xVal})
	return res
}

func lowerBitOrBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])
	res := b.NewTemp(xVal.Type())
	b.Emit(&BinaryInst{Target: res, Op: OR, Left: xVal, Right: yVal})
	return res
}

func lowerBitXorBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])
	res := b.NewTemp(xVal.Type())
	b.Emit(&BinaryInst{Target: res, Op: XOR, Left: xVal, Right: yVal})
	return res
}

func lowerBitAndBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])
	res := b.NewTemp(xVal.Type())
	b.Emit(&BinaryInst{Target: res, Op: AND, Left: xVal, Right: yVal})
	return res
}

func lowerAbsBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	arg := b.ensureValue(call.Args[0])
	if _, ok := arg.Type().(*IntegerType); ok {
		return lowerABSInteger(arg, b)
	}
	return lowerABSReal(arg, b)
}

func lowerABSReal(x Value, b *IRBuilder) Value {
	zero := Float64Lit(0.0)
	cmp := b.NewTemp(Int1Type)
	b.Emit(&FCmpInst{Target: cmp, Op: FLT, Left: x, Right: zero})

	blkNeg := b.NewBlock("abs.neg")
	blkPos := b.NewBlock("abs.pos")
	blkEnd := b.NewBlock("abs.end")

	b.SetTerm(&CondBrInst{Cond: cmp, TrueLabel: blkNeg.Label, FalseLabel: blkPos.Label})

	b.SetBlock(blkNeg)
	negTemp := b.NewTemp(x.Type())
	b.Emit(&UnaryInst{Target: negTemp, Op: FNEG, Operand: x})
	b.SetTerm(&JumpInst{Target: blkEnd.Label})

	b.SetBlock(blkPos)
	posTemp := b.NewTemp(x.Type())
	b.Emit(&MoveInst{Target: posTemp, Value: x})
	b.SetTerm(&JumpInst{Target: blkEnd.Label})

	b.SetBlock(blkEnd)
	result := b.NewTemp(x.Type())
	phi := &PhiInst{Target: result}
	phi.AddArg(blkNeg, negTemp)
	phi.AddArg(blkPos, posTemp)
	b.Emit(phi)

	return result
}

func lowerABSInteger(x Value, b *IRBuilder) Value {
	mask := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Target: mask, Op: ASHR, Left: x, Right: Int32Lit(31)})
	t1 := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Target: t1, Op: XOR, Left: x, Right: mask})
	result := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{Target: result, Op: SUB, Left: t1, Right: mask})
	return result
}

func lowerCapBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	res := b.NewTemp(UInt8Type)
	b.Emit(&BinaryInst{Target: res, Op: AND, Left: xVal, Right: UInt8Lit(223)})
	return res
}

func lowerPrintfBuiltin(b *IRBuilder, fn *Function, call *desugar.FuncCall) Value {
	var args []Value
	b.lowerArgs(fn, call.Args, 0, len(fn.Params), &args)
	if fn.Variadic {
		b.lowerVarArgs(call.Args, len(fn.Params), len(call.Args), &args)
	}
	ret := b.NewTemp(Int64Type)
	b.Emit(&CallInst{Target: ret, Callee: "printf", Args: args})
	return ret
}

func lowerNewBuiltin(b *IRBuilder, _ *Function, call *desugar.FuncCall) Value {
	dst := b.ensureAddr(call.Args[0])
	args := []Value{dst}
	for _, a := range call.Args[1:] {
		args = append(args, b.ensureValue(a))
	}

	var ptr Value
	if len(args) > 1 {
		dims := args[1:]
		arr := args[0].Type().(*PointerType).Ref.(*ArrayType)
		elemSize := uint64(arr.Elem.Width())
		ptr = b.lowerNEWOpen(dims, elemSize, b.ctx.TargetMachineWordSize)
	} else {
		ty := dst.Type()
		if pt, ok := ty.(*PointerType); ok {
			ty = pt.Ref
		}
		switch v := ty.(type) {
		case *ArrayType:
			ptr = b.lowerNEWFixedArray(uint64(v.Width()))
		case *RecordType:
			ptr = b.lowerNEWPtrToRec(uint64(v.Width()))
		default:
			panic("unsupported type for new")
		}
	}

	b.emitAssign(dst, ptr)
	return ptr
}

