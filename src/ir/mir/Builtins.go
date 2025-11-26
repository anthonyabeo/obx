package mir

import "github.com/anthonyabeo/obx/src/ir/hir"

var builtinLowering = map[string]func(*IRBuilder, *Function, *hir.FuncCall) Value{
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

func lowerAssertBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerBytesBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerDecBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerExclBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerHaltBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerIncBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerInclBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerNumberBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerPcallBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerRaiseBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerCopyBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerPackBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerUnpackBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerLDCMDBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerLDMODBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerASHBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerASRBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerENTIERBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerLSLBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerRORBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerCastBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerCHRBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerDefaultBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerFloorBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerFLTBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerLenBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerLongBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerMaxBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerMinBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	if len(call.Args) == 1 {
		v := b.ensureValue(call.Args[0])
		return lowerPredeclaredMINType(v.(Type), b)
	} else {
		return lowerMINInts(call.Args[0], call.Args[1], b)
	}
}

func lowerPredeclaredMINType(typ Type, b *IRBuilder) Value {
	temp := b.NewTemp(typ)

	switch ty := typ.(type) {
	case *IntegerType:
		var minVal int64
		if ty.Signed {
			minVal = int64(-1) << uint(ty.Bits-1)
		}

		b.Emit(&MoveInst{
			Target: temp,
			Value:  Int64Lit(uint64(minVal)),
		})
	case *Set:
		b.Emit(&MoveInst{
			Target: temp,
			Value:  Int64Lit(0),
		})
	case *FloatType:
		var minVal float64
		if ty.Bits == 32 {
			minVal = -3.4028235e+38 // min float32
		} else {
			minVal = -1.7976931348623157e+308 // min float64
		}

		b.Emit(&MoveInst{
			Target: temp,
			Value:  Float64Lit(minVal), // min float64
		})
	case *EnumType:
		panic("not implemented")
	default:
		panic("unsupported type for predeclared MIN")
	}

	return temp
}

func lowerMINInts(xExpr, yExpr hir.Expr, b *IRBuilder) Value {
	x := b.ensureValue(xExpr)
	y := b.ensureValue(yExpr)

	// d = x - y
	d := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{
		Op:     SUB,
		Target: d,
		Left:   x,
		Right:  y,
	})

	// m = d >> 31   (arithmetic shift → sign-bit mask)
	m := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{
		Op:     ASHR, // arithmetic shift
		Target: m,
		Left:   d,
		Right:  Int64Lit((b.ctx.TargetMachineWordSize * 8) - 1),
	})

	// t = m & d
	t := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{
		Op:     AND,
		Target: t,
		Left:   m,
		Right:  d,
	})

	// z = y + t
	z := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{
		Op:     ADD,
		Target: z,
		Left:   y,
		Right:  t,
	})

	return z
}

func lowerOddBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	// 1. Evaluate argument
	xVal := b.ensureValue(call.Args[0])

	// 2. Generate temp for (x & 1)
	t := b.NewTemp(xVal.Type()) // same integer type as x
	b.Emit(&BinaryInst{
		Op:     AND,
		Target: t,
		Left:   xVal,
		Right:  UInt64Lit(1),
	})

	// 3. Compare != 0 to produce BOOLEAN
	result := b.NewTemp(Int1Type)
	b.Emit(&ICmpInst{
		Op:     NE,
		Target: result,
		Left:   t,
		Right:  UInt64Lit(0),
	})

	return result
}

func lowerOrdBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	return b.ensureValue(call.Args[0])
}

func lowerShortBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerSizeBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	tVal := b.ensureValue(call.Args[0])

	typ, ok := tVal.(Type)
	if !ok {
		panic("size argument is not a type")
	}

	sizeTemp := b.NewTemp(Int64Type)
	b.Emit(&MoveInst{
		Target: sizeTemp,
		Value:  Int64Lit(uint64(typ.Width())),
	})

	return sizeTemp
}

func lowerWCHARBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerStrLenBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerBitSHRBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])

	// Allocate result temp
	res := b.NewTemp(xVal.Type())

	// Emit the logical shift right operation
	b.Emit(&BinaryInst{
		Target: res,
		Op:     LSHR,
		Left:   xVal,
		Right:  yVal,
	})

	return res
}

func lowerBitSHLBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])

	// Allocate result temp
	res := b.NewTemp(xVal.Type())

	// Emit the logical shift left operation
	b.Emit(&BinaryInst{
		Target: res,
		Op:     LSHL,
		Left:   xVal,
		Right:  yVal,
	})

	return res
}

func lowerBitsBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	panic("not implemented")
}

func lowerBitASRBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])

	// Allocate result temp
	res := b.NewTemp(xVal.Type())

	// Emit the arithmetic shift right operation
	b.Emit(&BinaryInst{
		Target: res,
		Op:     ASHR,
		Left:   xVal,
		Right:  yVal,
	})

	return res
}

func lowerBitNotBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])

	// Allocate result temp
	res := b.NewTemp(xVal.Type())

	// Emit the bitwise NOT operation
	b.Emit(&UnaryInst{
		Target:  res,
		Op:      NOT,
		Operand: xVal,
	})

	return res
}

func lowerBitOrBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])

	// Allocate result temp
	res := b.NewTemp(xVal.Type())

	// Emit the bitwise OR operation
	b.Emit(&BinaryInst{
		Target: res,
		Op:     OR,
		Left:   xVal,
		Right:  yVal,
	})
	return res
}

func lowerBitXorBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])

	// Allocate result temp
	res := b.NewTemp(xVal.Type())

	// Emit the bitwise XOR operation
	b.Emit(&BinaryInst{
		Target: res,
		Op:     XOR,
		Left:   xVal,
		Right:  yVal,
	})

	return res
}

func lowerBitAndBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	xVal := b.ensureValue(call.Args[0])
	yVal := b.ensureValue(call.Args[1])

	// Allocate result temp
	res := b.NewTemp(xVal.Type())

	// Emit the bitwise AND operation
	b.Emit(&BinaryInst{
		Target: res,
		Op:     AND, // your 3AC AND opcode
		Left:   xVal,
		Right:  yVal,
	})

	return res
}

func lowerAbsBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	arg := b.ensureValue(call.Args[0])

	if _, ok := arg.Type().(*IntegerType); ok {
		return lowerABSInteger(arg, b)
	} else {
		return lowerABSReal(arg, b)
	}
}

func lowerABSReal(x Value, b *IRBuilder) Value {
	// compare x < 0.0
	zero := Float64Lit(0.0)
	cmp := b.NewTemp(Int1Type)
	b.Emit(&FCmpInst{
		Target: cmp,
		Op:     FLT,
		Left:   x,
		Right:  zero,
	})

	blkNeg := b.NewBlock("abs.neg")
	blkPos := b.NewBlock("abs.pos")
	blkEnd := b.NewBlock("abs.end")

	b.SetTerm(&CondBrInst{Cond: cmp, TrueLabel: blkNeg.Label, FalseLabel: blkPos.Label})

	// Negative branch: result = -x
	b.SetBlock(blkNeg)
	negTemp := b.NewTemp(x.Type())
	b.Emit(&UnaryInst{Target: negTemp, Op: FNEG, Operand: x})
	b.SetTerm(&JumpInst{Target: blkEnd.Label})

	// Positive branch: result = x
	b.SetBlock(blkPos)
	posTemp := b.NewTemp(x.Type())
	b.Emit(&MoveInst{Target: posTemp, Value: x})
	b.SetTerm(&JumpInst{Target: blkEnd.Label})

	// Join
	b.SetBlock(blkEnd)
	result := b.NewTemp(x.Type())
	b.emitAssign(result, posTemp)

	return result
}

func lowerABSInteger(x Value, b *IRBuilder) Value {
	// mask = x >> 31
	mask := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{
		Target: mask,
		Op:     ASHR, // arithmetic shift right
		Left:   x,
		Right:  Int32Lit(31),
	})

	// t1 = x XOR mask
	t1 := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{
		Target: t1,
		Op:     XOR,
		Left:   x,
		Right:  mask,
	})

	// result = t1 - mask
	result := b.NewTemp(Int32Type)
	b.Emit(&BinaryInst{
		Target: result,
		Op:     SUB,
		Left:   t1,
		Right:  mask,
	})

	return result
}

// lowerCapBuiltin lowers the builtin function CAP(x)
// into 3AC form: result = x AND 0xDF.
//
// Semantics: If x is an ASCII lowercase letter, produces its uppercase.
// Otherwise, x is returned unchanged. This matches Oberon+’s definition
// ("only the ASCII subset").
//
// Input:
//
//	arg   — expression node for x (CHAR)
//	b     — pointer to your IR builder
//
// Output:
//
//	Value — a temp holding the result.
func lowerCapBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
	// 1. Get the 3AC value of the argument
	xVal := b.ensureValue(call.Args[0])

	// 2. Allocate a temp for the result (CHAR treated as integer)
	res := b.NewTemp(UInt8Type)

	// 3. Emit a single AND instruction with 0xDF (223)
	b.Emit(&BinaryInst{
		Target: res,
		Op:     AND, // your IR's AND opcode
		Left:   xVal,
		Right:  UInt8Lit(223), // 0xDF
	})

	return res
}

func lowerPrintfBuiltin(b *IRBuilder, fn *Function, call *hir.FuncCall) Value {
	var args []Value
	b.lowerArgs(fn, call.Args, 0, len(fn.Params), &args)
	if fn.Variadic {
		b.lowerVarArgs(call.Args, len(fn.Params), len(call.Args), &args)
	}

	ret := b.NewTemp(Int64Type)
	b.Emit(&CallInst{
		Target: ret,
		Callee: "printf",
		Args:   args,
	})
	return ret
}

func lowerNewBuiltin(b *IRBuilder, _ *Function, call *hir.FuncCall) Value {
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
		switch v := dst.Type().(type) {
		case *ArrayType:
			ptr = b.lowerNEWFixedArray(uint64(args[0].Type().Width()))
		case *RecordType:
			ptr = b.lowerNEWPtrToRec(uint64(v.Width()))
		default:
			panic("unsupported type for new")
		}
	}

	b.emitAssign(dst, ptr)
	return ptr
}

func (b *IRBuilder) lowerNEWPtrToRec(allocSize uint64) Value {
	sizeTemp := b.NewTemp(Int64Type)
	b.Emit(&MoveInst{Target: sizeTemp, Value: Int64Lit(allocSize)})

	ptrTemp := b.NewTemp(PointerTo(UInt8Type))
	b.Emit(&CallInst{
		Target: ptrTemp,
		Callee: "malloc",
		Args:   []Value{sizeTemp},
	})

	return ptrTemp
}

func (b *IRBuilder) lowerNEWFixedArray(allocSize uint64) Value {
	// size immediate
	sizeImm := UInt64Lit(allocSize)

	// prepare argument: t = MOV sizeImm
	t := b.NewTemp(Int64Type)
	b.Emit(&MoveInst{Target: t, Value: sizeImm})
	b.Emit(&Arg{Index: 0, Value: t})

	// call allocator: t_ptr = CALL __alloc, sizeImm
	ptr := b.NewTemp(PointerTo(UInt8Type))
	b.Emit(&CallInst{
		Target: ptr,
		Callee: "malloc",
		Args:   []Value{t},
	})

	return ptr
}

func (b *IRBuilder) lowerNEWOpen(dims []Value, elemSize uint64, wordSize uint64) Value {
	n := len(dims)

	// 1) compute product = x0 * x1 * ... * x_{n-1}
	prod := b.NewTemp(Int64Type)
	b.Emit(&MoveInst{Target: prod, Value: Int64Lit(1)})
	for _, d := range dims {
		tmp := b.NewTemp(Int64Type)
		b.Emit(&BinaryInst{
			Target: tmp,
			Op:     MUL,
			Left:   prod,
			Right:  d,
		})
		prod = tmp
	}

	// 2) multiply by element size: bytesForData = prod * elemSize
	elemSizeImm := Int64Lit(elemSize)
	dataBytes := b.NewTemp(UInt64Type)
	b.Emit(&BinaryInst{
		Target: dataBytes,
		Op:     MUL,
		Left:   prod,
		Right:  elemSizeImm,
	})

	// 3) add header size: totalSize = dataBytes + (n * wordSize)
	headerBytesImm := Int64Lit(uint64(n) * wordSize)
	totalSize := b.NewTemp(Int64Type)
	b.Emit(&BinaryInst{
		Target: totalSize,
		Op:     ADD,
		Left:   dataBytes,
		Right:  headerBytesImm,
	})

	// (Optional) alignment: round totalSize up to multiple of wordSize.
	// If you want alignment, do:
	// pad := NewTemp()
	// Emit( ADD totalSize, NewImm(wordSize-1) -> pad )
	// Emit( AND pad, ~ (wordSize-1) -> totalSize )
	// Omitted here for brevity.

	b.Emit(&Arg{Index: 0, Value: totalSize})

	// 4) call allocator: ptr = CALL __alloc, totalSize
	ptr := b.NewTemp(Int64Type)
	b.Emit(&CallInst{
		Target: ptr,
		Callee: "malloc",
		Args:   []Value{totalSize},
	})

	// 5) store dimension lengths into header: STORE dims[i] into (t_ptr + i*wordSize)
	for i, d := range dims {
		// mem = t_ptr + IMM(i * wordSize)
		offsetImm := int64(i) * int64(wordSize)
		mem := &Mem{Base: ptr, Offs: offsetImm}
		b.Emit(&StoreInst{
			Addr: mem,
			Val:  d,
		})
	}

	return ptr
}
