package mir

import "github.com/anthonyabeo/obx/src/ir/hir"

var builtinLowering = map[string]func(*IRBuilder, *Function, *hir.FuncCall) Value{
	"new":    lowerNewBuiltin,
	"printf": lowerPrintfBuiltin,
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
