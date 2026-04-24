package obxir

import "github.com/anthonyabeo/obx/src/ir/desugar"

var externalsLowering map[string]func(*IRBuilder, *desugar.FuncCall) Value

func init() {
	externalsLowering = map[string]func(*IRBuilder, *desugar.FuncCall) Value{
		"adr": lowerAdr,

		"printf":   lowerPrintf,
		"fprintf":  lowerFPrintf,
		"sprintf":  lowerSPrintf,
		"snprintf": lowerSNPrintf,
		"scanf":    lowerScanf,
		"fscanf":   lowerFScanf,
		"sscanf":   lowerSScanf,
		"putchar":  lowerPutChar,
		"getchar":  lowerGetChar,
		"putc":     lowerPutC,
		"getc":     lowerGetC,
		"puts":     lowerPuts,
		"fputs":    lowerFPuts,
		"fgets":    lowerFGets,
		"fread":    lowerFRead,
		"fwrite":   lowerFWrite,
		"fopen":    lowerFOpen,
		"fclose":   lowerFClose,
		"fflush":   lowerFFlush,
		"fseek":    lowerFSeek,
		"ftell":    lowerFTell,
		"rewind":   lowerRewind,
		"feof":     lowerFeof,
		"ferror":   lowerFError,
		"remove":   lowerRemove,
		"rename":   lowerRename,
	}
}

// helper that emits Arg instructions, a CallInst and returns a temp of retType
func callExternal(b *IRBuilder, v *desugar.FuncCall, callee string, retType Type) Value {
	var args []Value
	for i, a := range v.Args {
		val := b.ensureValue(a)
		b.Emit(&Arg{Index: i, Value: val})
		args = append(args, val)
	}
	var ret Value
	if retType == nil {
		ret = b.NewTemp(Void)
	} else {
		ret = b.NewTemp(retType)
	}
	b.Emit(&CallInst{Target: ret, Callee: b.lookupCalleeByName(callee), Args: args})
	return ret
}

func lowerPrintf(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "printf", Int64Type)
}
func lowerFPrintf(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "fprintf", Int64Type)
}
func lowerSPrintf(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "sprintf", Int64Type)
}
func lowerSNPrintf(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "snprintf", Int64Type)
}
func lowerScanf(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "scanf", Int64Type)
}
func lowerFScanf(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "fscanf", Int64Type)
}
func lowerSScanf(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "sscanf", Int64Type)
}
func lowerPutChar(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "putchar", Int64Type)
}
func lowerGetChar(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "getchar", Int64Type)
}
func lowerPutC(b *IRBuilder, v *desugar.FuncCall) Value { return callExternal(b, v, "putc", Int64Type) }
func lowerPuts(b *IRBuilder, v *desugar.FuncCall) Value { return callExternal(b, v, "puts", Int64Type) }
func lowerGetC(b *IRBuilder, v *desugar.FuncCall) Value { return callExternal(b, v, "getc", Int64Type) }
func lowerFPuts(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "fputs", Int64Type)
}
func lowerFGets(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "fgets", PointerTo(UInt8Type))
}
func lowerFRead(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "fread", Int64Type)
}
func lowerFWrite(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "fwrite", Int64Type)
}
func lowerFOpen(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "fopen", PointerTo(UInt8Type))
}
func lowerFClose(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "fclose", Int64Type)
}
func lowerFFlush(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "fflush", Int64Type)
}
func lowerFSeek(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "fseek", Int64Type)
}
func lowerFTell(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "ftell", Int64Type)
}
func lowerRewind(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "rewind", Int64Type)
}
func lowerFeof(b *IRBuilder, v *desugar.FuncCall) Value { return callExternal(b, v, "feof", Int64Type) }
func lowerFError(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "ferror", Int64Type)
}
func lowerRemove(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "remove", Int64Type)
}
func lowerRename(b *IRBuilder, v *desugar.FuncCall) Value {
	return callExternal(b, v, "rename", Int64Type)
}

// returns the machine address of any variable as INTEGER.
// Accepts one argument of any type (IsVarArgs covers extra/any args).
func lowerAdr(b *IRBuilder, v *desugar.FuncCall) Value {
	if len(v.Args) == 0 {
		return nil
	}
	// We accept any expression. If it's addressable, create an AddrOf and
	// return its machine integer representation. Otherwise, evaluate the
	// expression to a value and return its representation as an integer.
	argExpr := v.Args[0]

	// Try addressable cases first (variable, param, field, index, deref)
	switch argExpr.(type) {
	case *desugar.VariableRef, *desugar.Param, *desugar.FieldAccess, *desugar.IndexExpr, *desugar.DerefExpr:
		addr := b.ensureAddr(argExpr)
		// materialize address into a temp (ADDR instr)
		addrTemp := b.CreateAddrOf(addr)
		// convert address value to machine INTEGER (use Int64Type for pointers)
		out := b.NewTemp(Int64Type)
		b.Emit(&MoveInst{Target: out, Value: addrTemp})
		return out
	default:
		// non-addressable: evaluate to value and return as integer
		val := b.ensureValue(argExpr)
		out := b.NewTemp(Int64Type)
		b.Emit(&MoveInst{Target: out, Value: val})
		return out
	}
}
