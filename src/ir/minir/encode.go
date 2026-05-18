package minir

// encode.go — binary serialisation of a minir.Module.
//
// Wire format overview
// ────────────────────
// A serialised Module is a flat sequence of records, each prefixed with a
// 1-byte opcode and a 4-byte (u32LE) payload length.  The decoder reads
// records until EOF.
//
//	Record = [1] opcode  [4] payloadLen  [N] payload
//
// ── Module-level opcodes ─────────────────────────────────────────────────────
//
//	0x20  REC_GLOBAL  – GlobalVar   (name, linkage, type, optional init)
//	0x21  REC_CONST   – GlobalConst (name, linkage, type, init)
//	0x22  REC_EXTERN  – ExternalFunc (name, sig, ExternalAttrs)
//	0x23  REC_FUNC    – Function body (params, blocks, instructions)
//
// ── Type opcodes (inline within payloads) ────────────────────────────────────
//
//	0x01  MTYPE_PRIM    – PrimitiveType:  [str] name
//	0x02  MTYPE_PTR     – PointerType:    [T] elem
//	0x03  MTYPE_RECORD  – RecordType:     [str] name, [u32] nFields, fields…
//	0x04  MTYPE_ARRAY   – ArrayType:      [i32] len. [T] elem
//	0x05  MTYPE_FUNC    – FunctionType:   [u32] nParams, [T]… params, [bool] hasResult, [T?] result
//	0x06  MTYPE_NIL     – nil type
//
// ── Value opcodes (inline within instruction payloads) ───────────────────────
//
//	0x00  VAL_NIL
//	0x01  VAL_CONST_INT    – [i64] value
//	0x02  VAL_CONST_FLOAT  – [f64] ieee bits
//	0x03  VAL_CONST_NAMED  – [str] NameStr, [i64] intVal
//	0x04  VAL_TEMP         – [str] NameStr, [u32] ID, [bool] IsAddr, [T] type
//	0x05  VAL_GLOBALREF    – [str] GlobalName, [T] ptrType
//
// ── Instruction opcodes ───────────────────────────────────────────────────────
//
//	0x01  OP_PHI     0x02  OP_BIN     0x03  OP_ICMP    0x04  OP_FCMP
//	0x05  OP_LOAD    0x06  OP_STORE   0x07  OP_ALLOCA  0x08  OP_GEP
//	0x09  OP_CALL    0x0A  OP_UNARY   0x0B  OP_CAST
//	0x0C  OP_HALT    0x0D  OP_RET     0x0E  OP_JMP
//	0x0F  OP_CONDBR  0x10  OP_SWITCH

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	"math"
)

// ── module-level record opcodes ──────────────────────────────────────────────
const (
	recGlobal byte = 0x20 // GlobalVar
	recConst  byte = 0x21 // GlobalConst
	recExtern byte = 0x22 // ExternalFunc
	recFunc   byte = 0x23 // Function body
)

// ── type opcodes ──────────────────────────────────────────────────────────────
const (
	mtypePrim   byte = 0x01
	mtypePtr    byte = 0x02
	mtypeRecord byte = 0x03
	mtypeArray  byte = 0x04
	mtypeFunc   byte = 0x05
	mtypeNil    byte = 0x06
)

// ── value opcodes ─────────────────────────────────────────────────────────────
const (
	valNil         byte = 0x00
	valConstInt    byte = 0x01
	valConstFloat  byte = 0x02
	valConstNamed  byte = 0x03
	valTemp        byte = 0x04
	valGlobalRef   byte = 0x05
	valConstString byte = 0x06 // [str] NameStr, [str] Value
	valConstNil    byte = 0x07 // [str] NameStr, [T] type
)

// ── instruction opcodes ───────────────────────────────────────────────────────
const (
	opPhi    byte = 0x01
	opBin    byte = 0x02
	opICmp   byte = 0x03
	opFCmp   byte = 0x04
	opLoad   byte = 0x05
	opStore  byte = 0x06
	opAlloca byte = 0x07
	opGEP    byte = 0x08
	opCall   byte = 0x09
	opUnary  byte = 0x0A
	opCast   byte = 0x0B
	opHalt   byte = 0x0C
	opRet    byte = 0x0D
	opJmp    byte = 0x0E
	opCondBr byte = 0x0F
	opSwitch byte = 0x10
)

// ── helpers ───────────────────────────────────────────────────────────────────

func encWriteU8(w io.Writer, b byte) error {
	_, err := w.Write([]byte{b})
	return err
}
func encWriteU32(w io.Writer, n uint32) error {
	var buf [4]byte
	binary.LittleEndian.PutUint32(buf[:], n)
	_, err := w.Write(buf[:])
	return err
}
func encWriteI32(w io.Writer, n int32) error { return encWriteU32(w, uint32(n)) }
func encWriteU64(w io.Writer, n uint64) error {
	var buf [8]byte
	binary.LittleEndian.PutUint64(buf[:], n)
	_, err := w.Write(buf[:])
	return err
}
func encWriteI64(w io.Writer, n int64) error { return encWriteU64(w, uint64(n)) }
func encWriteString(w io.Writer, s string) error {
	if err := encWriteU32(w, uint32(len(s))); err != nil {
		return err
	}
	_, err := io.WriteString(w, s)
	return err
}
func encWriteBool(w io.Writer, b bool) error {
	if b {
		return encWriteU8(w, 1)
	}
	return encWriteU8(w, 0)
}

// record writes a tagged record (opcode + u32LE len + payload) to w.
func encRecord(w io.Writer, op byte, payload []byte) error {
	if err := encWriteU8(w, op); err != nil {
		return err
	}
	if err := encWriteU32(w, uint32(len(payload))); err != nil {
		return err
	}
	if len(payload) > 0 {
		_, err := w.Write(payload)
		return err
	}
	return nil
}

// ── Type encoding ─────────────────────────────────────────────────────────────

// EncodeType writes a minir.Type into w using the MTYPE_* opcodes.
func EncodeType(w io.Writer, t Type) error {
	if t == nil {
		return encWriteU8(w, mtypeNil)
	}
	switch v := t.(type) {
	case *PrimitiveType:
		if err := encWriteU8(w, mtypePrim); err != nil {
			return err
		}
		return encWriteString(w, v.Name)

	case *PointerType:
		if err := encWriteU8(w, mtypePtr); err != nil {
			return err
		}
		return EncodeType(w, v.Elem)

	case *RecordType:
		if err := encWriteU8(w, mtypeRecord); err != nil {
			return err
		}
		if err := encWriteString(w, v.TypeName); err != nil {
			return err
		}
		if err := encWriteU32(w, uint32(len(v.Fields))); err != nil {
			return err
		}
		for _, f := range v.Fields {
			if err := encWriteString(w, f.Name); err != nil {
				return err
			}
			if err := encWriteI32(w, int32(f.Offset)); err != nil {
				return err
			}
			if err := EncodeType(w, f.Type); err != nil {
				return err
			}
		}
		return nil

	case *ArrayType:
		if err := encWriteU8(w, mtypeArray); err != nil {
			return err
		}
		if err := encWriteI32(w, int32(v.Len)); err != nil {
			return err
		}
		return EncodeType(w, v.Elem)

	case *FunctionType:
		if err := encWriteU8(w, mtypeFunc); err != nil {
			return err
		}
		if err := encWriteU32(w, uint32(len(v.Params))); err != nil {
			return err
		}
		for _, p := range v.Params {
			if err := EncodeType(w, p); err != nil {
				return err
			}
		}
		hasResult := v.Result != nil
		if err := encWriteBool(w, hasResult); err != nil {
			return err
		}
		if hasResult {
			return EncodeType(w, v.Result)
		}
		return nil

	default:
		return encWriteU8(w, mtypeNil)
	}
}

// ── Value encoding ────────────────────────────────────────────────────────────

// EncodeValue writes a minir.Value into w.
func EncodeValue(w io.Writer, v Value) error {
	if v == nil {
		return encWriteU8(w, valNil)
	}
	switch x := v.(type) {
	case *IntegerConst:
		if x.NameStr != "" {
			if err := encWriteU8(w, valConstNamed); err != nil {
				return err
			}
			if err := encWriteString(w, x.NameStr); err != nil {
				return err
			}
			return encWriteI64(w, int64(x.Value))
		}
		if err := encWriteU8(w, valConstInt); err != nil {
			return err
		}
		return encWriteI64(w, int64(x.Value))

	case *FloatConst:
		if x.NameStr != "" {
			if err := encWriteU8(w, valConstNamed); err != nil {
				return err
			}
			if err := encWriteString(w, x.NameStr); err != nil {
				return err
			}
			return encWriteU64(w, math.Float64bits(x.Value))
		}
		if err := encWriteU8(w, valConstFloat); err != nil {
			return err
		}
		return encWriteU64(w, math.Float64bits(x.Value))

	case *NilConst:
		if err := encWriteU8(w, valConstNil); err != nil {
			return err
		}
		if err := encWriteString(w, x.NameStr); err != nil {
			return err
		}
		return EncodeType(w, x.Ty)

	case *StringConst:
		if err := encWriteU8(w, valConstString); err != nil {
			return err
		}
		if err := encWriteString(w, x.NameStr); err != nil {
			return err
		}
		return encWriteString(w, x.Value)

	case *AggregateConst:
		// Best-effort: encode numerically when possible, otherwise write zero.
		if x.NameStr != "" {
			if err := encWriteU8(w, valConstNamed); err != nil {
				return err
			}
			if err := encWriteString(w, x.NameStr); err != nil {
				return err
			}
			return encWriteI64(w, toInt64(x.Val))
		}
		if err := encWriteU8(w, valConstInt); err != nil {
			return err
		}
		return encWriteI64(w, toInt64(x.Val))

	case *Temp:
		if err := encWriteU8(w, valTemp); err != nil {
			return err
		}
		if err := encWriteString(w, x.NameStr); err != nil {
			return err
		}
		if err := encWriteU32(w, uint32(x.ID)); err != nil {
			return err
		}
		if err := encWriteBool(w, x.IsAddr); err != nil {
			return err
		}
		return EncodeType(w, x.Ty)

	case *GlobalRef:
		if err := encWriteU8(w, valGlobalRef); err != nil {
			return err
		}
		if err := encWriteString(w, x.GlobalName); err != nil {
			return err
		}
		return EncodeType(w, x.Ty)

	default:
		return encWriteU8(w, valNil)
	}
}

// encodeTemp is a helper that encodes a *Temp (guaranteed non-nil typed dest).
func encodeTemp(w io.Writer, t *Temp) error {
	hasTemp := t != nil
	if err := encWriteBool(w, hasTemp); err != nil {
		return err
	}
	if hasTemp {
		if err := encWriteString(w, t.NameStr); err != nil {
			return err
		}
		if err := encWriteU32(w, uint32(t.ID)); err != nil {
			return err
		}
		if err := encWriteBool(w, t.IsAddr); err != nil {
			return err
		}
		return EncodeType(w, t.Ty)
	}
	return nil
}

// ── Instruction encoding ──────────────────────────────────────────────────────

// EncodeInstr writes a single instruction to w.
// The format is:  [1] op-byte  then instruction-specific fields.
func EncodeInstr(w io.Writer, ins Instr) error {
	switch v := ins.(type) {
	case *PhiInst:
		if err := encWriteU8(w, opPhi); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Dst); err != nil {
			return err
		}
		if err := encWriteU32(w, uint32(len(v.Args))); err != nil {
			return err
		}
		for _, a := range v.Args {
			if err := encWriteString(w, a.BlockLabel); err != nil {
				return err
			}
			if err := EncodeValue(w, a.Val); err != nil {
				return err
			}
		}

	case *BinaryInst:
		if err := encWriteU8(w, opBin); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Dst); err != nil {
			return err
		}
		if err := encWriteString(w, v.Op); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Left); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Right); err != nil {
			return err
		}

	case *ICmpInst:
		if err := encWriteU8(w, opICmp); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Dst); err != nil {
			return err
		}
		if err := encWriteString(w, v.Pred); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Left); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Right); err != nil {
			return err
		}

	case *FCmpInst:
		if err := encWriteU8(w, opFCmp); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Dst); err != nil {
			return err
		}
		if err := encWriteString(w, v.Pred); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Left); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Right); err != nil {
			return err
		}

	case *LoadInst:
		if err := encWriteU8(w, opLoad); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Dst); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Addr); err != nil {
			return err
		}

	case *StoreInst:
		if err := encWriteU8(w, opStore); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Val); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Addr); err != nil {
			return err
		}

	case *AllocaInst:
		if err := encWriteU8(w, opAlloca); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Dst); err != nil {
			return err
		}
		if err := EncodeType(w, v.AllocType); err != nil {
			return err
		}

	case *GEPInst:
		if err := encWriteU8(w, opGEP); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Dst); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Base); err != nil {
			return err
		}
		if err := EncodeType(w, v.ElemType); err != nil {
			return err
		}
		if err := encWriteU32(w, uint32(len(v.Offsets))); err != nil {
			return err
		}
		for _, off := range v.Offsets {
			if err := encWriteI32(w, int32(off)); err != nil {
				return err
			}
		}
		if err := encWriteU32(w, uint32(len(v.Indices))); err != nil {
			return err
		}
		for _, idx := range v.Indices {
			if err := EncodeValue(w, idx); err != nil {
				return err
			}
		}

	case *CallInst:
		if err := encWriteU8(w, opCall); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Dst); err != nil {
			return err
		}
		if err := encWriteString(w, v.Callee); err != nil {
			return err
		}
		if err := encWriteU32(w, uint32(len(v.Args))); err != nil {
			return err
		}
		for _, a := range v.Args {
			if err := EncodeValue(w, a); err != nil {
				return err
			}
		}

	case *UnaryInst:
		if err := encWriteU8(w, opUnary); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Dst); err != nil {
			return err
		}
		if err := encWriteString(w, v.Op); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Src); err != nil {
			return err
		}

	case *CastInst:
		if err := encWriteU8(w, opCast); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Dst); err != nil {
			return err
		}
		if err := encWriteString(w, v.Op); err != nil {
			return err
		}
		if err := EncodeValue(w, v.Src); err != nil {
			return err
		}

	case *HaltInst:
		if err := encWriteU8(w, opHalt); err != nil {
			return err
		}
		if err := encWriteBool(w, v.Code != nil); err != nil {
			return err
		}
		if v.Code != nil {
			if err := EncodeValue(w, v.Code); err != nil {
				return err
			}
		}

	case *ReturnInst:
		if err := encWriteU8(w, opRet); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Result); err != nil {
			return err
		}

	case *JumpInst:
		if err := encWriteU8(w, opJmp); err != nil {
			return err
		}
		if err := encWriteString(w, v.Target); err != nil {
			return err
		}

	case *CondBrInst:
		if err := encWriteU8(w, opCondBr); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Cond); err != nil {
			return err
		}
		if err := encWriteString(w, v.TrueLabel); err != nil {
			return err
		}
		if err := encWriteString(w, v.FalseLabel); err != nil {
			return err
		}

	case *SwitchInst:
		if err := encWriteU8(w, opSwitch); err != nil {
			return err
		}
		if err := encodeTemp(w, v.Key); err != nil {
			return err
		}
		if err := encWriteString(w, v.Default); err != nil {
			return err
		}
		if err := encWriteU32(w, uint32(len(v.Arms))); err != nil {
			return err
		}
		for _, a := range v.Arms {
			if err := encWriteI32(w, int32(a.Val)); err != nil {
				return err
			}
			if err := encWriteString(w, a.Label); err != nil {
				return err
			}
		}

	default:
		return fmt.Errorf("EncodeInstr: unknown instruction type %T", ins)
	}
	return nil
}

// ── Block encoding ────────────────────────────────────────────────────────────

// EncodeBlock serialises a *Block (without CFG edge data; edges are stored
// separately in the function record).
func EncodeBlock(w io.Writer, b *Block) error {
	if err := encWriteI32(w, int32(b.ID)); err != nil {
		return err
	}
	if err := encWriteString(w, b.Label); err != nil {
		return err
	}

	// instructions (including terminator if not already the last of Instrs)
	instrs := b.Instrs
	termAlreadyIn := false
	if b.Term != nil && len(instrs) > 0 {
		if instrs[len(instrs)-1] == b.Term {
			termAlreadyIn = true
		}
	}
	count := uint32(len(instrs))
	if b.Term != nil && !termAlreadyIn {
		count++
	}
	if err := encWriteU32(w, count); err != nil {
		return err
	}
	for _, ins := range instrs {
		if err := EncodeInstr(w, ins); err != nil {
			return err
		}
	}
	if b.Term != nil && !termAlreadyIn {
		if err := EncodeInstr(w, b.Term); err != nil {
			return err
		}
	}

	// predecessor IDs
	if err := encWriteU32(w, uint32(len(b.PredOrder))); err != nil {
		return err
	}
	for _, id := range b.PredOrder {
		if err := encWriteI32(w, int32(id)); err != nil {
			return err
		}
	}

	// successor IDs
	if err := encWriteU32(w, uint32(len(b.SuccOrder))); err != nil {
		return err
	}
	for _, id := range b.SuccOrder {
		if err := encWriteI32(w, int32(id)); err != nil {
			return err
		}
	}
	return nil
}

// ── Function encoding ────────────────────────────────────────────────────────

// EncodeFunction serialises fn into a REC_FUNC payload written to w.
func EncodeFunction(w io.Writer, fn *Function) error {
	if err := encWriteString(w, fn.FnName); err != nil {
		return err
	}

	// return type
	if err := EncodeType(w, fn.Result); err != nil {
		return err
	}

	// params
	if err := encWriteU32(w, uint32(len(fn.Params))); err != nil {
		return err
	}
	for i, p := range fn.Params {
		if err := encWriteString(w, p.NameStr); err != nil {
			return err
		}
		if err := encWriteU32(w, uint32(p.ID)); err != nil {
			return err
		}
		if err := EncodeType(w, p.Ty); err != nil {
			return err
		}
		if i < len(fn.ParamKinds) {
			if err := encWriteU8(w, byte(fn.ParamKinds[i])); err != nil {
				return err
			}
		} else {
			if err := encWriteU8(w, 0); err != nil {
				return err
			}
		}
	}

	// entry/exit block IDs
	entryID := int32(-1)
	if fn.Entry != nil {
		entryID = int32(fn.Entry.ID)
	}
	exitID := int32(-1)
	if fn.Exit != nil {
		exitID = int32(fn.Exit.ID)
	}
	if err := encWriteI32(w, entryID); err != nil {
		return err
	}
	if err := encWriteI32(w, exitID); err != nil {
		return err
	}

	// blocks in ascending ID order
	if err := encWriteU32(w, uint32(len(fn.Blocks))); err != nil {
		return err
	}
	for _, b := range fn.SortedBlocks() {
		if err := EncodeBlock(w, b); err != nil {
			return err
		}
	}
	return nil
}

// ── Module-level records ──────────────────────────────────────────────────────

// EncodeExternalFunc writes the payload bytes for a REC_EXTERN / TagMirExtern
// record into w. Exported so cache/encode.go can write per-entity records.
func EncodeExternalFunc(w io.Writer, ef *ExternalFunc) error {
	return encodeExternalFunc(w, ef)
}

// encodeExternalFunc builds the payload for a REC_EXTERN record.
func encodeExternalFunc(w io.Writer, ef *ExternalFunc) error {
	if err := encWriteString(w, ef.Name); err != nil {
		return err
	}
	if err := encWriteU8(w, byte(ef.Linkage)); err != nil {
		return err
	}

	// signature
	if ef.Sig == nil {
		if err := encWriteU32(w, 0); err != nil {
			return err
		}
		if err := encWriteBool(w, false); err != nil {
			return err
		}
		if err := encWriteU8(w, mtypeNil); err != nil {
			return err
		}
	} else {
		if err := encWriteU32(w, uint32(len(ef.Sig.Params))); err != nil {
			return err
		}
		for _, p := range ef.Sig.Params {
			if err := EncodeType(w, p); err != nil {
				return err
			}
		}
		hasResult := ef.Sig.Result != nil
		if err := encWriteBool(w, hasResult); err != nil {
			return err
		}
		if hasResult {
			if err := EncodeType(w, ef.Sig.Result); err != nil {
				return err
			}
		}
	}

	// ExternalAttrs
	hasAttrs := ef.Attrs != nil
	if err := encWriteBool(w, hasAttrs); err != nil {
		return err
	}
	if hasAttrs {
		if err := encWriteString(w, ef.Attrs.CName); err != nil {
			return err
		}
		if err := encWriteString(w, ef.Attrs.DLLName); err != nil {
			return err
		}
		if err := encWriteBool(w, ef.Attrs.Variadic); err != nil {
			return err
		}
		if err := encWriteString(w, ef.Attrs.CallConv); err != nil {
			return err
		}
	}
	return nil
}

// EncodeGlobalVar writes the payload bytes for a REC_GLOBAL / TagMirGlobal
// record into w. Exported so cache/encode.go can write per-entity records.
func EncodeGlobalVar(w io.Writer, gv *GlobalVar) error {
	return encodeGlobalVar(w, gv)
}

// encodeGlobalVar builds the payload for a REC_GLOBAL record.
func encodeGlobalVar(w io.Writer, gv *GlobalVar) error {
	if err := encWriteString(w, gv.Name); err != nil {
		return err
	}
	if err := encWriteU8(w, byte(gv.Linkage)); err != nil {
		return err
	}
	if err := EncodeType(w, gv.Ty); err != nil {
		return err
	}
	hasInit := gv.Init != nil
	if err := encWriteBool(w, hasInit); err != nil {
		return err
	}
	if hasInit {
		if err := EncodeValue(w, gv.Init); err != nil {
			return err
		}
	}
	return nil
}

// EncodeGlobalConst writes the payload bytes for a REC_CONST / TagMirConst
// record into w. Exported so cache/encode.go can write per-entity records.
func EncodeGlobalConst(w io.Writer, gc *GlobalConst) error {
	return encodeGlobalConst(w, gc)
}

// encodeGlobalConst builds the payload for a REC_CONST record.
func encodeGlobalConst(w io.Writer, gc *GlobalConst) error {
	if err := encWriteString(w, gc.Name); err != nil {
		return err
	}
	if err := encWriteU8(w, byte(gc.Linkage)); err != nil {
		return err
	}
	if err := EncodeType(w, gc.Ty); err != nil {
		return err
	}
	return EncodeValue(w, gc.Init)
}

// ── EncodeModule ─────────────────────────────────────────────────────────────

// EncodeModule writes the full binary serialisation of m to w.
// Each top-level entity is written as an opcode-prefixed record.
func EncodeModule(w io.Writer, m *Module) error {
	for _, gv := range m.Globals {
		var buf bytes.Buffer
		if err := encodeGlobalVar(&buf, gv); err != nil {
			return fmt.Errorf("encode global %s: %w", gv.Name, err)
		}
		if err := encRecord(w, recGlobal, buf.Bytes()); err != nil {
			return err
		}
	}
	for _, gc := range m.Constants {
		var buf bytes.Buffer
		if err := encodeGlobalConst(&buf, gc); err != nil {
			return fmt.Errorf("encode const %s: %w", gc.Name, err)
		}
		if err := encRecord(w, recConst, buf.Bytes()); err != nil {
			return err
		}
	}
	for _, ef := range m.Externals {
		var buf bytes.Buffer
		if err := encodeExternalFunc(&buf, ef); err != nil {
			return fmt.Errorf("encode extern %s: %w", ef.Name, err)
		}
		if err := encRecord(w, recExtern, buf.Bytes()); err != nil {
			return err
		}
	}
	for _, fn := range m.Functions {
		var buf bytes.Buffer
		if err := EncodeFunction(&buf, fn); err != nil {
			return fmt.Errorf("encode func %s: %w", fn.FnName, err)
		}
		if err := encRecord(w, recFunc, buf.Bytes()); err != nil {
			return err
		}
	}
	return nil
}

// ModuleBytes is a convenience that encodes m into a []byte.
func ModuleBytes(m *Module) ([]byte, error) {
	var buf bytes.Buffer
	if err := EncodeModule(&buf, m); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

