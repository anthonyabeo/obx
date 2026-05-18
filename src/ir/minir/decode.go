package minir

// decode.go — binary deserialization of a minir.Module.
// Mirrors the format defined in encode.go.

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	"math"

	"github.com/anthonyabeo/obx/src/ir/desugar"
)

// ── helpers ───────────────────────────────────────────────────────────────────

func decReadU8(r io.Reader) (byte, error) {
	var buf [1]byte
	if _, err := io.ReadFull(r, buf[:]); err != nil {
		return 0, err
	}
	return buf[0], nil
}
func decReadU32(r io.Reader) (uint32, error) {
	var buf [4]byte
	if _, err := io.ReadFull(r, buf[:]); err != nil {
		return 0, err
	}
	return binary.LittleEndian.Uint32(buf[:]), nil
}
func decReadI32(r io.Reader) (int32, error) {
	n, err := decReadU32(r)
	return int32(n), err
}
func decReadU64(r io.Reader) (uint64, error) {
	var buf [8]byte
	if _, err := io.ReadFull(r, buf[:]); err != nil {
		return 0, err
	}
	return binary.LittleEndian.Uint64(buf[:]), nil
}
func decReadI64(r io.Reader) (int64, error) {
	n, err := decReadU64(r)
	return int64(n), err
}
func decReadString(r io.Reader) (string, error) {
	n, err := decReadU32(r)
	if err != nil {
		return "", err
	}
	if n == 0 {
		return "", nil
	}
	buf := make([]byte, n)
	if _, err := io.ReadFull(r, buf); err != nil {
		return "", err
	}
	return string(buf), nil
}
func decReadBool(r io.Reader) (bool, error) {
	b, err := decReadU8(r)
	return b != 0, err
}

// ── Type decoding ─────────────────────────────────────────────────────────────

// DecodeType reads a minir.Type from r.
func DecodeType(r io.Reader) (Type, error) {
	tag, err := decReadU8(r)
	if err != nil {
		return nil, fmt.Errorf("decode minir type tag: %w", err)
	}
	switch tag {
	case mtypePrim:
		name, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		return NormalizeType(&PrimitiveType{Name: name}), nil

	case mtypePtr:
		elem, err := DecodeType(r)
		if err != nil {
			return nil, err
		}
		return &PointerType{Elem: elem}, nil

	case mtypeRecord:
		typeName, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		nFields, err := decReadU32(r)
		if err != nil {
			return nil, err
		}
		fields := make([]RecordField, nFields)
		for i := range fields {
			name, err := decReadString(r)
			if err != nil {
				return nil, err
			}
			offset, err := decReadI32(r)
			if err != nil {
				return nil, err
			}
			ft, err := DecodeType(r)
			if err != nil {
				return nil, err
			}
			fields[i] = RecordField{Name: name, Offset: int(offset), Type: ft}
		}
		return NewRecordType(typeName, fields), nil

	case mtypeArray:
		length, err := decReadI32(r)
		if err != nil {
			return nil, err
		}
		elem, err := DecodeType(r)
		if err != nil {
			return nil, err
		}
		return NewArrayType(int(length), elem), nil

	case mtypeFunc:
		nParams, err := decReadU32(r)
		if err != nil {
			return nil, err
		}
		params := make([]Type, nParams)
		for i := range params {
			p, err := DecodeType(r)
			if err != nil {
				return nil, err
			}
			params[i] = p
		}
		hasResult, err := decReadBool(r)
		if err != nil {
			return nil, err
		}
		var result Type
		if hasResult {
			result, err = DecodeType(r)
			if err != nil {
				return nil, err
			}
		}
		return &FunctionType{Params: params, Result: result}, nil

	case mtypeNil:
		return nil, nil

	default:
		return nil, fmt.Errorf("DecodeType: unknown minir type tag 0x%02X", tag)
	}
}

// ── Value decoding ────────────────────────────────────────────────────────────

// DecodeValue reads a minir.Value from r.
func DecodeValue(r io.Reader) (Value, error) {
	kind, err := decReadU8(r)
	if err != nil {
		return nil, fmt.Errorf("decode value kind: %w", err)
	}
	switch kind {
	case valNil:
		return nil, nil

	case valConstInt:
		v, err := decReadI64(r)
		if err != nil {
			return nil, err
		}
		return NewConst("", int64(v), I64()), nil

	case valConstFloat:
		bits, err := decReadU64(r)
		if err != nil {
			return nil, err
		}
		return NewConst("", math.Float64frombits(bits), F64()), nil

	case valConstNamed:
		name, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		v, err := decReadI64(r)
		if err != nil {
			return nil, err
		}
		return NewConst(name, int64(v), I64()), nil

	case valConstString:
		name, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		val, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		return ConstString(name, val), nil

	case valConstNil:
		name, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		ty, err := DecodeType(r)
		if err != nil {
			return nil, err
		}
		return ConstNil(name, ty), nil

	case valTemp:
		nameStr, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		id, err := decReadU32(r)
		if err != nil {
			return nil, err
		}
		isAddr, err := decReadBool(r)
		if err != nil {
			return nil, err
		}
		ty, err := DecodeType(r)
		if err != nil {
			return nil, err
		}
		t := &Temp{
			ID:      int(id),
			NameStr: nameStr,
			IsAddr:  isAddr,
			Ty:      ty,
		}
		return t, nil

	case valGlobalRef:
		name, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		ty, err := DecodeType(r)
		if err != nil {
			return nil, err
		}
		ptrTy, _ := ty.(*PointerType)
		return &GlobalRef{GlobalName: name, Ty: ptrTy}, nil

	default:
		return nil, fmt.Errorf("DecodeValue: unknown value kind 0x%02X", kind)
	}
}

// decodeTemp reads an optional *Temp (hasDst bool prefix).
func decodeTemp(r io.Reader) (*Temp, error) {
	has, err := decReadBool(r)
	if err != nil {
		return nil, err
	}
	if !has {
		return nil, nil
	}
	nameStr, err := decReadString(r)
	if err != nil {
		return nil, err
	}
	id, err := decReadU32(r)
	if err != nil {
		return nil, err
	}
	isAddr, err := decReadBool(r)
	if err != nil {
		return nil, err
	}
	ty, err := DecodeType(r)
	if err != nil {
		return nil, err
	}
	return &Temp{ID: int(id), NameStr: nameStr, IsAddr: isAddr, Ty: ty}, nil
}

// ── Instruction decoding ──────────────────────────────────────────────────────

// DecodeInstr reads a single instruction from r (the opcode byte is read first).
func DecodeInstr(r io.Reader) (Instr, error) {
	op, err := decReadU8(r)
	if err != nil {
		return nil, fmt.Errorf("decode instr op: %w", err)
	}
	switch op {
	case opPhi:
		dst, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		n, err := decReadU32(r)
		if err != nil {
			return nil, err
		}
		args := make([]PhiArm, n)
		for i := range args {
			label, err := decReadString(r)
			if err != nil {
				return nil, err
			}
			val, err := DecodeValue(r)
			if err != nil {
				return nil, err
			}
			args[i] = PhiArm{BlockLabel: label, Val: val}
		}
		return &PhiInst{Dst: dst, Args: args}, nil

	case opBin:
		dst, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		op, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		left, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		right, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		return &BinaryInst{Dst: dst, Op: op, Left: left, Right: right}, nil

	case opICmp:
		dst, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		pred, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		left, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		right, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		return &ICmpInst{Dst: dst, Pred: pred, Left: left, Right: right}, nil

	case opFCmp:
		dst, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		pred, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		left, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		right, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		inner := ICmpInst{Dst: dst, Pred: pred, Left: left, Right: right}
		return &FCmpInst{ICmpInst: inner}, nil

	case opLoad:
		dst, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		addr, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		return &LoadInst{Dst: dst, Addr: addr}, nil

	case opStore:
		val, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		addr, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		return &StoreInst{Val: val, Addr: addr}, nil

	case opAlloca:
		dst, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		allocType, err := DecodeType(r)
		if err != nil {
			return nil, err
		}
		return &AllocaInst{Dst: dst, AllocType: allocType}, nil

	case opGEP:
		dst, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		base, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		elemType, err := DecodeType(r)
		if err != nil {
			return nil, err
		}
		nOff, err := decReadU32(r)
		if err != nil {
			return nil, err
		}
		offsets := make([]int, nOff)
		for i := range offsets {
			o, err := decReadI32(r)
			if err != nil {
				return nil, err
			}
			offsets[i] = int(o)
		}
		nIdx, err := decReadU32(r)
		if err != nil {
			return nil, err
		}
		indices := make([]Value, nIdx)
		for i := range indices {
			v, err := DecodeValue(r)
			if err != nil {
				return nil, err
			}
			indices[i] = v
		}
		return &GEPInst{Dst: dst, Base: base, ElemType: elemType, Offsets: offsets, Indices: indices}, nil

	case opCall:
		dst, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		callee, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		nArgs, err := decReadU32(r)
		if err != nil {
			return nil, err
		}
		args := make([]Value, nArgs)
		for i := range args {
			a, err := DecodeValue(r)
			if err != nil {
				return nil, err
			}
			args[i] = a
		}
		return &CallInst{Dst: dst, Callee: callee, Args: args}, nil

	case opUnary:
		dst, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		uop, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		src, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		return &UnaryInst{Dst: dst, Op: uop, Src: src}, nil

	case opCast:
		dst, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		cop, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		src, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		return &CastInst{Dst: dst, Op: cop, Src: src}, nil

	case opHalt:
		hasCode, err := decReadBool(r)
		if err != nil {
			return nil, err
		}
		var code Value
		if hasCode {
			code, err = DecodeValue(r)
			if err != nil {
				return nil, err
			}
		}
		return &HaltInst{Code: code}, nil

	case opRet:
		result, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		return &ReturnInst{Result: result}, nil

	case opJmp:
		target, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		return &JumpInst{Target: target}, nil

	case opCondBr:
		cond, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		trueLabel, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		falseLabel, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		return &CondBrInst{Cond: cond, TrueLabel: trueLabel, FalseLabel: falseLabel}, nil

	case opSwitch:
		key, err := decodeTemp(r)
		if err != nil {
			return nil, err
		}
		def, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		nArms, err := decReadU32(r)
		if err != nil {
			return nil, err
		}
		arms := make([]SwitchArm, nArms)
		for i := range arms {
			val, err := decReadI32(r)
			if err != nil {
				return nil, err
			}
			label, err := decReadString(r)
			if err != nil {
				return nil, err
			}
			arms[i] = SwitchArm{Val: int(val), Label: label}
		}
		return &SwitchInst{Key: key, Default: def, Arms: arms}, nil

	default:
		return nil, fmt.Errorf("DecodeInstr: unknown opcode 0x%02X", op)
	}
}

// ── Block decoding ────────────────────────────────────────────────────────────

// DecodeBlock reads and reconstructs a *Block from r.
func DecodeBlock(r io.Reader) (*Block, error) {
	id, err := decReadI32(r)
	if err != nil {
		return nil, err
	}
	label, err := decReadString(r)
	if err != nil {
		return nil, err
	}
	nInstrs, err := decReadU32(r)
	if err != nil {
		return nil, err
	}
	b := &Block{
		ID:    int(id),
		Label: label,
		Preds: make(map[int]*Block),
		Succs: make(map[int]*Block),
	}
	for i := uint32(0); i < nInstrs; i++ {
		ins, err := DecodeInstr(r)
		if err != nil {
			return nil, fmt.Errorf("block %s instr %d: %w", label, i, err)
		}
		b.Instrs = append(b.Instrs, ins)
		if t, ok := ins.(Terminator); ok {
			b.Term = t
		}
	}

	// pred ID list (blocks resolved later)
	nPreds, err := decReadU32(r)
	if err != nil {
		return nil, err
	}
	b.PredOrder = make([]int, nPreds)
	for i := range b.PredOrder {
		pid, err := decReadI32(r)
		if err != nil {
			return nil, err
		}
		b.PredOrder[i] = int(pid)
	}

	// succ ID list
	nSuccs, err := decReadU32(r)
	if err != nil {
		return nil, err
	}
	b.SuccOrder = make([]int, nSuccs)
	for i := range b.SuccOrder {
		sid, err := decReadI32(r)
		if err != nil {
			return nil, err
		}
		b.SuccOrder[i] = int(sid)
	}
	return b, nil
}

// ── Function decoding ────────────────────────────────────────────────────────

// DecodeFunction reads and reconstructs a *Function from r.
func DecodeFunction(r io.Reader) (*Function, error) {
	name, err := decReadString(r)
	if err != nil {
		return nil, err
	}
	result, err := DecodeType(r)
	if err != nil {
		return nil, err
	}

	nParams, err := decReadU32(r)
	if err != nil {
		return nil, err
	}
	params := make([]*Temp, nParams)
	paramKinds := make([]desugar.ParamKind, nParams)
	for i := range params {
		pName, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		pID, err := decReadU32(r)
		if err != nil {
			return nil, err
		}
		pTy, err := DecodeType(r)
		if err != nil {
			return nil, err
		}
		kindByte, err := decReadU8(r)
		if err != nil {
			return nil, err
		}
		params[i] = &Temp{ID: int(pID), NameStr: pName, Ty: pTy}
		paramKinds[i] = desugar.ParamKind(kindByte)
	}

	entryID, err := decReadI32(r)
	if err != nil {
		return nil, err
	}
	exitID, err := decReadI32(r)
	if err != nil {
		return nil, err
	}

	nBlocks, err := decReadU32(r)
	if err != nil {
		return nil, err
	}
	blockMap := make(map[int]*Block, nBlocks)
	for i := uint32(0); i < nBlocks; i++ {
		b, err := DecodeBlock(r)
		if err != nil {
			return nil, fmt.Errorf("func %s block %d: %w", name, i, err)
		}
		blockMap[b.ID] = b
	}

	// Wire up pred/succ pointers from the stored ID lists.
	for _, b := range blockMap {
		for _, pid := range b.PredOrder {
			if pred, ok := blockMap[pid]; ok {
				b.Preds[pid] = pred
			}
		}
		for _, sid := range b.SuccOrder {
			if succ, ok := blockMap[sid]; ok {
				b.Succs[sid] = succ
			}
		}
	}

	fn := &Function{
		FnName:     name,
		Result:     result,
		Params:     params,
		ParamKinds: paramKinds,
		Blocks:     blockMap,
	}
	if entryID >= 0 {
		fn.Entry = blockMap[int(entryID)]
	}
	if exitID >= 0 {
		fn.Exit = blockMap[int(exitID)]
	}
	return fn, nil
}

// ── Module-level record decoders ──────────────────────────────────────────────

func decodeExternalFunc(payload []byte) (*ExternalFunc, error) {
	r := bytes.NewReader(payload)

	name, err := decReadString(r)
	if err != nil {
		return nil, err
	}
	linkageByte, err := decReadU8(r)
	if err != nil {
		return nil, err
	}

	// signature
	nParams, err := decReadU32(r)
	if err != nil {
		return nil, err
	}
	params := make([]Type, nParams)
	for i := range params {
		p, err := DecodeType(r)
		if err != nil {
			return nil, err
		}
		params[i] = p
	}
	hasResult, err := decReadBool(r)
	if err != nil {
		return nil, err
	}
	var result Type
	if hasResult {
		result, err = DecodeType(r)
		if err != nil {
			return nil, err
		}
	}

	ef := &ExternalFunc{
		Name:    name,
		Linkage: Linkage(linkageByte),
		Sig:     &FunctionType{Params: params, Result: result},
	}

	// ExternalAttrs
	hasAttrs, err := decReadBool(r)
	if err != nil {
		return nil, err
	}
	if hasAttrs {
		cname, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		dll, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		variadic, err := decReadBool(r)
		if err != nil {
			return nil, err
		}
		callconv, err := decReadString(r)
		if err != nil {
			return nil, err
		}
		ef.Attrs = &ExternalAttrs{
			CName:    cname,
			DLLName:  dll,
			Variadic: variadic,
			CallConv: callconv,
		}
	}
	return ef, nil
}

func decodeGlobalVar(payload []byte) (*GlobalVar, error) {
	r := bytes.NewReader(payload)
	name, err := decReadString(r)
	if err != nil {
		return nil, err
	}
	linkageByte, err := decReadU8(r)
	if err != nil {
		return nil, err
	}
	ty, err := DecodeType(r)
	if err != nil {
		return nil, err
	}
	hasInit, err := decReadBool(r)
	if err != nil {
		return nil, err
	}
	gv := &GlobalVar{Name: name, Linkage: Linkage(linkageByte), Ty: ty}
	if hasInit {
		init, err := DecodeValue(r)
		if err != nil {
			return nil, err
		}
		if c, ok := init.(Constant); ok {
			gv.Init = c
		}
	}
	return gv, nil
}

func decodeGlobalConst(payload []byte) (*GlobalConst, error) {
	r := bytes.NewReader(payload)
	name, err := decReadString(r)
	if err != nil {
		return nil, err
	}
	linkageByte, err := decReadU8(r)
	if err != nil {
		return nil, err
	}
	ty, err := DecodeType(r)
	if err != nil {
		return nil, err
	}
	init, err := DecodeValue(r)
	if err != nil {
		return nil, err
	}
	gc := &GlobalConst{Name: name, Linkage: Linkage(linkageByte), Ty: ty}
	if c, ok := init.(Constant); ok {
		gc.Init = c
	}
	return gc, nil
}

// ── DecodeModule ─────────────────────────────────────────────────────────────

// DecodeModule reads a module from an already-verified body reader (no header).
// It reconstructs GlobalVar, GlobalConst, ExternalFunc, and Function entries.
func DecodeModule(name string, body []byte) (*Module, error) {
	m := &Module{Name: name}
	r := bytes.NewReader(body)

	for {
		// read opcode
		op, err := decReadU8(r)
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, fmt.Errorf("decode module record: %w", err)
		}

		// read payload length
		payLen, err := decReadU32(r)
		if err != nil {
			return nil, err
		}
		payload := make([]byte, payLen)
		if payLen > 0 {
			if _, err := io.ReadFull(r, payload); err != nil {
				return nil, fmt.Errorf("decode module payload (op=0x%02X): %w", op, err)
			}
		}

		switch op {
		case recGlobal:
			gv, err := decodeGlobalVar(payload)
			if err != nil {
				return nil, fmt.Errorf("decode GlobalVar: %w", err)
			}
			m.Globals = append(m.Globals, gv)
			_ = m.SymTab.Define(gv.Name, gv.Ref())

		case recConst:
			gc, err := decodeGlobalConst(payload)
			if err != nil {
				return nil, fmt.Errorf("decode GlobalConst: %w", err)
			}
			m.Constants = append(m.Constants, gc)
			if gc.Init != nil {
				_ = m.SymTab.Define(gc.Name, gc.Init)
			}

		case recExtern:
			ef, err := decodeExternalFunc(payload)
			if err != nil {
				return nil, fmt.Errorf("decode ExternalFunc: %w", err)
			}
			m.Externals = append(m.Externals, ef)

		case recFunc:
			fn, err := DecodeFunction(bytes.NewReader(payload))
			if err != nil {
				return nil, fmt.Errorf("decode Function: %w", err)
			}
			m.Functions = append(m.Functions, fn)

		default:
			// unknown record — skip
		}
	}
	return m, nil
}
