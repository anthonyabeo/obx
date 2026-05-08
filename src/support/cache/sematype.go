package cache

// Sema-type binary encoding/decoding helpers.
//
// Sema types (sema/types.Type) are tagged records:
//
//	0x01  STYPE_BASIC   – [1] BasicKind (maps to types.BasicKind)
//	0x02  STYPE_CPTR    – [T] base type
//	0x03  STYPE_NAMED   – [str] name + [T] underlying type
//	0x04  STYPE_ARRAY   – [4] length (i32LE, -1 = open) + [T] elem
//	0x05  STYPE_PROC    – [4] param count + params([str] kind + [T] type) +
//	                      [bool] hasResult + [T?] result
//	0x06  STYPE_VOID    – no payload
//	0x07  STYPE_PTR     – [T] base type  (Oberon POINTER TO)

import (
	"bytes"
	"fmt"
	"io"

	"github.com/anthonyabeo/obx/src/sema/types"
)

// sema type tag constants.
const (
	stypeBasic byte = 0x01
	stypeCptr  byte = 0x02
	stypeNamed byte = 0x03
	stypeArray byte = 0x04
	stypeProc  byte = 0x05
	stypeVoid  byte = 0x06
	stypePtr   byte = 0x07
)

// basicKindWire maps types.BasicKind → wire byte (identity cast is fine since
// BasicKind is iota-based and we control the format).
func basicKindWire(k types.BasicKind) byte { return byte(k) }
func wireBasicKind(b byte) types.BasicKind { return types.BasicKind(b) }

// canonicalBasicType maps a BasicKind to its canonical singleton.
var canonicalBasicType = map[types.BasicKind]*types.BasicType{
	types.BYTE:     types.ByteType,
	types.INT8:     types.Int8Type,
	types.INT16:    types.Int16Type,
	types.INT32:    types.Int32Type,
	types.INT64:    types.Int64Type,
	types.SHORTINT: types.ShortIntType,
	types.INTEGER:  types.IntegerType,
	types.LONGINT:  types.LongIntType,
	types.REAL:     types.RealType,
	types.LONGREAL: types.LongRealType,
	types.BOOLEAN:  types.BooleanType,
	types.CHAR:     types.CharType,
	types.WCHAR:    types.WCharType,
	types.SET:      types.SetType,
	types.NIL:      types.NilType,
	types.VOID:     types.VoidType,
	types.UNKNOWN:  types.UnknownType,
}

// ── encoder ──────────────────────────────────────────────────────────────────

// EncodeSemaType serialises t into w.
func EncodeSemaType(w io.Writer, t types.Type) error {
	if t == nil {
		return WriteU8(w, stypeVoid)
	}
	switch v := t.(type) {
	case *types.BasicType:
		if err := WriteU8(w, stypeBasic); err != nil {
			return err
		}
		return WriteU8(w, basicKindWire(v.Kind))

	case *types.CPointerType:
		if err := WriteU8(w, stypeCptr); err != nil {
			return err
		}
		return EncodeSemaType(w, v.Base)

	case *types.NamedType:
		if err := WriteU8(w, stypeNamed); err != nil {
			return err
		}
		if err := WriteString(w, v.Name); err != nil {
			return err
		}
		return EncodeSemaType(w, v.Def)

	case *types.ArrayType:
		if err := WriteU8(w, stypeArray); err != nil {
			return err
		}
		if err := WriteI32LE(w, int32(v.Length)); err != nil {
			return err
		}
		return EncodeSemaType(w, v.Elem)

	case *types.ProcedureType:
		if err := WriteU8(w, stypeProc); err != nil {
			return err
		}
		if err := WriteU32LE(w, uint32(len(v.Params))); err != nil {
			return err
		}
		for _, p := range v.Params {
			if err := WriteString(w, p.Kind); err != nil {
				return err
			}
			if err := WriteString(w, p.Name); err != nil {
				return err
			}
			if err := EncodeSemaType(w, p.Type); err != nil {
				return err
			}
		}
		hasResult := v.Result != nil
		if err := WriteBool(w, hasResult); err != nil {
			return err
		}
		if hasResult {
			return EncodeSemaType(w, v.Result)
		}
		return nil

	case *types.PointerType:
		if err := WriteU8(w, stypePtr); err != nil {
			return err
		}
		return EncodeSemaType(w, v.Base)

	default:
		// Void / unknown
		return WriteU8(w, stypeVoid)
	}
}

// ── decoder ──────────────────────────────────────────────────────────────────

// DecodeSemaType reads a sema type from r.
func DecodeSemaType(r io.Reader) (types.Type, error) {
	tag, err := ReadU8(r)
	if err != nil {
		return nil, fmt.Errorf("decode sema type tag: %w", err)
	}
	switch tag {
	case stypeBasic:
		kb, err := ReadU8(r)
		if err != nil {
			return nil, err
		}
		k := wireBasicKind(kb)
		if canonical, ok := canonicalBasicType[k]; ok {
			return canonical, nil
		}
		return types.UnknownType, nil

	case stypeCptr:
		base, err := DecodeSemaType(r)
		if err != nil {
			return nil, err
		}
		return &types.CPointerType{Base: base}, nil

	case stypeNamed:
		name, err := ReadString(r)
		if err != nil {
			return nil, err
		}
		underlying, err := DecodeSemaType(r)
		if err != nil {
			return nil, err
		}
		return &types.NamedType{Name: name, Def: underlying}, nil

	case stypeArray:
		length, err := ReadI32LE(r)
		if err != nil {
			return nil, err
		}
		elem, err := DecodeSemaType(r)
		if err != nil {
			return nil, err
		}
		return &types.ArrayType{Length: int(length), Elem: elem}, nil

	case stypeProc:
		paramCount, err := ReadU32LE(r)
		if err != nil {
			return nil, err
		}
		params := make([]*types.FormalParam, paramCount)
		for i := range params {
			kind, err := ReadString(r)
			if err != nil {
				return nil, err
			}
			name, err := ReadString(r)
			if err != nil {
				return nil, err
			}
			ptype, err := DecodeSemaType(r)
			if err != nil {
				return nil, err
			}
			params[i] = &types.FormalParam{Kind: kind, Name: name, Type: ptype}
		}
		hasResult, err := ReadBool(r)
		if err != nil {
			return nil, err
		}
		var result types.Type
		if hasResult {
			result, err = DecodeSemaType(r)
			if err != nil {
				return nil, err
			}
		}
		return &types.ProcedureType{Params: params, Result: result}, nil

	case stypePtr:
		base, err := DecodeSemaType(r)
		if err != nil {
			return nil, err
		}
		return &types.PointerType{Base: base}, nil

	case stypeVoid:
		return types.VoidType, nil

	default:
		return nil, fmt.Errorf("unknown sema type tag 0x%02X", tag)
	}
}

// EncodeSemaTypeBytes is a convenience that serialises a type into a []byte.
func EncodeSemaTypeBytes(t types.Type) ([]byte, error) {
	var buf bytes.Buffer
	if err := EncodeSemaType(&buf, t); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

