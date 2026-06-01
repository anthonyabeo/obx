package minir

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// Type/constant lowering and RTTI helpers live in this file, so lower.go can
// focus on control-flow and statement lowering.

// countOpenDims walks a chain of *types.ArrayType nodes that begin with an
// open dimension (Length == -1), counting how many consecutive open dimensions
// exist.  It returns (count, innerElem) where innerElem is the first sema
// type in the chain that is NOT an open array — either a fixed-length
// ArrayType, a scalar, or a record.
//
// Examples:
//
//	ARRAY OF INTEGER           → (1, INTEGER)
//	ARRAY OF ARRAY OF INTEGER  → (2, INTEGER)
//	ARRAY 10 OF ARRAY OF T     → caller never reaches here (not open)
func countOpenDims(at *types.ArrayType) (int, types.Type) {
	n := 0
	var cur types.Type = at
	for {
		arr, ok := cur.(*types.ArrayType)
		if !ok || !arr.IsOpen() {
			break
		}
		n++
		cur = arr.Elem
	}
	return n, cur
}

// currentModule is a package-level hook used by LowerType to register
// module-scoped constants (vtable arrays, RTTI PODs) while lowering a module.
// It is set by Lower() for the module currently being processed and cleared
// afterward.
var currentModule *Module

// RTTI ID assignment state.
var (
	nextRTTIID uint64 = 1
	rttiID            = map[string]uint64{}
)

// Offsets (in bytes) within the RTTI POD for inline loads. We choose a stable
// layout: ID(uint64) at offset 0, Base pointer at offset 8, VTable pointer at
// offset 16, Name pointer at offset 24, Size(uint64) at offset 32.
const (
	rttiIDOff      = 0
	rttiBasePtrOff = 8
	rttiVTableOff  = 16
	rttiNameOff    = 24
	rttiSizeOff    = 32
)

// rttiPODRecordType returns a RecordType descriptor matching the RTTI POD
// layout: { ID uint64, Base *i32, VTable *i32, Name *i32, Size uint64 }.
func rttiPODRecordType() *RecordType {
	return NewRecordType("", []RecordField{
		{Name: "ID", Type: I64(), Offset: rttiIDOff},
		{Name: "Base", Type: Ptr(I32()), Offset: rttiBasePtrOff},
		{Name: "VTable", Type: Ptr(I32()), Offset: rttiVTableOff},
		{Name: "Name", Type: Ptr(I32()), Offset: rttiNameOff},
		{Name: "Size", Type: I64(), Offset: rttiSizeOff},
	})
}

// rttiNameForType extracts the RTTI symbol name from a sema type, unwrapping
// NamedType → RecordType (and optionally PointerType base) if needed.
func rttiNameForType(ty types.Type) string {
	switch tt := ty.(type) {
	case *types.NamedType:
		if rec, ok := tt.Def.(*types.RecordType); ok && rec.Layout != nil {
			return rec.Layout.RTTIName
		}
	case *types.RecordType:
		if tt.Layout != nil {
			return tt.Layout.RTTIName
		}
	case *types.PointerType:
		return rttiNameForType(tt.Base)
	}
	return ""
}

// lowerConstant evaluates a literal expression to a Constant without requiring
// an active basic block. Returns nil for non-literal expressions.
func lowerConstant(expr desugar.Expr) Constant {
	lit, ok := expr.(*desugar.Literal)
	if !ok {
		return nil
	}

	ty := LowerType(lit.SemaType)
	if ty == nil {
		ty = I32()
	}

	switch lit.Kind {
	case token.BYTE_LIT:
		iv, err := strconv.ParseUint(lit.Value, 10, 8)
		if err != nil {
			return NewConst(lit.Value, lit.Value, ty)
		}
		return NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
	case token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
		v := lit.Value
		var iv int64
		var err error
		if strings.HasSuffix(v, "h") || strings.HasSuffix(v, "H") {
			v2 := v[:len(v)-1]
			iv, err = strconv.ParseInt(v2, 16, 64)
		} else {
			iv, err = strconv.ParseInt(v, 10, 64)
		}
		if err != nil {
			return NewConst(lit.Value, lit.Value, ty)
		}
		return NewConst(fmt.Sprintf("%d", iv), iv, ty)
	case token.REAL_LIT, token.LONGREAL_LIT:
		v := lit.Value
		norm := strings.ReplaceAll(v, "D", "E")
		norm = strings.ReplaceAll(norm, "d", "E")
		norm = strings.ReplaceAll(norm, "S", "E")
		norm = strings.ReplaceAll(norm, "s", "E")
		fv, _ := strconv.ParseFloat(norm, 64)
		name := strconv.FormatFloat(fv, 'g', -1, 64)
		return NewConst(name, fv, ty)
	case token.TRUE:
		return NewConst("true", int64(1), I1())
	case token.FALSE:
		return NewConst("false", int64(0), I1())
	case token.CHAR_LIT:
		v := lit.Value
		var iv uint64
		var err error
		if strings.HasSuffix(v, "x") || strings.HasSuffix(v, "X") {
			v2 := v[:len(v)-1]
			iv, err = strconv.ParseUint(v2, 16, 8)
		} else {
			iv, err = strconv.ParseUint(v, 16, 8)
		}
		if err != nil {
			return NewConst(lit.Value, lit.Value, ty)
		}
		return NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
	case token.WCHAR_LIT:
		v := lit.Value
		var iv uint64
		var err error
		if strings.HasSuffix(v, "x") || strings.HasSuffix(v, "X") {
			v2 := v[:len(v)-1]
			iv, err = strconv.ParseUint(v2, 16, 16)
		} else {
			iv, err = strconv.ParseUint(v, 16, 16)
		}
		if err != nil {
			return NewConst(lit.Value, lit.Value, ty)
		}
		return NewConst(fmt.Sprintf("%d", iv), int64(iv), ty)
	case token.STR_LIT, token.HEX_STR_LIT:
		return NewConst(lit.Value, lit.Value, NewArrayType(len(lit.Value)+1, U16()))
	case token.NIL:
		return NewConst("nil", int64(0), Ptr(Void()))
	default:
		iv, err := strconv.ParseInt(lit.Value, 10, 64)
		if err == nil {
			return NewConst(lit.Value, iv, ty)
		}
		return NewConst(lit.Value, lit.Value, ty)
	}
}

// LowerType maps a sema/types.Type to a Type.
// Exported so callers can use it without constructing a Lowerer.
func LowerType(ty types.Type) Type {
	if ty == nil {
		return nil
	}

	switch t := ty.(type) {
	case *types.BasicType:
		switch t.Kind {
		case types.BOOLEAN:
			return I1()
		case types.BYTE, types.CHAR:
			return U8()
		case types.INT8:
			return I8()
		case types.INT16, types.SHORTINT:
			return I16()
		case types.INT32, types.INTEGER:
			return I32()
		case types.WCHAR:
			return U16()
		case types.SET:
			return U32()
		case types.INT64, types.LONGINT:
			return I64()
		case types.REAL:
			return F32()
		case types.LONGREAL:
			return F64()
		case types.VOID:
			return Void()
		case types.NIL, types.UNKNOWN:
			return nil
		default:
			return nil
		}
	case *types.PointerType:
		base := LowerType(t.Base)
		if base == nil {
			return Ptr(Void())
		}
		return Ptr(base)
	case *types.CPointerType:
		base := LowerType(t.Base)
		if base == nil {
			return Ptr(Void())
		}
		return Ptr(base)
	case *types.ArrayType:
		// Count consecutive open (Length == -1) dimensions from this node.
		// Fixed dimensions are encoded as a wrapping ArrayType; only open
		// ones get a dope-vector header slot.
		if t.IsOpen() {
			ndims, innerSema := countOpenDims(t)
			return NewOpenArrayType(ndims, LowerType(innerSema))
		}
		return NewArrayType(t.Length, LowerType(t.Elem))
	case *types.CStructType:
		// CSTRUCT types (FFI-bound C structs) lower identically to RecordType:
		// compute aligned byte offsets using the C-struct width/alignment rules.
		var fields []RecordField
		offset := 0
		for _, f := range t.Fields {
			ft := LowerType(f.Type)
			// Align field to its natural alignment before assigning offset.
			align := 1
			if f.Type != nil {
				if a := f.Type.Alignment(); a > 0 {
					align = a
				}
			}
			if align > 1 {
				offset = (offset + align - 1) / align * align
			}
			fields = append(fields, RecordField{Name: f.Name, Type: ft, Offset: offset})
			if f.Type != nil {
				fw := f.Type.Width()
				if fw > 0 {
					offset += fw
				}
			}
		}
		return NewRecordType("cstruct", fields)
	case *types.CUnionType:
		// CUNION: represent as a struct whose sole field is the widest member.
		// For simplicity, just use a struct with the full union width as a byte array.
		total := t.Width()
		if total <= 0 {
			total = 8
		}
		// Materialise as a single opaque byte-array field.
		arrTy := NewArrayType(total, I8())
		return NewRecordType("cunion", []RecordField{{Name: "_data", Type: arrTy, Offset: 0}})
	case *types.CArrayType:
		if t.Elem == nil {
			return nil
		}
		if t.Length < 0 {
			// Open CARRAY: treat as pointer to element.
			return Ptr(LowerType(t.Elem))
		}
		return NewArrayType(t.Length, LowerType(t.Elem))
	case *types.RecordType:
		var fields []RecordField
		offset := 0
		for _, f := range t.Fields {
			ft := LowerType(f.Type)
			fields = append(fields, RecordField{Name: f.Name, Type: ft, Offset: offset})
			offset += f.Type.Width()
		}

		rec := NewRecordType("", fields)

		if t.Layout != nil && currentModule != nil {
			if t.Layout.VTableName != "" {
				vtabLen := len(t.Layout.VTable)
				vtabTyp := NewArrayType(vtabLen, I32())
				vals := make([]uint32, 0, vtabLen)
				for _, ms := range t.Layout.VTable {
					vals = append(vals, uint32(ms.FuncIndex))
				}
				vtabConst := NewConst(t.Layout.VTableName, vals, vtabTyp)
				gv := &GlobalConst{Name: t.Layout.VTableName, Ty: vtabTyp, Init: vtabConst, Linkage: InternalLinkage}
				currentModule.Constants = append(currentModule.Constants, gv)
				_ = currentModule.SymTab.Define(t.Layout.VTableName, gv.Ref())
			}

			if t.Layout.RTTIName != "" {
				nameSym := t.Layout.RTTIName + "_name"
				nameConst := NewConst(nameSym, t.String(), NewArrayType(len(t.String())+1, I32()))
				nameGV := &GlobalConst{Name: nameSym, Ty: nameConst.Type(), Init: nameConst, Linkage: PrivateLinkage}
				currentModule.Constants = append(currentModule.Constants, nameGV)
				_ = currentModule.SymTab.Define(nameSym, nameGV.Ref())

				id := rttiID[t.Layout.RTTIName]
				if id == 0 {
					id = nextRTTIID
					nextRTTIID++
					rttiID[t.Layout.RTTIName] = id
				}

				baseSym := ""
				if t.Base != nil && t.Base.Layout != nil {
					baseSym = t.Base.Layout.RTTIName
				}

				rttiVal := struct {
					ID     uint64
					Base   string
					VTable string
					Name   string
					Size   uint64
				}{
					ID:     id,
					Base:   baseSym,
					VTable: t.Layout.VTableName,
					Name:   nameSym,
					Size:   uint64(t.Layout.Size),
				}
				rttiConst := NewConst(t.Layout.RTTIName, rttiVal, Ptr(I32()))
				rttiGV := &GlobalConst{Name: t.Layout.RTTIName, Ty: Ptr(I32()), Init: rttiConst, Linkage: InternalLinkage}
				currentModule.Constants = append(currentModule.Constants, rttiGV)
				_ = currentModule.SymTab.Define(t.Layout.RTTIName, rttiGV.Ref())
			}
		}

		return rec
	case *types.NamedType:
		inner := LowerType(t.Def)
		if rec, ok := inner.(*RecordType); ok && rec.TypeName == "" {
			return NewRecordType(t.Name, rec.Fields)
		}
		return inner
	case *types.ProcedureType:
		var params []Type
		for _, p := range t.Params {
			params = append(params, LowerType(p.Type))
		}
		return &FunctionType{Params: params, Result: LowerType(t.Result)}
	case *types.StringType:
		return NewArrayType(t.Length+1, I32())
	case *types.EnumType:
		return U32()
	default:
		return nil
	}
}
