package cache

// decode.go — .obxi binary bundle decoder.
//
// LoadBundle reads an .obxi file, verifies the magic and (optionally) the
// source hash, and returns the reconstructed sema LexicalScope and
// minir.Module so the build pipeline can inject them directly without
// re-parsing or re-lowering any Oberon+ source.

import (
	"bytes"
	"crypto/sha256"
	"fmt"
	"io"
	"os"

	"github.com/anthonyabeo/obx/src/ir/minir"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// Bundle is the decoded content of an .obxi file.
type Bundle struct {
	ModuleName string
	Scope      *ast.LexicalScope // sema symbol table for the module
	Module     *minir.Module     // pre-lowered minir module (may be empty)
}

// LoadBundle reads path, checks the magic and optional staleness hash, and
// returns the decoded Bundle.
//
// If srcContent is non-nil the stored 8-byte hash is compared against the
// first 8 bytes of SHA-256(srcContent).  A mismatch returns ErrStale so the
// caller can fall back to full recompilation.
func LoadBundle(path string, srcContent []byte) (*Bundle, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return DecodeBundle(data, srcContent)
}

// ErrStale is returned by LoadBundle when the stored hash no longer matches
// the source .def file.
var ErrStale = fmt.Errorf("cache: .obxi is stale (source hash mismatch)")

// DecodeBundle decodes an in-memory .obxi blob.
func DecodeBundle(data []byte, srcContent []byte) (*Bundle, error) {
	r := bytes.NewReader(data)

	// ── magic ─────────────────────────────────────────────────────────────
	var magic [8]byte
	if _, err := io.ReadFull(r, magic[:]); err != nil {
		return nil, fmt.Errorf("cache: read magic: %w", err)
	}
	if magic != Magic {
		return nil, fmt.Errorf("cache: bad magic %q", magic)
	}

	// ── source hash ───────────────────────────────────────────────────────
	var storedHash [8]byte
	if _, err := io.ReadFull(r, storedHash[:]); err != nil {
		return nil, fmt.Errorf("cache: read hash: %w", err)
	}
	if srcContent != nil {
		sum := sha256.Sum256(srcContent)
		var freshHash [8]byte
		copy(freshHash[:], sum[:8])
		if storedHash != freshHash {
			return nil, ErrStale
		}
	}

	// ── module name ───────────────────────────────────────────────────────
	moduleName, err := ReadString(r)
	if err != nil {
		return nil, fmt.Errorf("cache: read module name: %w", err)
	}

	// ── collect all remaining bytes for records ────────────────────────────
	body, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("cache: read body: %w", err)
	}

	bundle := &Bundle{
		ModuleName: moduleName,
		Scope:      ast.NewLexicalScope(nil, moduleName),
		Module:     &minir.Module{Name: moduleName},
	}

	br := bytes.NewReader(body)
	for {
		tag, length, err := ReadRecordHeader(br)
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, fmt.Errorf("cache: record header: %w", err)
		}
		payload, err := ReadRecordPayload(br, length)
		if err != nil {
			return nil, err
		}

		switch tag {
		case TagSymType:
			sym, err := decodeTypeSymbol(payload)
			if err != nil {
				return nil, fmt.Errorf("cache: decode TypeSymbol: %w", err)
			}
			bundle.Scope.Insert(sym)

		case TagSymVar:
			sym, err := decodeVarSymbol(payload)
			if err != nil {
				return nil, fmt.Errorf("cache: decode VarSymbol: %w", err)
			}
			bundle.Scope.Insert(sym)

		case TagSymProc:
			sym, err := decodeProcSymbol(payload)
			if err != nil {
				return nil, fmt.Errorf("cache: decode ProcSymbol: %w", err)
			}
			bundle.Scope.Insert(sym)

		case TagMirExtern, TagMirGlobal, TagMirConst, TagMirFunc:
			// The payload was encoded by minir's per-entity encoders (EncodeGlobalVar,
			// EncodeExternalFunc, etc.).  To reuse minir.DecodeModule we reconstruct a
			// one-record stream, but we must use the minir opcode (0x20–0x23), not the
			// cache tag (0x10–0x13), because DecodeModule's switch matches the former.
			var mirOp byte
			switch tag {
			case TagMirGlobal:
				mirOp = 0x20 // recGlobal
			case TagMirConst:
				mirOp = 0x21 // recConst
			case TagMirExtern:
				mirOp = 0x22 // recExtern
			case TagMirFunc:
				mirOp = 0x23 // recFunc
			}
			var onRecord bytes.Buffer
			onRecord.WriteByte(mirOp)
			WriteU32LE(&onRecord, uint32(len(payload)))
			onRecord.Write(payload)

			partial, err := minir.DecodeModule(moduleName, onRecord.Bytes())
			if err != nil {
				return nil, fmt.Errorf("cache: decode minir record (tag=0x%02X): %w", tag, err)
			}
			// merge into bundle.Module
			bundle.Module.Globals = append(bundle.Module.Globals, partial.Globals...)
			bundle.Module.Constants = append(bundle.Module.Constants, partial.Constants...)
			bundle.Module.Externals = append(bundle.Module.Externals, partial.Externals...)
			bundle.Module.Functions = append(bundle.Module.Functions, partial.Functions...)

		default:
			// unknown tag – skip (forward-compatibility)
		}
	}

	return bundle, nil
}

// ── sema symbol decoders ──────────────────────────────────────────────────────

func decodeTypeSymbol(payload []byte) (*ast.TypeSymbol, error) {
	r := bytes.NewReader(payload)
	name, err := ReadString(r)
	if err != nil {
		return nil, err
	}
	propsByte, err := ReadU8(r)
	if err != nil {
		return nil, err
	}
	mangled, err := ReadString(r)
	if err != nil {
		return nil, err
	}
	stype, err := DecodeSemaType(r)
	if err != nil {
		return nil, err
	}
	sym := ast.NewTypeSymbol(name, wireProps(propsByte), nil)
	sym.SetType(stype)
	sym.SetMangledName(mangled)
	return sym, nil
}

func decodeVarSymbol(payload []byte) (*ast.VariableSymbol, error) {
	r := bytes.NewReader(payload)
	name, err := ReadString(r)
	if err != nil {
		return nil, err
	}
	propsByte, err := ReadU8(r)
	if err != nil {
		return nil, err
	}
	mangled, err := ReadString(r)
	if err != nil {
		return nil, err
	}
	stype, err := DecodeSemaType(r)
	if err != nil {
		return nil, err
	}
	isExt, err := ReadBool(r)
	if err != nil {
		return nil, err
	}
	cname, err := ReadString(r)
	if err != nil {
		return nil, err
	}
	dllName, err := ReadString(r)
	if err != nil {
		return nil, err
	}
	sym := ast.NewVariableSymbol(name, wireProps(propsByte), nil)
	sym.SetType(stype)
	sym.SetMangledName(mangled)
	sym.IsExternal = isExt
	sym.CName = cname
	sym.DLLName = dllName
	return sym, nil
}

func decodeProcSymbol(payload []byte) (*ast.ProcedureSymbol, error) {
	r := bytes.NewReader(payload)
	name, err := ReadString(r)
	if err != nil {
		return nil, err
	}
	propsByte, err := ReadU8(r)
	if err != nil {
		return nil, err
	}
	mangled, err := ReadString(r)
	if err != nil {
		return nil, err
	}
	stype, err := DecodeSemaType(r)
	if err != nil {
		return nil, err
	}
	isExt, err := ReadBool(r)
	if err != nil {
		return nil, err
	}
	isVarArgs, err := ReadBool(r)
	if err != nil {
		return nil, err
	}
	cname, err := ReadString(r)
	if err != nil {
		return nil, err
	}
	dllName, err := ReadString(r)
	if err != nil {
		return nil, err
	}

	sym := ast.NewProcedureSymbol(name, wireProps(propsByte), nil, nil, 0)
	sym.SetType(stype)
	sym.SetMangledName(mangled)
	sym.IsExternal = isExt
	sym.IsVarArgs = isVarArgs
	sym.CName = cname
	sym.DLLName = dllName
	return sym, nil
}

// wireProps converts a wire props byte back to ast.IdentProps.
func wireProps(b byte) ast.IdentProps {
	switch b {
	case PropsExported:
		return ast.Exported
	case PropsReadOnly:
		return ast.ReadOnly
	case PropsExportedROonly:
		return ast.ExportedReadOnly
	default:
		return ast.Unexported
	}
}

// ── DefToBundle: .def source → Bundle (for precompile-stdlib) ─────────────────

// DefToBundle parses a .def stub, builds the sema LexicalScope, and produces
// the matching minir.Module (ExternalFunc-only for FFI DEFINITION modules).
// This is the heart of the precompile-stdlib pipeline.
func DefToBundle(src []byte) (*Bundle, error) {
	m, err := ParseDef(string(src))
	if err != nil {
		return nil, fmt.Errorf("def parse: %w", err)
	}

	scope := ast.NewLexicalScope(nil, m.Name)
	mirMod := &minir.Module{Name: m.Name}

	// ── type declarations ──────────────────────────────────────────────────
	// Build a local type table so 'proc' declarations can reference them.
	localTypes := make(map[string]types.Type)
	for _, td := range m.Types {
		st := defTypeToSema(td.Def, localTypes)
		namedSt := &types.NamedType{Name: m.Name + "." + td.Name, Def: st}
		localTypes[td.Name] = namedSt

		props := ast.Unexported
		if td.Exported {
			props = ast.Exported
		}
		sym := ast.NewTypeSymbol(td.Name, props, nil)
		sym.SetType(namedSt)
		sym.SetMangledName(td.Name)
		scope.Insert(sym)
	}

	// ── var declarations ───────────────────────────────────────────────────
	for _, vd := range m.Vars {
		st := defTypeToSema(vd.Typ, localTypes)
		props := ast.Unexported
		if vd.Exported {
			props = ast.Exported
		}
		sym := ast.NewVariableSymbol(vd.Name, props, nil)
		sym.SetType(st)
		sym.SetMangledName(vd.Name)
		if m.DLLName != "" {
			sym.IsExternal = true
			sym.CName = m.Prefix + vd.Name
			sym.DLLName = m.DLLName
		}
		scope.Insert(sym)

		// minir: extern global (linkage = external)
		mirTy := semaTypeToMinir(st)
		gv := &minir.GlobalVar{
			Name:    m.Name + "." + vd.Name,
			Ty:      mirTy,
			Linkage: minir.ExternalLinkage,
		}
		mirMod.Globals = append(mirMod.Globals, gv)
		_ = mirMod.SymTab.Define(gv.Name, gv.Ref())
	}

	// ── proc declarations ─────────────────────────────────────────────────
	for _, pd := range m.Procs {
		st := defProcToSemaType(pd, localTypes)
		props := ast.Unexported
		if pd.Exported {
			props = ast.Exported
		}
		sym := ast.NewProcedureSymbol(pd.Name, props, nil, nil, 0)
		sym.SetType(st)
		sym.SetMangledName(pd.Name)
		sym.IsExternal = m.DLLName != "" || pd.DLLName != ""
		sym.IsVarArgs = pd.Varargs
		sym.CName = pd.CName
		sym.DLLName = pd.DLLName
		if sym.DLLName == "" {
			sym.DLLName = m.DLLName
		}
		scope.Insert(sym)

		// minir: ExternalFunc
		if sym.IsExternal {
			ef := defProcToExternalFunc(pd, st, m.DLLName)
			mirMod.Externals = append(mirMod.Externals, ef)
		}
	}

	return &Bundle{
		ModuleName: m.Name,
		Scope:      scope,
		Module:     mirMod,
	}, nil
}

// ── type conversion helpers ───────────────────────────────────────────────────

// defTypeToSema converts a DefTypeExpr to a sema types.Type.
func defTypeToSema(te DefTypeExpr, locals map[string]types.Type) types.Type {
	switch v := te.(type) {
	case BasicTExpr:
		return basicNameToSema(v.Name)
	case *CptrTExpr:
		return &types.CPointerType{Base: defTypeToSema(v.Base, locals)}
	case *ArrayTExpr:
		length := -1
		if !v.Open {
			length = v.Len
		}
		return &types.ArrayType{Length: length, Elem: defTypeToSema(v.Elem, locals)}
	case NamedTExpr:
		if t, ok := locals[v.Name]; ok {
			return t
		}
		return &types.NamedType{Name: v.Name, Def: types.UnknownType}
	}
	return types.UnknownType
}

func basicNameToSema(name string) types.Type {
	switch name {
	case "integer", "int32":
		return types.IntegerType
	case "longint", "int64":
		return types.LongIntType
	case "shortint", "int16":
		return types.ShortIntType
	case "int8":
		return types.Int8Type
	case "byte":
		return types.ByteType
	case "real", "float32":
		return types.RealType
	case "longreal", "float64":
		return types.LongRealType
	case "boolean", "bool":
		return types.BooleanType
	case "char":
		return types.CharType
	case "wchar":
		return types.WCharType
	case "set":
		return types.SetType
	case "void":
		return types.VoidType
	default:
		return types.UnknownType
	}
}

func defProcToSemaType(pd *DefProc, locals map[string]types.Type) *types.ProcedureType {
	params := make([]*types.FormalParam, len(pd.Params))
	for i, p := range pd.Params {
		params[i] = &types.FormalParam{
			Kind: p.Kind,
			Name: p.Name,
			Type: defTypeToSema(p.Typ, locals),
		}
	}
	var result types.Type
	if pd.Result != nil {
		result = defTypeToSema(pd.Result, locals)
	}
	return &types.ProcedureType{Params: params, Result: result}
}

// semaTypeToMinir converts a sema type to the corresponding minir IR type.
func semaTypeToMinir(t types.Type) minir.Type {
	if t == nil {
		return nil
	}
	switch v := t.(type) {
	case *types.BasicType:
		switch v.Kind {
		case types.INTEGER, types.INT32:
			return minir.I32()
		case types.LONGINT, types.INT64:
			return minir.I64()
		case types.INT16, types.SHORTINT:
			return minir.I32()
		case types.INT8, types.BYTE:
			return minir.I32()
		case types.REAL:
			return minir.F32()
		case types.LONGREAL:
			return minir.F64()
		case types.BOOLEAN:
			return minir.Bool()
		case types.CHAR:
			return minir.I32()
		default:
			return minir.I64()
		}
	case *types.CPointerType:
		inner := semaTypeToMinir(v.Base)
		if inner == nil {
			inner = minir.I32()
		}
		return minir.Ptr(inner)
	case *types.PointerType:
		inner := semaTypeToMinir(v.Base)
		if inner == nil {
			inner = minir.I32()
		}
		return minir.Ptr(inner)
	case *types.NamedType:
		return semaTypeToMinir(v.Def)
	case *types.ArrayType:
		elem := semaTypeToMinir(v.Elem)
		if elem == nil {
			elem = minir.I32()
		}
		return minir.NewArrayType(v.Length, elem)
	default:
		return minir.I64() // fallback: opaque pointer-sized integer
	}
}

func defProcToExternalFunc(pd *DefProc, st *types.ProcedureType, modDLL string) *minir.ExternalFunc {
	params := make([]minir.Type, len(pd.Params))
	for i, p := range pd.Params {
		// Only value params — IN/VAR are not allowed in extern procs per spec.
		_ = p // type from st.Params[i]
		if i < len(st.Params) {
			params[i] = semaTypeToMinir(st.Params[i].Type)
		} else {
			params[i] = minir.I64()
		}
	}
	var result minir.Type
	if st.Result != nil {
		result = semaTypeToMinir(st.Result)
	}
	dll := pd.DLLName
	if dll == "" {
		dll = modDLL
	}
	cname := pd.CName
	if cname == "" {
		cname = pd.Name
	}
	return &minir.ExternalFunc{
		Name:    cname,
		Sig:     &minir.FunctionType{Params: params, Result: result},
		Linkage: minir.ExternalLinkage,
		Attrs: &minir.ExternalAttrs{
			CName:    cname,
			DLLName:  dll,
			Variadic: pd.Varargs,
		},
	}
}

