package cache

// encode.go — .obxi binary bundle encoder.
//
// Writes a complete .obxi file containing:
//   - The file header (magic + source hash + module name)
//   - Sema symbol records (TagSymType, TagSymVar, TagSymProc)
//   - minir section records (TagMirExtern, TagMirGlobal, TagMirConst, TagMirFunc)
//
// Usage:
//
//	e := NewEncoder(w)
//	e.WriteHeader(moduleName, srcHash)
//	e.WriteScope(scope)
//	e.WriteModule(mirModule)

import (
	"bytes"
	"crypto/sha256"
	"fmt"
	"io"
	"os"

	"github.com/anthonyabeo/obx/src/ir/minir"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// Encoder serialises a sema LexicalScope + minir.Module into an .obxi file.
type Encoder struct {
	w io.Writer
}

// NewEncoder returns an Encoder that writes to w.
func NewEncoder(w io.Writer) *Encoder { return &Encoder{w: w} }

// ── Header ────────────────────────────────────────────────────────────────────

// WriteHeader writes the 8-byte magic, 8-byte source hash, and module name.
// srcContent is the raw content of the source .def file; the first 8 bytes of
// its SHA-256 digest are stored as the staleness indicator.
func (e *Encoder) WriteHeader(moduleName string, srcContent []byte) error {
	// magic
	if _, err := e.w.Write(Magic[:]); err != nil {
		return err
	}
	// 8-byte truncated SHA-256 of source
	sum := sha256.Sum256(srcContent)
	if _, err := e.w.Write(sum[:8]); err != nil {
		return err
	}
	// module name
	return WriteString(e.w, moduleName)
}

// ── Sema scope ────────────────────────────────────────────────────────────────

// WriteScope serialises all symbols in scope into TagSym* records.
// Only TypeSymbol, VariableSymbol, ProcedureSymbol, and ConstantSymbol are
// serialized; other symbol kinds (module, field, param) are skipped.
func (e *Encoder) WriteScope(scope *ast.LexicalScope) error {
	for _, sym := range scope.Elems() {
		switch s := sym.(type) {
		case *ast.TypeSymbol:
			if err := e.writeTypeSymbol(s); err != nil {
				return err
			}
		case *ast.VariableSymbol:
			if err := e.writeVarSymbol(s); err != nil {
				return err
			}
		case *ast.ProcedureSymbol:
			if err := e.writeProcSymbol(s); err != nil {
				return err
			}
		case *ast.ConstantSymbol:
			if err := e.writeConstSymbol(s); err != nil {
				return err
			}
		}
	}
	return nil
}

func (e *Encoder) writeTypeSymbol(s *ast.TypeSymbol) error {
	var buf bytes.Buffer
	if err := WriteString(&buf, s.Name()); err != nil {
		return err
	}
	if err := WriteU8(&buf, propsWire(s.Props())); err != nil {
		return err
	}
	if err := WriteString(&buf, s.MangledName()); err != nil {
		return err
	}
	if err := EncodeSemaType(&buf, s.Type()); err != nil {
		return err
	}
	return WriteRecord(e.w, TagSymType, buf.Bytes())
}

func (e *Encoder) writeVarSymbol(s *ast.VariableSymbol) error {
	var buf bytes.Buffer
	if err := WriteString(&buf, s.Name()); err != nil {
		return err
	}
	if err := WriteU8(&buf, propsWire(s.Props())); err != nil {
		return err
	}
	if err := WriteString(&buf, s.MangledName()); err != nil {
		return err
	}
	if err := EncodeSemaType(&buf, s.Type()); err != nil {
		return err
	}
	// FFI fields (VariableSymbol may be an extern C global)
	if err := WriteBool(&buf, s.IsExternal); err != nil {
		return err
	}
	if err := WriteString(&buf, s.CName); err != nil {
		return err
	}
	if err := WriteString(&buf, s.DLLName); err != nil {
		return err
	}
	return WriteRecord(e.w, TagSymVar, buf.Bytes())
}

func (e *Encoder) writeProcSymbol(s *ast.ProcedureSymbol) error {
	var buf bytes.Buffer
	if err := WriteString(&buf, s.Name()); err != nil {
		return err
	}
	if err := WriteU8(&buf, propsWire(s.Props())); err != nil {
		return err
	}
	if err := WriteString(&buf, s.MangledName()); err != nil {
		return err
	}
	if err := EncodeSemaType(&buf, s.Type()); err != nil {
		return err
	}
	if err := WriteBool(&buf, s.IsExternal); err != nil {
		return err
	}
	if err := WriteBool(&buf, s.IsVarArgs); err != nil {
		return err
	}
	if err := WriteString(&buf, s.CName); err != nil {
		return err
	}
	if err := WriteString(&buf, s.DLLName); err != nil {
		return err
	}
	return WriteRecord(e.w, TagSymProc, buf.Bytes())
}

// ── minir module ──────────────────────────────────────────────────────────────

// WriteModule serialises all minir-level entities from m using the cache
// record tags (TagMirGlobal, TagMirConst, TagMirExtern, TagMirFunc) so that
// DecodeBundle can identify and reassemble them correctly.
//
// Previous versions wrote raw minir bytes (opcodes 0x20–0x23) that the decoder
// never matched (it expected 0x10–0x13), silently producing empty modules.
func (e *Encoder) WriteModule(m *minir.Module) error {
	// Write module-level metadata (DLLName) so the decoder can restore it.
	if m.DLLName != "" {
		var mbuf bytes.Buffer
		if err := WriteString(&mbuf, m.DLLName); err != nil {
			return fmt.Errorf("encode module DLLName: %w", err)
		}
		if err := WriteRecord(e.w, TagMirModuleMeta, mbuf.Bytes()); err != nil {
			return err
		}
	}
	for _, gv := range m.Globals {
		var buf bytes.Buffer
		if err := minir.EncodeGlobalVar(&buf, gv); err != nil {
			return fmt.Errorf("encode GlobalVar %s: %w", gv.Name, err)
		}
		if err := WriteRecord(e.w, TagMirGlobal, buf.Bytes()); err != nil {
			return err
		}
	}
	for _, gc := range m.Constants {
		var buf bytes.Buffer
		if err := minir.EncodeGlobalConst(&buf, gc); err != nil {
			return fmt.Errorf("encode GlobalConst %s: %w", gc.Name, err)
		}
		if err := WriteRecord(e.w, TagMirConst, buf.Bytes()); err != nil {
			return err
		}
	}
	for _, ef := range m.Externals {
		var buf bytes.Buffer
		if err := minir.EncodeExternalFunc(&buf, ef); err != nil {
			return fmt.Errorf("encode ExternalFunc %s: %w", ef.Name, err)
		}
		if err := WriteRecord(e.w, TagMirExtern, buf.Bytes()); err != nil {
			return err
		}
	}
	for _, fn := range m.Functions {
		var buf bytes.Buffer
		if err := minir.EncodeFunction(&buf, fn); err != nil {
			return fmt.Errorf("encode Function %s: %w", fn.FnName, err)
		}
		if err := WriteRecord(e.w, TagMirFunc, buf.Bytes()); err != nil {
			return err
		}
	}
	return nil
}

// ── File I/O helpers ──────────────────────────────────────────────────────────

// SaveBundle is a convenience that writes a complete .obxi file at path.
// srcContent must be the raw source of the .def file for hash computation.
func SaveBundle(path string, moduleName string, src []byte,
	scope *ast.LexicalScope, m *minir.Module) error {

	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()

	enc := NewEncoder(f)
	if err := enc.WriteHeader(moduleName, src); err != nil {
		return err
	}
	if scope != nil {
		if err := enc.WriteScope(scope); err != nil {
			return err
		}
	}
	if m != nil {
		if err := enc.WriteModule(m); err != nil {
			return err
		}
	}
	return nil
}

// writeConstSymbol serialises a ConstantSymbol.
// The Value field (AST Expression) is stored as a (litKind u32, litVal string)
// pair sourced from the *ast.BasicLit that the constant evaluates to.
// If the Value is nil or is not a *ast.BasicLit, litKind=0 and litVal="" are
// written so the decoder can still reconstruct a placeholder symbol.
func (e *Encoder) writeConstSymbol(s *ast.ConstantSymbol) error {
	var buf bytes.Buffer
	if err := WriteString(&buf, s.Name()); err != nil {
		return err
	}
	if err := WriteU8(&buf, propsWire(s.Props())); err != nil {
		return err
	}
	if err := WriteString(&buf, s.MangledName()); err != nil {
		return err
	}
	if err := EncodeSemaType(&buf, s.Type()); err != nil {
		return err
	}
	// Encode the concrete value as (litKind u32LE, litVal string).
	var litKind uint32
	var litVal string
	if lit, ok := s.Value.(*ast.BasicLit); ok && lit != nil {
		litKind = uint32(lit.Kind)
		litVal = lit.Val
	}
	if err := WriteU32LE(&buf, litKind); err != nil {
		return err
	}
	if err := WriteString(&buf, litVal); err != nil {
		return err
	}
	return WriteRecord(e.w, TagSymConst, buf.Bytes())
}

// ── helpers ────────────────────────────────────────────────────────────────

func propsWire(p ast.IdentProps) byte {
	switch p {
	case ast.Exported:
		return PropsExported
	case ast.ReadOnly:
		return PropsReadOnly
	case ast.ExportedReadOnly:
		return PropsExportedROonly
	default:
		return PropsUnexported
	}
}
