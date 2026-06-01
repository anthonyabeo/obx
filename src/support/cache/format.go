// Package cache provides the .obxi binary symbol-table cache used by the obx
// compiler to skip parse + sema + lowering for pre-compiled stdlib modules.
//
// # .obxi file layout
//
//	[8]  Magic   – "OBXIv01\0"
//	[8]  SrcHash – first 8 bytes of SHA-256(source .def content)
//	[4]  NameLen – u32LE length of module name
//	[N]  Name    – module name bytes (UTF-8)
//	     <records until EOF>
//
// Each record has the form:
//
//	[1]  Tag     – record kind (TagXxx constant)
//	[4]  Len     – u32LE payload length in bytes
//	[N]  Payload – Len bytes of tag-specific data
//
// # Sema symbol record tags (0x01–0x0F)
//
//	TagSymType (0x01) TypeSymbol   – name, props, sema type
//	TagSymVar  (0x02) VariableSymbol – name, props, sema type, FFI attrs
//	TagSymProc (0x03) ProcedureSymbol – name, props, sema type, FFI attrs
//
// # minir section record tags (0x10–0x1F)
//
//	TagMirExtern (0x10) ExternalFunc – name, sig, ExternalAttrs
//	TagMirGlobal (0x11) GlobalVar    – name, linkage, type, optional init
//	TagMirConst  (0x12) GlobalConst  – name, linkage, type, init
//	TagMirFunc   (0x13) Function     – full function body (future use)
package cache

import (
	"encoding/binary"
	"fmt"
	"io"
)

// Magic is the 8-byte file identifier written at the start of every .obxi.
var Magic = [8]byte{'O', 'B', 'X', 'I', 'v', '0', '1', 0}

// Record tag constants.
const (
	// Sema symbol records.
	TagSymType  byte = 0x01
	TagSymVar   byte = 0x02
	TagSymProc  byte = 0x03
	TagSymConst byte = 0x04 // ConstantSymbol — name, props, mangledName, semaType, litKind, litVal

	// minir section records.
	TagMirExtern     byte = 0x10
	TagMirGlobal     byte = 0x11
	TagMirConst      byte = 0x12
	TagMirFunc       byte = 0x13
	TagMirModuleMeta byte = 0x14 // module-level metadata: DLLName
)

// IdentProps wire codes (must match ast.IdentProps bit positions).
const (
	PropsUnexported     byte = 0
	PropsExported       byte = 1
	PropsReadOnly       byte = 2
	PropsExportedROonly byte = 3
)

// Linkage wire codes (must match minir.Linkage).
const (
	LinkExternal byte = 0
	LinkInternal byte = 1
	LinkPrivate  byte = 2
)

// ── low-level binary writers ─────────────────────────────────────────────────

// WriteU8 writes one byte to w.
func WriteU8(w io.Writer, b byte) error {
	_, err := w.Write([]byte{b})
	return err
}

// WriteU32LE writes a little-endian uint32 to w.
func WriteU32LE(w io.Writer, n uint32) error {
	var buf [4]byte
	binary.LittleEndian.PutUint32(buf[:], n)
	_, err := w.Write(buf[:])
	return err
}

// WriteU64LE writes a little-endian uint64 to w.
func WriteU64LE(w io.Writer, n uint64) error {
	var buf [8]byte
	binary.LittleEndian.PutUint64(buf[:], n)
	_, err := w.Write(buf[:])
	return err
}

// WriteI32LE writes a little-endian int32 to w.
func WriteI32LE(w io.Writer, n int32) error {
	return WriteU32LE(w, uint32(n))
}

// WriteI64LE writes a little-endian int64 to w.
func WriteI64LE(w io.Writer, n int64) error {
	return WriteU64LE(w, uint64(n))
}

// WriteString writes a length-prefixed UTF-8 string: u32LE len + bytes.
func WriteString(w io.Writer, s string) error {
	if err := WriteU32LE(w, uint32(len(s))); err != nil {
		return err
	}
	if len(s) > 0 {
		_, err := io.WriteString(w, s)
		return err
	}
	return nil
}

// WriteBool writes a single byte: 0 for false, 1 for true.
func WriteBool(w io.Writer, b bool) error {
	if b {
		return WriteU8(w, 1)
	}
	return WriteU8(w, 0)
}

// ── low-level binary readers ─────────────────────────────────────────────────

// ReadU8 reads one byte from r.
func ReadU8(r io.Reader) (byte, error) {
	var buf [1]byte
	if _, err := io.ReadFull(r, buf[:]); err != nil {
		return 0, err
	}
	return buf[0], nil
}

// ReadU32LE reads a little-endian uint32 from r.
func ReadU32LE(r io.Reader) (uint32, error) {
	var buf [4]byte
	if _, err := io.ReadFull(r, buf[:]); err != nil {
		return 0, err
	}
	return binary.LittleEndian.Uint32(buf[:]), nil
}

// ReadU64LE reads a little-endian uint64 from r.
func ReadU64LE(r io.Reader) (uint64, error) {
	var buf [8]byte
	if _, err := io.ReadFull(r, buf[:]); err != nil {
		return 0, err
	}
	return binary.LittleEndian.Uint64(buf[:]), nil
}

// ReadI32LE reads a little-endian int32 from r.
func ReadI32LE(r io.Reader) (int32, error) {
	n, err := ReadU32LE(r)
	return int32(n), err
}

// ReadI64LE reads a little-endian int64 from r.
func ReadI64LE(r io.Reader) (int64, error) {
	n, err := ReadU64LE(r)
	return int64(n), err
}

// ReadString reads a length-prefixed UTF-8 string from r.
func ReadString(r io.Reader) (string, error) {
	n, err := ReadU32LE(r)
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

// ReadBool reads a single byte and returns false for 0, true for non-zero.
func ReadBool(r io.Reader) (bool, error) {
	b, err := ReadU8(r)
	return b != 0, err
}

// ── record helpers ────────────────────────────────────────────────────────────

// WriteRecord writes a complete tagged record to w.
// tag is the record kind; payload is the already-serialised body.
func WriteRecord(w io.Writer, tag byte, payload []byte) error {
	if err := WriteU8(w, tag); err != nil {
		return err
	}
	if err := WriteU32LE(w, uint32(len(payload))); err != nil {
		return err
	}
	if len(payload) > 0 {
		if _, err := w.Write(payload); err != nil {
			return err
		}
	}
	return nil
}

// ReadRecordHeader reads the tag + length of the next record from r.
// Returns (0, 0, io.EOF) at end of file.
func ReadRecordHeader(r io.Reader) (tag byte, length uint32, err error) {
	tag, err = ReadU8(r)
	if err != nil {
		return 0, 0, err
	}
	length, err = ReadU32LE(r)
	return tag, length, err
}

// ReadRecordPayload reads exactly length bytes from r into a fresh []byte.
func ReadRecordPayload(r io.Reader, length uint32) ([]byte, error) {
	if length == 0 {
		return nil, nil
	}
	buf := make([]byte, length)
	if _, err := io.ReadFull(r, buf); err != nil {
		return nil, fmt.Errorf("reading record payload: %w", err)
	}
	return buf, nil
}

