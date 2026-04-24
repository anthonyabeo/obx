package types

import (
	"fmt"
	"strings"
)

type Field struct {
	Name       string
	Type       Type
	IsExported bool
}

type RecordType struct {
	Fields  []*Field
	Methods map[string]*Field // type-bound procedure methods keyed by name
	Base    *RecordType
	// Layout holds computed runtime layout and dispatch tables.  Populated by
	// the InheritanceViewPass after type-checking.  Nil until the pass runs.
	Layout  *RecordLayout
}

func (r *RecordType) String() string {
	var parts []string
	for _, f := range r.Fields {
		parts = append(parts, fmt.Sprintf("%s: %s", f.Name, f.Type.String()))
	}
	return fmt.Sprintf("RECORD %s END", strings.Join(parts, "; "))
}

func (r *RecordType) Alignment() int {
	maxAlign := 1
	for _, f := range r.Fields {
		align := f.Type.Alignment()
		if align > maxAlign {
			maxAlign = align
		}
	}
	if r.Base != nil {
		baseAlign := r.Base.Alignment()
		if baseAlign > maxAlign {
			maxAlign = baseAlign
		}
	}
	return maxAlign
}

// Width returns the byte size of this record type including alignment padding
// between fields and a final size-rounding pad to a multiple of the record's
// own alignment.  Returns -1 if any component has an unknown width.
func (r *RecordType) Width() int {
	offset := 0

	// Start after the base record's padded size.
	if r.Base != nil {
		bw := r.Base.Width()
		if bw < 0 {
			return -1
		}
		offset = bw
		// Pad so that the first own field starts at a properly-aligned offset.
		if ba := r.Base.Alignment(); ba > 0 {
			offset = (offset + ba - 1) / ba * ba
		}
	}

	for _, f := range r.Fields {
		fw := f.Type.Width()
		if fw < 0 {
			return -1
		}
		// Align field to its own alignment requirement.
		if fa := f.Type.Alignment(); fa > 0 {
			offset = (offset + fa - 1) / fa * fa
		}
		offset += fw
	}

	// Pad the total to a multiple of the record's alignment so that arrays
	// of this record type lay out correctly.
	if a := r.Alignment(); a > 0 {
		offset = (offset + a - 1) / a * a
	}
	return offset
}

func (r *RecordType) Equals(other Type) bool {
	o, ok := other.(*RecordType)
	if !ok || len(r.Fields) != len(o.Fields) {
		return false
	}
	// Base records must also match.
	if r.Base != o.Base {
		if r.Base == nil || o.Base == nil {
			return false
		}
		if !r.Base.Equals(o.Base) {
			return false
		}
	}
	for i := range r.Fields {
		if r.Fields[i].Name != o.Fields[i].Name || !r.Fields[i].Type.Equals(o.Fields[i].Type) {
			return false
		}
	}
	return true
}

func (r *RecordType) GetField(name string) *Field {
	for _, f := range r.Fields {
		if f.Name == name {
			return f
		}
	}

	if r.Base != nil {
		return r.Base.GetField(name)
	}

	return nil
}

// InsertMethod inserts a type-bound procedure into the record's own method table.
// It does not modify any base record type; it only records the method at this level.
func (r *RecordType) InsertMethod(name string, ty *ProcedureType, exported bool) {
	if r.Methods == nil {
		r.Methods = make(map[string]*Field)
	}
	if _, exists := r.Methods[name]; !exists {
		r.Methods[name] = &Field{Name: name, Type: ty, IsExported: exported}
	}
}

// GetMethod searches the method table of this record and its base chain.
func (r *RecordType) GetMethod(name string) *Field {
	for cur := r; cur != nil; cur = cur.Base {
		if cur.Methods != nil {
			if f, ok := cur.Methods[name]; ok {
				return f
			}
		}
	}
	return nil
}

// RecordLayout contains runtime layout metadata computed after semantic
// analysis: field offsets, size, alignment, vtable method order (indices), and
// RTTI names.  The InheritanceViewPass is responsible for filling this in.
type RecordLayout struct {
	// FieldOffsets maps field name to its byte offset within the object
	// instance.  Offsets are measured from the start of the object and the
	// object includes a per-instance vptr at offset 0.
	FieldOffsets map[string]int

	// Fields describes the flattened list of fields (including inherited
	// fields) in declaration order with offsets and GC pointer flag.
	Fields []FieldLayout

	// Size is the total byte size of the record (including vptr header and
	// alignment padding).
	Size int

	// Alignment is the record alignment in bytes.
	Alignment int

	// VTable is the ordered list of method slots (base first).  Each entry
	// references a method by its mangled name and module-local function index.
	VTable []MethodSlot

	// VTableIndex maps method name to index in VTable for quick lookup.
	VTableIndex map[string]int

	// RTTIName is the mangled symbol name for this record's RTTI global.
	RTTIName string

	// VTableName is the mangled symbol name for this record's vtable global.
	VTableName string
}

type FieldLayout struct {
	Name   string
	Type   Type
	Offset int
	IsPtr  bool
}

type MethodSlot struct {
	Name      string
	Mangled   string
	FuncIndex uint32
}

// GetFieldOffset returns the byte offset of field name if present.
func (rl *RecordLayout) GetFieldOffset(name string) (int, bool) {
	if rl == nil || rl.FieldOffsets == nil {
		return 0, false
	}
	off, ok := rl.FieldOffsets[name]
	return off, ok
}

// MethodIndex returns the vtable index for method name if present.
func (rl *RecordLayout) MethodIndex(name string) (int, bool) {
	if rl == nil || rl.VTableIndex == nil {
		return 0, false
	}
	i, ok := rl.VTableIndex[name]
	return i, ok
}

var AnyRec *RecordType

func init() {
	AnyRec = &RecordType{}
}
