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

var AnyRec *RecordType

func init() {
	AnyRec = &RecordType{}
}
