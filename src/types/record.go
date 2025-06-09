package types

import (
	"fmt"
	"strings"
)

type Field struct {
	Name string
	Type Type
}

type RecordType struct {
	Fields []Field
	Base   *RecordType
}

func (r *RecordType) String() string {
	var parts []string
	for _, f := range r.Fields {
		parts = append(parts, fmt.Sprintf("%s: %s", f.Name, f.Type.String()))
	}
	return fmt.Sprintf("RECORD %s END", strings.Join(parts, "; "))
}

func (r *RecordType) Alignment() int {
	panic("Not implemented")
}

func (r *RecordType) Width() int {
	panic("Not implemented")
}

func (r *RecordType) Equals(other Type) bool {
	o, ok := other.(*RecordType)
	if !ok || len(r.Fields) != len(o.Fields) {
		return false
	}
	for i := range r.Fields {
		if r.Fields[i].Name != o.Fields[i].Name || !r.Fields[i].Type.Equals(o.Fields[i].Type) {
			return false
		}
	}
	return true
}
