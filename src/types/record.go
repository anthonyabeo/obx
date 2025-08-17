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
	Fields map[string]*Field
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
	return 8 /* yet to be implemented*/
}

func (r *RecordType) Width() int { return 8 /* yet to be implemented*/ }

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

func (r *RecordType) GetField(name string) *Field {
	f, ok := r.Fields[name]
	if !ok && r.Base != nil {
		return r.Base.GetField(name)
	}

	return f
}

var AnyRec *RecordType

func init() {
	AnyRec = &RecordType{}
}
