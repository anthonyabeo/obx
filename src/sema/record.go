package sema

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/sema/types"
)

type Record struct {
	base   *Record
	fields *Scope
}

func NewRecordType(fields *Scope, base *Record) *Record {
	return &Record{base, fields}
}

func (r Record) String() string {
	var fields []string
	for name, sym := range r.fields.elems {
		fields = append(fields, fmt.Sprintf("%s: %s", name, sym.Type()))
	}

	return fmt.Sprintf("{%s}", strings.Join(fields, "; "))
}

func (r Record) Underlying() types.Type {
	return r
}

func (r Record) Width() int {
	var w int
	for _, field := range r.fields.elems {
		w += field.Type().Width()
	}

	return w
}

func (r Record) ExtendsBase() bool { return r.base != nil }

func (r Record) Base() *Record { return r.base }
