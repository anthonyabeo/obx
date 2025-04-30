package ast

import (
	"bytes"
	"fmt"
	"strings"
)

type Import struct {
	Alias      string
	Name       string
	ImportPath []string
	Meta       []Expression
}

func (imp *Import) String() string {
	buf := new(bytes.Buffer)
	if imp.Alias != "" {
		buf.WriteString(imp.Alias)
		buf.WriteString(" := ")
	}

	var path []string
	for _, id := range imp.ImportPath {
		path = append(path, id)
	}

	if len(path) > 0 {
		buf.WriteString(strings.Join(path, "."))
		buf.WriteString(".")
	}

	buf.WriteString(imp.Name)
	if len(imp.Meta) > 0 {
		var metas []string
		for _, meta := range imp.Meta {
			metas = append(metas, meta.String())
		}
		buf.WriteString(fmt.Sprintf("(%s)", strings.Join(metas, ", ")))
	}

	return buf.String()
}
func (imp *Import) Accept(vst Visitor) { vst.VisitImport(imp) }
