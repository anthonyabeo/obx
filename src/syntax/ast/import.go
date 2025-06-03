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

	StartOffset int
	EndOffset   int
}

func (imp *Import) String() string {
	buf := new(bytes.Buffer)
	if imp.Alias != "" {
		buf.WriteString(imp.Alias)
		buf.WriteString(" := ")
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
func (imp *Import) Accept(vst Visitor) any { return vst.VisitImport(imp) }
func (imp *Import) Pos() int               { return imp.StartOffset }
func (imp *Import) End() int               { return imp.EndOffset }
func (imp *Import) Children() []Node {
	children := make([]Node, 0)

	for _, expression := range imp.Meta {
		children = append(children, expression)
	}

	return children
}
