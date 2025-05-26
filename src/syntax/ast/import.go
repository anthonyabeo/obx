package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/report"
)

type Import struct {
	Alias      string
	Name       string
	ImportPath []string
	Meta       []Expression

	Pos *report.Position
	Rng *report.Range
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
func (imp *Import) Accept(vst Visitor) any     { return vst.VisitImport(imp) }
func (imp *Import) Position() *report.Position { return imp.Pos }
func (imp *Import) Range() *report.Range       { return imp.Rng }
