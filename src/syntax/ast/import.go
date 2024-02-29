package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Import struct {
	ImpNamePos *token.Position
	Alias      *Ident
	Name       *Ident
	ImportPath []*Ident
	Meta       []Expression
}

func (imp *Import) Pos() *token.Position { panic("not implemented") }
func (imp *Import) End() *token.Position { panic("not implemented") }
func (imp *Import) String() string {
	buf := new(bytes.Buffer)
	if imp.Alias != nil {
		buf.WriteString(imp.Alias.Name)
		buf.WriteString(" := ")
	}

	var path []string
	for _, id := range imp.ImportPath {
		path = append(path, id.Name)
	}

	if len(path) > 0 {
		buf.WriteString(strings.Join(path, "."))
		buf.WriteString(".")
	}

	buf.WriteString(imp.Name.Name)
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
