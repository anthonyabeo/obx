package ast

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Designator struct {
	QIdentPos      *token.Position
	QualifiedIdent Expression
	Selector       Expression
	EType          types.Type
}

func (d *Designator) Pos() *token.Position {
	return d.QIdentPos
}

func (d *Designator) End() (pos *token.Position) {
	if d.Selector != nil {
		pos = d.Selector.End()
	} else {
		pos = d.QualifiedIdent.End()
	}

	return
}

func (d *Designator) Type() types.Type {
	return d.EType
}

func (d *Designator) Accept(vst Visitor) {
	vst.VisitDesignator(d)
}

func (d *Designator) expr() {}
func (d *Designator) String() string {
	return ""
}
