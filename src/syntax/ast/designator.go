package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type Designator struct {
	QIdentPos      *token.Position
	QualifiedIdent Expression
	Selector       Expression
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

func (d *Designator) expr() {}
func (d *Designator) String() string {
	return ""
}
