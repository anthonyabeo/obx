package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Designator struct {
	QIdentPos      *token.Position
	QualifiedIdent Expression
	Selector       Selector
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
	s := d.QualifiedIdent.String()
	if d.Selector != nil {
		switch d.Selector.(type) {
		case *DotOp:
		case *IndexOp:
		case *TypeGuard:
		}
	}

	return s
}

// Selectors
// ---------------------

type Selector interface {
	Expression
	sel()
}

// DotOp
// ---------------------
type DotOp struct {
	Field *Ident
	EType types.Type
}

func (d *DotOp) Pos() *token.Position { return d.Field.Pos() }
func (d *DotOp) End() *token.Position { return d.Field.End() }
func (d *DotOp) String() string       { return fmt.Sprintf(".%s", d.Field) }
func (d *DotOp) Type() types.Type     { return d.EType }
func (d *DotOp) Accept(vst Visitor)   { vst.VisitDotOp(d) }
func (d *DotOp) expr()                {}
func (d *DotOp) sel()                 {}

// IndexOp
// ---------------------
type IndexOp struct {
	List  []Expression
	EType types.Type
}

func (id *IndexOp) Pos() *token.Position { panic("not implement") }
func (id *IndexOp) End() *token.Position { panic("not implement") }
func (id *IndexOp) String() string {
	var list []string
	for _, expr := range id.List {
		list = append(list, expr.String())
	}

	return fmt.Sprintf("[%v]", strings.Join(list, ","))
}
func (id *IndexOp) Type() types.Type   { return id.EType }
func (id *IndexOp) Accept(vst Visitor) { vst.VisitIndexOp(id) }
func (id *IndexOp) expr()              {}
func (id *IndexOp) sel()               {}

// TypeGuard
// ---------------------
type TypeGuard struct {
	Typ   Expression
	EType types.Type
}

func (t *TypeGuard) Pos() *token.Position { panic("not implement") }
func (t *TypeGuard) End() *token.Position { panic("not implement") }
func (t *TypeGuard) String() string       { panic("not implement") }
func (t *TypeGuard) Type() types.Type     { return t.EType }
func (t *TypeGuard) Accept(vst Visitor)   { vst.VisitTypeGuard(t) }
func (t *TypeGuard) expr()                {}
func (t *TypeGuard) sel()                 {}

// PointerDeref
// ---------------------
type PointerDeref struct {
	EType types.Type
}

func (deref *PointerDeref) Pos() *token.Position { panic("not implement") }
func (deref *PointerDeref) End() *token.Position { panic("not implement") }
func (deref *PointerDeref) String() string       { panic("not implement") }
func (deref *PointerDeref) Type() types.Type     { return deref.EType }
func (deref *PointerDeref) Accept(vst Visitor)   { vst.VisitPointerDeref(deref) }
func (deref *PointerDeref) expr()                {}
func (deref *PointerDeref) sel()                 {}
