package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type QualifiedIdent struct {
	X     Expression
	Sel   *Ident
	EType types.Type
}

func (q *QualifiedIdent) Pos() *token.Position {
	return q.X.Pos()
}

func (q *QualifiedIdent) End() *token.Position {
	panic("not implemented")
}

func (q *QualifiedIdent) Type() types.Type {
	return q.EType
}

func (q *QualifiedIdent) Accept(vst Visitor) {
	vst.VisitQualifiedIdent(q)
}

func (q *QualifiedIdent) expr() {}
func (q *QualifiedIdent) String() string {
	return fmt.Sprintf("%v.%v", q.X, q.Sel)
}
