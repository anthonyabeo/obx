package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type ReturnStmt struct {
	Return *token.Position
	Value  Expression
}

func (r *ReturnStmt) Pos() *token.Position {
	return r.Return
}

func (r *ReturnStmt) End() (pos *token.Position) {
	if r.Value != nil {
		pos = r.Value.End()
	} else {
		pos = r.Pos()
		pos.Column += len("return")
	}

	return
}

func (r *ReturnStmt) Accept(vst Visitor) {
	vst.VisitReturnStmt(r)
}

func (r *ReturnStmt) stmt() {}

func (r *ReturnStmt) String() string {
	return fmt.Sprintf("return %v", r.Value)
}
