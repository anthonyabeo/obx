package ast

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type FuncCall struct {
	Dsg          Expression
	ActualParams []Expression
	EType        types.Type
}

func (f *FuncCall) Pos() *token.Position {
	return f.Dsg.Pos()
}

func (f *FuncCall) End() (pos *token.Position) {
	size := len(f.ActualParams)
	if size > 0 {
		pos = f.ActualParams[size-1].End()
		pos.Column += len(")")
	} else {
		pos = f.Dsg.End()
		pos.Column += len("()")
	}

	return
}

func (f *FuncCall) Type() types.Type {
	return f.EType
}

func (f *FuncCall) Accept(vst Visitor) {
	vst.VisitFuncCall(f)
}

func (f *FuncCall) expr() {}
func (f *FuncCall) String() string {
	return ""
}
