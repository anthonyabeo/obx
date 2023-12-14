package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type ProcCall struct {
	NamePos      *token.Position
	ProcName     Expression
	ActualParams []Expression
}

func (p *ProcCall) Pos() *token.Position {
	return p.NamePos
}

func (p *ProcCall) End() *token.Position {
	panic("not implemented")
}

func (p *ProcCall) stmt() {}
func (p *ProcCall) String() string {
	return ""
}
