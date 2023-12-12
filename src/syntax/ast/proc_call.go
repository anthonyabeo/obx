package ast

type ProcCall struct {
	ProcName     Expression
	ActualParams []Expression
}

func (p *ProcCall) stmt() {}
func (p *ProcCall) String() string {
	return ""
}
