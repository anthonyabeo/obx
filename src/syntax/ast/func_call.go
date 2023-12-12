package ast

type FuncCall struct {
	Dsg          Expression
	ActualParams []Expression
}

func (f *FuncCall) expr() {}
func (f *FuncCall) String() string {
	return ""
}
