package ast

type BadExpr struct {
}

func (b *BadExpr) expr() {}
func (b *BadExpr) String() string {
	return ""
}
