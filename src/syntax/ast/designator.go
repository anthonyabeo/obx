package ast

type Designator struct {
	QualifiedIdent Expression
	Selector       Expression
}

func (d *Designator) expr() {}
func (d *Designator) String() string {
	return ""
}
