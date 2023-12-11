package ast

type UInt struct {
	Value string
}

func (i *UInt) expr() {}
func (i *UInt) String() string {
	return i.Value
}
