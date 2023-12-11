package ast

type Ident struct {
	Name     string
	Exported bool
}

func (i *Ident) String() string {
	return i.Name
}
