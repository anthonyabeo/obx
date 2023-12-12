package ast

type Oberon struct {
	Program map[string]*Module
}

func NewOberon() *Oberon {
	return &Oberon{Program: make(map[string]*Module)}
}
