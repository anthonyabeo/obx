package ast

import "fmt"

type QualifiedIdent struct {
	X   Expression
	Sel *Ident
}

func (q *QualifiedIdent) expr() {}
func (q *QualifiedIdent) String() string {
	return fmt.Sprintf("%v.%v", q.X, q.Sel)
}
