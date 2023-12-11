package ast

import "fmt"

type Node interface {
	fmt.Stringer
}

type Statement interface {
	Node
	stmt()
}

type Expression interface {
	Node
	expr()
}

type Declaration interface {
	Node
	decl()
}

type Module struct {
	BeginName, EndName *Ident
	//ImportList []*Import
	DeclSeq []Declaration
	StmtSeq []Statement
}

type Definition struct {
}
