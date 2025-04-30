package ast

import "fmt"

type Node interface {
	fmt.Stringer
	Accept(Visitor)
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

type Type interface {
	Node
}

type Unit interface {
	Node
	Name() string
	ListImport() []*Import
	Edges() map[string]Unit
	addEdge(string, Unit)
}
