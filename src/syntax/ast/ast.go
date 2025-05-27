package ast

import (
	"fmt"
)

type Node interface {
	fmt.Stringer
	Pos() int
	End() int
	Accept(Visitor) any
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
	typ()
	Width() int
}

type CompilationUnit interface {
	Node
	Name() string
	ListImport() []*Import
	Edges() map[string]CompilationUnit
	addEdge(string, CompilationUnit)
}
