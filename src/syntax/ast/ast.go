package ast

import (
	"fmt"
)

type Node interface {
	fmt.Stringer
	Pos() int
	End() int
	Accept(Visitor) any
	Children() []Node
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
	Imports() []*Import
	Environ() *Environment
}

func Walk(fn func(Node) bool, node Node) {
	var visit func(Node)
	visit = func(n Node) {
		if n == nil || !fn(n) {
			return
		}
		for _, child := range n.Children() {
			visit(child)
		}
	}
	visit(node)
}
