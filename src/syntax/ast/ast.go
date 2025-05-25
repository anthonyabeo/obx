package ast

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/report"
)

type Node interface {
	fmt.Stringer
	Position() *report.Position
	Range() *report.Range
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
