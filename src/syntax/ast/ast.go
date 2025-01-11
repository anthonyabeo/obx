package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/meer"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

type Node interface {
	fmt.Stringer
	Pos() *token.Position
	End() *token.Position
	Accept(Visitor)
}

type Statement interface {
	Node
	stmt()
}

type Expression interface {
	Node
	expr()
	Type() types.Type
	Value() tacil.Expr
	MirValue() meer.Expression
}

type Declaration interface {
	Node
	decl()
}

type Type interface {
	Node
	Type() types.Type
	IRType() tacil.Type
	MirType() meer.Type
}

type Unit interface {
	Node
	Name() string
	ListImport() []*Import
	Edges() map[string]Unit
	addEdge(string, Unit)
}
