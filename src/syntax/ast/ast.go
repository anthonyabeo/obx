package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/sema/types"

	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/ir"
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
	Value() ir.Value
}

type Declaration interface {
	Node
	decl()
}

type Type interface {
	Node
	Type() types.Type
	IRType() ir.Type
}
