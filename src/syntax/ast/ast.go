package ast

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/translate/ir"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
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
	Operand() ir.Operand
}

type Declaration interface {
	Node
	decl()
}

type Type interface {
	Node
	Type() types.Type
	Width() int
}
