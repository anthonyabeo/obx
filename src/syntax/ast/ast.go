package ast

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Node interface {
	fmt.Stringer
	Pos() *token.Position
	End() *token.Position
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
