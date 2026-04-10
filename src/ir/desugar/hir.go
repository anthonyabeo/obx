package desugar

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/sema/types"
)

type Node interface {
	fmt.Stringer
}

type Stmt interface {
	stmt()
	Node
}

type Decl interface {
	decl()
	Node
}

type Expr interface {
	Node
	expr()
	Type() types.Type
}
