package hir

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/types"
)

type Node interface {
	fmt.Stringer
}

type Stmt interface {
	isStmt()
	Node
}

type Decl interface {
	isDecl()
	Node
}

type Expr interface {
	Node
	isExpr()
	Type() types.Type
}

//type Op string
//
//const (
//	Add  Op = "+"
//	Sub  Op = "-"
//	Mul  Op = "*"
//	Quot Op = "/"
//	Div  Op = "div"
//	Mod  Op = "mod"
//	And  Op = "and"
//	Or   Op = "or"
//	Eq   Op = "="
//	Neq  Op = "#"
//	Lt   Op = "<"
//	Gt   Op = ">"
//	Le   Op = "<="
//	Ge   Op = ">="
//	Not  Op = "~"
//)
