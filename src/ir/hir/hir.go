package hir

import "fmt"

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
	Type() Type
}

type Type interface {
	Node
	isType()
}
