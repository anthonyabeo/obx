package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

// BadExpr
// ------------------------------------------
type BadExpr struct {
	From *token.Position
	To   *token.Position
}

func (b *BadExpr) Pos() *token.Position {
	return b.From
}

func (b *BadExpr) End() *token.Position {
	return b.To
}

func (b *BadExpr) expr() {}
func (b *BadExpr) String() string {
	return ""
}

// BadStmt
// ------------------------------------------
type BadStmt struct {
	From *token.Position
	To   *token.Position
}

func (b *BadStmt) Pos() *token.Position {
	return b.From
}

func (b *BadStmt) End() *token.Position {
	return b.To
}

func (b *BadStmt) stmt() {}
func (b *BadStmt) String() string {
	return ""
}

// BadDecl
// ------------------------------------------
type BadDecl struct {
	From *token.Position
	To   *token.Position
}

func (b *BadDecl) Pos() *token.Position {
	return b.From
}

func (b *BadDecl) End() *token.Position {
	return b.To
}

func (b *BadDecl) decl() {}
func (b *BadDecl) String() string {
	return ""
}
