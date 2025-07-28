package hir

import (
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

func (g Generator) emitParamKind(kind token.Kind) ParamKind {
	switch kind {
	case token.VAR:
		return VarParam
	case token.IN:
		return InParam
	default:
		return ValueParam
	}
}

func (g Generator) visitStmtSeq(stmts []ast.Statement) *CompoundStmt {
	var bodyStmts []Stmt
	for _, stmt := range stmts {
		hirStmt := stmt.Accept(g).(Stmt)
		bodyStmts = append(bodyStmts, hirStmt)
	}

	return &CompoundStmt{Stmts: bodyStmts}
}

func (g Generator) visitDeclSeq(decls []ast.Declaration) (d []Decl) {
	for _, decl := range decls {
		d = append(d, decl.Accept(g).(Decl))
	}

	return d
}

func (g Generator) visitElseIfs(branches []*ast.ElseIfBranch) []*ElseIfBranch {
	var elseIfs []*ElseIfBranch
	for _, branch := range branches {
		elseIfs = append(elseIfs, branch.Accept(g).(*ElseIfBranch))
	}

	return elseIfs
}

func (g Generator) visitExprList(list []ast.Expression) (l []Expr) {
	for _, expression := range list {
		expr := expression.Accept(g).(Expr)
		l = append(l, expr)
	}

	return l
}
