package hir

import (
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

func (g Generator) emitOp(tok token.Kind) Op {
	switch tok {
	case token.PLUS:
		return Add
	case token.MINUS:
		return Sub
	case token.STAR:
		return Mul
	case token.QUOT:
		return Quot
	case token.DIV:
		return Div
	case token.MOD:
		return Mod
	case token.AND:
		return And
	case token.OR:
		return Or
	case token.EQUAL:
		return Eq
	case token.NEQ:
		return Neq
	case token.LESS:
		return Lt
	case token.LEQ:
		return Le
	case token.GREAT:
		return Gt
	case token.GEQ:
		return Ge
	case token.NOT:
		return Not
	default:
		return "nil"
	}
}

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

	return &CompoundStmt{
		Stmts: bodyStmts,
	}
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
