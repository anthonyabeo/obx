package sema

import (
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
)

type Visitor struct {
	errors lexer.ErrorList

	ast *ast.Oberon
	env Scope
}

func (v Visitor) InitSemaVisitor(ast *ast.Oberon, env Scope) {
	v.ast = ast
	v.env = env
}

func (v Visitor) VisitModule(name string) {
	module := v.ast.Program[name]

	for _, decl := range module.DeclSeq {
		decl.Accept(v)
	}

	for _, stmt := range module.StmtSeq {
		stmt.Accept(v)
	}
}

func (v Visitor) VisitIdentifier(id *ast.Ident) {
	obj := v.env.Lookup(id.Name)
	if obj == nil {
	}

	id.EType = obj.Type()
}

func (v Visitor) VisitUInt(uInt *ast.UInt) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitDesignator(designator *ast.Designator) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitFuncCall(call *ast.FuncCall) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitUnaryExpr(expr *ast.UnaryExpr) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitQualifiedIdent(ident *ast.QualifiedIdent) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitIfStmt(stmt *ast.IfStmt) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitAssignStmt(stmt *ast.AssignStmt) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitWhileStmt(stmt *ast.WhileStmt) {
	panic("implement me")
}

func (v Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitProcCall(call *ast.ProcCall) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitProcDecl(decl *ast.ProcDecl) {
	//TODO implement me
	panic("implement me")
}

func (v Visitor) VisitVarDecl(decl *ast.VarDecl) {
	//TODO implement me
	panic("implement me")
}
