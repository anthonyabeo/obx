package translate

import (
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type Visitor struct {
	Instr []ir.Instruction

	ast *ast.Oberon
	env *sema.Scope
}

func NewVisitor(ast *ast.Oberon, env *sema.Scope) *Visitor {
	return &Visitor{ast: ast, env: env}
}

func (i *Visitor) VisitIdentifier(ident *ast.Ident) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitDesignator(designator *ast.Designator) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitFuncCall(call *ast.FuncCall) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitUnaryExpr(expr *ast.UnaryExpr) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitQualifiedIdent(ident *ast.QualifiedIdent) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitSet(set *ast.Set) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitBasicLit(lit *ast.BasicLit) {
	switch lit.Kind {
	case token.INT:

	}
}

func (i *Visitor) VisitIfStmt(stmt *ast.IfStmt) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitAssignStmt(stmt *ast.AssignStmt) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitProcCall(call *ast.ProcCall) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitWhileStmt(stmt *ast.WhileStmt) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitCaseStmt(stmt *ast.CaseStmt) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitForStmt(stmt *ast.ForStmt) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitExitStmt(stmt *ast.ExitStmt) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitWithStmt(stmt *ast.WithStmt) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitImport(imp *ast.Import) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitProcDecl(decl *ast.ProcDecl) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitVarDecl(decl *ast.VarDecl) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitConstDecl(decl *ast.ConstDecl) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitTypeDecl(decl *ast.TypeDecl) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitBasicType(basicType *ast.BasicType) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitArrayType(arrayType *ast.ArrayType) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitPointerType(pointerType *ast.PointerType) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitProcType(procType *ast.ProcType) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitRecordType(recordType *ast.RecordType) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitEnumType(enumType *ast.EnumType) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitReceiver(receiver *ast.Receiver) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitProcHead(head *ast.ProcHead) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitProcBody(body *ast.ProcBody) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitFPSection(section *ast.FPSection) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitFormalParams(params *ast.FormalParams) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitDotOp(op *ast.DotOp) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitIndexOp(op *ast.IndexOp) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitTypeGuard(guard *ast.TypeGuard) {
	//TODO implement me
	panic("implement me")
}

func (i *Visitor) VisitPointerDeref(deref *ast.PointerDeref) {
	//TODO implement me
	panic("implement me")
}
