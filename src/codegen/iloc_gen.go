package codegen

import (
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type ILOCCodeGen struct {
	Instr []Instruction

	ast *ast.Oberon
	env *sema.Scope
}

func (I ILOCCodeGen) VisitIdentifier(ident *ast.Ident) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitBinaryExpr(expr *ast.BinaryExpr) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitDesignator(designator *ast.Designator) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitFuncCall(call *ast.FuncCall) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitUnaryExpr(expr *ast.UnaryExpr) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitQualifiedIdent(ident *ast.QualifiedIdent) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitSet(set *ast.Set) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitBasicLit(lit *ast.BasicLit) {
	switch lit.Kind {
	case token.INT:

	}
}

func (I ILOCCodeGen) VisitIfStmt(stmt *ast.IfStmt) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitAssignStmt(stmt *ast.AssignStmt) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitReturnStmt(stmt *ast.ReturnStmt) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitProcCall(call *ast.ProcCall) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitWhileStmt(stmt *ast.WhileStmt) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitLoopStmt(stmt *ast.LoopStmt) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitCaseStmt(stmt *ast.CaseStmt) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitForStmt(stmt *ast.ForStmt) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitExitStmt(stmt *ast.ExitStmt) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitWithStmt(stmt *ast.WithStmt) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitImport(i *ast.Import) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitProcDecl(decl *ast.ProcDecl) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitVarDecl(decl *ast.VarDecl) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitConstDecl(decl *ast.ConstDecl) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitTypeDecl(decl *ast.TypeDecl) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitBasicType(basicType *ast.BasicType) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitArrayType(arrayType *ast.ArrayType) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitPointerType(pointerType *ast.PointerType) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitProcType(procType *ast.ProcType) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitRecordType(recordType *ast.RecordType) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitEnumType(enumType *ast.EnumType) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitReceiver(receiver *ast.Receiver) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitProcHead(head *ast.ProcHead) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitProcBody(body *ast.ProcBody) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitFPSection(section *ast.FPSection) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitFormalParams(params *ast.FormalParams) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitDotOp(op *ast.DotOp) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitIndexOp(op *ast.IndexOp) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitTypeGuard(guard *ast.TypeGuard) {
	//TODO implement me
	panic("implement me")
}

func (I ILOCCodeGen) VisitPointerDeref(deref *ast.PointerDeref) {
	//TODO implement me
	panic("implement me")
}
