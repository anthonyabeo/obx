package sema

import (
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type FlowChecker struct {
	ctx *report.Context

	label         string
	loopIDCounter int
}

func NewFlowChecker(ctx *report.Context) *FlowChecker {
	return &FlowChecker{ctx: ctx}
}

func (f *FlowChecker) Analyse(unit ast.CompilationUnit) {
	unit.Accept(f)
}

func (f *FlowChecker) VisitOberon(oberon *ast.OberonX) any {
	for _, unit := range oberon.Units {
		unit.Accept(f)
	}

	return oberon
}

func (f *FlowChecker) VisitModule(module *ast.Module) any {
	for _, decl := range module.DeclSeq {
		decl.Accept(f)
	}

	for _, stmt := range module.StmtSeq {
		stmt.Accept(f)
	}

	return module
}

func (f *FlowChecker) VisitDefinition(def *ast.Definition) any { return def }

func (f *FlowChecker) VisitBinaryExpr(expr *ast.BinaryExpr) any { return expr }

func (f *FlowChecker) VisitDesignator(dsg *ast.Designator) any { return dsg }

func (f *FlowChecker) VisitFunctionCall(call *ast.FunctionCall) any { return call }

func (f *FlowChecker) VisitUnaryExpr(expr *ast.UnaryExpr) any { return expr }

func (f *FlowChecker) VisitQualifiedIdent(ident *ast.QualifiedIdent) any { return ident }

func (f *FlowChecker) VisitSet(set *ast.Set) any { return set }

func (f *FlowChecker) VisitBasicLit(lit *ast.BasicLit) any { return lit }

func (f *FlowChecker) VisitExprRange(rng *ast.ExprRange) any { return rng }

func (f *FlowChecker) VisitNil(n *ast.Nil) any { return n }

func (f *FlowChecker) VisitIfStmt(stmt *ast.IfStmt) any {
	for _, s := range stmt.ThenPath {
		s.Accept(f)
	}

	for _, elif := range stmt.ElseIfBranches {
		for _, s := range elif.ThenPath {
			s.Accept(f)
		}
	}

	for _, s := range stmt.ElsePath {
		s.Accept(f)
	}

	return stmt
}

func (f *FlowChecker) VisitAssignmentStmt(stmt *ast.AssignmentStmt) any { return stmt }

func (f *FlowChecker) VisitReturnStmt(stmt *ast.ReturnStmt) any { return stmt }

func (f *FlowChecker) VisitProcedureCall(call *ast.ProcedureCall) any { return call }

func (f *FlowChecker) VisitRepeatStmt(stmt *ast.RepeatStmt) any {
	tmp := f.label
	defer func() { f.label = tmp }()

	f.label = f.newLoopLabel("repeat")
	for _, s := range stmt.StmtSeq {
		s.Accept(f)
	}

	stmt.Label = f.label

	return stmt
}

func (f *FlowChecker) VisitWhileStmt(stmt *ast.WhileStmt) any {
	tmp := f.label
	defer func() { f.label = tmp }()

	f.label = f.newLoopLabel("while")

	f.visitStmtSeq(stmt.StmtSeq)
	for _, elif := range stmt.ElsIfs {
		f.visitStmtSeq(elif.ThenPath)
	}

	stmt.Label = f.label

	return stmt
}

func (f *FlowChecker) VisitLoopStmt(stmt *ast.LoopStmt) any {
	tmp := f.label
	defer func() { f.label = tmp }()

	f.label = f.newLoopLabel("loop")
	f.visitStmtSeq(stmt.StmtSeq)

	stmt.Label = f.label

	return stmt
}

func (f *FlowChecker) VisitCaseStmt(stmt *ast.CaseStmt) any {
	for _, caseClause := range stmt.Cases {
		for _, s := range caseClause.StmtSeq {
			s.Accept(f)
		}
	}

	f.visitStmtSeq(stmt.Else)

	return stmt
}

func (f *FlowChecker) VisitForStmt(stmt *ast.ForStmt) any {
	tmp := f.label
	defer func() { f.label = tmp }()

	f.label = f.newLoopLabel("for")

	f.visitStmtSeq(stmt.StmtSeq)
	stmt.Label = f.label

	return stmt
}

func (f *FlowChecker) VisitExitStmt(stmt *ast.ExitStmt) any {
	if f.label == "" {
		f.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "Exit statement outside of loop",
			Range:    f.ctx.Source.Span(f.ctx.FileName, stmt.StartOffset, stmt.EndOffset),
		})
	}

	stmt.Label = f.label

	return stmt
}

func (f *FlowChecker) VisitWithStmt(stmt *ast.WithStmt) any {
	for _, arm := range stmt.Arms {
		f.visitStmtSeq(arm.StmtSeq)
	}

	f.visitStmtSeq(stmt.Else)

	return stmt
}

func (f *FlowChecker) VisitImport(i *ast.Import) any { return i }

func (f *FlowChecker) VisitProcedureDecl(decl *ast.ProcedureDecl) any {
	if decl.Body != nil {
		f.visitStmtSeq(decl.Body.StmtSeq)
	}

	return decl
}

func (f *FlowChecker) VisitVariableDecl(decl *ast.VariableDecl) any { return decl }

func (f *FlowChecker) VisitConstantDecl(decl *ast.ConstantDecl) any { return decl }

func (f *FlowChecker) VisitTypeDecl(decl *ast.TypeDecl) any { return decl }

func (f *FlowChecker) VisitProcedureHeading(heading *ast.ProcedureHeading) any {
	return heading
}

func (f *FlowChecker) VisitBasicType(basic *ast.BasicType) any { return basic }

func (f *FlowChecker) VisitArrayType(array *ast.ArrayType) any { return array }

func (f *FlowChecker) VisitPointerType(pointer *ast.PointerType) any { return pointer }

func (f *FlowChecker) VisitProcedureType(procedure *ast.ProcedureType) any { return procedure }

func (f *FlowChecker) VisitRecordType(record *ast.RecordType) any { return record }

func (f *FlowChecker) VisitEnumType(enum *ast.EnumType) any { return enum }

func (f *FlowChecker) VisitNamedType(named *ast.NamedType) any { return named }

func (f *FlowChecker) VisitMetaSection(section *ast.MetaSection) any {
	return section
}

func (f *FlowChecker) VisitIdentifierDef(def *ast.IdentifierDef) any { return def }

func (f *FlowChecker) VisitIndexOp(op *ast.IndexOp) any { return op }

func (f *FlowChecker) VisitPtrDeref(deref *ast.PtrDeref) any { return deref }

func (f *FlowChecker) VisitDotOp(op *ast.DotOp) any { return op }

func (f *FlowChecker) VisitTypeGuard(guard *ast.TypeGuard) any { return guard }

func (f *FlowChecker) VisitProcedureBody(body *ast.ProcedureBody) any {
	f.visitStmtSeq(body.StmtSeq)
	f.visitStmtSeq(body.StmtSeq)
	return body
}

func (f *FlowChecker) VisitFieldList(list *ast.FieldList) any { return list }

func (f *FlowChecker) VisitLenList(list *ast.LenList) any { return list }

func (f *FlowChecker) VisitElseIfBranch(branch *ast.ElseIfBranch) any {
	f.visitStmtSeq(branch.ThenPath)
	return branch
}

func (f *FlowChecker) VisitReceiver(receiver *ast.Receiver) any { return receiver }

func (f *FlowChecker) VisitFPSection(section *ast.FPSection) any { return section }

func (f *FlowChecker) VisitFormalParams(params *ast.FormalParams) any { return params }

func (f *FlowChecker) VisitCase(c *ast.Case) any {
	f.visitStmtSeq(c.StmtSeq)
	return c
}

func (f *FlowChecker) VisitLabelRange(labelRange *ast.LabelRange) any { return labelRange }

func (f *FlowChecker) VisitGuard(guard *ast.Guard) any {
	f.visitStmtSeq(guard.StmtSeq)
	return guard
}

func (f *FlowChecker) VisitBadExpr(expr *ast.BadExpr) any { return expr }

func (f *FlowChecker) VisitBadDecl(decl *ast.BadDecl) any { return decl }

func (f *FlowChecker) VisitBadStmt(stmt *ast.BadStmt) any { return stmt }

func (f *FlowChecker) VisitBadType(badType *ast.BadType) any { return badType }

func (f *FlowChecker) visitStmtSeq(stmts []ast.Statement) {
	for _, stmt := range stmts {
		stmt.Accept(f)
	}
}

func (f *FlowChecker) visitDeclSeq(decls []ast.Declaration) {
	for _, decl := range decls {
		decl.Accept(f)
	}
}
