package sema

import (
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type LoopContextAnalyzer struct {
	ctx *report.Context

	label string
}

func NewLoopContextAnalyzer(ctx *report.Context) *LoopContextAnalyzer {
	return &LoopContextAnalyzer{ctx: ctx}
}

func (l *LoopContextAnalyzer) Analyse(unit ast.CompilationUnit) {
	unit.Accept(l)
}

func (l *LoopContextAnalyzer) VisitOberon(oberon *ast.Oberon) any {
	for _, unit := range oberon.Units() {
		unit.Accept(l)
	}

	return oberon
}

func (l *LoopContextAnalyzer) VisitModule(module *ast.Module) any {
	for _, decl := range module.DeclSeq {
		decl.Accept(l)
	}

	for _, stmt := range module.StmtSeq {
		stmt.Accept(l)
	}

	return module
}

func (l *LoopContextAnalyzer) VisitDefinition(def *ast.Definition) any { return def }

func (l *LoopContextAnalyzer) VisitBinaryExpr(expr *ast.BinaryExpr) any { return expr }

func (l *LoopContextAnalyzer) VisitDesignator(dsg *ast.Designator) any { return dsg }

func (l *LoopContextAnalyzer) VisitFunctionCall(call *ast.FunctionCall) any { return call }

func (l *LoopContextAnalyzer) VisitUnaryExpr(expr *ast.UnaryExpr) any { return expr }

func (l *LoopContextAnalyzer) VisitQualifiedIdent(ident *ast.QualifiedIdent) any { return ident }

func (l *LoopContextAnalyzer) VisitSet(set *ast.Set) any { return set }

func (l *LoopContextAnalyzer) VisitBasicLit(lit *ast.BasicLit) any { return lit }

func (l *LoopContextAnalyzer) VisitExprRange(rng *ast.ExprRange) any { return rng }

func (l *LoopContextAnalyzer) VisitNil(n *ast.Nil) any { return n }

func (l *LoopContextAnalyzer) VisitIfStmt(stmt *ast.IfStmt) any {
	for _, s := range stmt.ThenPath {
		s.Accept(l)
	}

	for _, elif := range stmt.ElseIfBranches {
		for _, s := range elif.ThenPath {
			s.Accept(l)
		}
	}

	for _, s := range stmt.ElsePath {
		s.Accept(l)
	}

	return stmt
}

func (l *LoopContextAnalyzer) VisitAssignmentStmt(stmt *ast.AssignmentStmt) any { return stmt }

func (l *LoopContextAnalyzer) VisitReturnStmt(stmt *ast.ReturnStmt) any { return stmt }

func (l *LoopContextAnalyzer) VisitProcedureCall(call *ast.ProcedureCall) any { return call }

func (l *LoopContextAnalyzer) VisitRepeatStmt(stmt *ast.RepeatStmt) any {
	tmp := l.label
	defer func() { l.label = tmp }()

	l.label = newLoopLabel("repeat")
	for _, s := range stmt.StmtSeq {
		s.Accept(l)
	}

	stmt.Label = l.label

	return stmt
}

func (l *LoopContextAnalyzer) VisitWhileStmt(stmt *ast.WhileStmt) any {
	tmp := l.label
	defer func() { l.label = tmp }()

	l.label = newLoopLabel("while")

	for _, s := range stmt.StmtSeq {
		s.Accept(l)
	}

	for _, elif := range stmt.ElsIfs {
		for _, s := range elif.ThenPath {
			s.Accept(l)
		}
	}

	stmt.Label = l.label

	return stmt
}

func (l *LoopContextAnalyzer) VisitLoopStmt(stmt *ast.LoopStmt) any {
	tmp := l.label
	defer func() { l.label = tmp }()

	l.label = newLoopLabel("loop")

	for _, s := range stmt.StmtSeq {
		s.Accept(l)
	}

	stmt.Label = l.label

	return stmt
}

func (l *LoopContextAnalyzer) VisitCaseStmt(stmt *ast.CaseStmt) any {
	for _, caseClause := range stmt.Cases {
		for _, s := range caseClause.StmtSeq {
			s.Accept(l)
		}
	}

	for _, s := range stmt.Else {
		s.Accept(l)
	}

	return stmt
}

func (l *LoopContextAnalyzer) VisitForStmt(stmt *ast.ForStmt) any {
	tmp := l.label
	defer func() { l.label = tmp }()

	l.label = newLoopLabel("for")

	for _, s := range stmt.StmtSeq {
		s.Accept(l)
	}

	stmt.Label = l.label

	return stmt
}

func (l *LoopContextAnalyzer) VisitExitStmt(stmt *ast.ExitStmt) any {
	if l.label == "" {
		l.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "Exit statement outside of loop",
			Range:    l.ctx.Source.Span(l.ctx.FileName, stmt.StartOffset, stmt.EndOffset),
		})
	}

	stmt.Label = l.label

	return stmt
}

func (l *LoopContextAnalyzer) VisitWithStmt(stmt *ast.WithStmt) any {
	for _, arm := range stmt.Arms {
		for _, arm := range arm.StmtSeq {
			arm.Accept(l)
		}
	}

	for _, s := range stmt.Else {
		s.Accept(l)
	}

	return stmt
}

func (l *LoopContextAnalyzer) VisitImport(i *ast.Import) any { return i }

func (l *LoopContextAnalyzer) VisitProcedureDecl(decl *ast.ProcedureDecl) any {
	if decl.Body != nil {
		for _, s := range decl.Body.StmtSeq {
			s.Accept(l)
		}
	}

	return decl
}

func (l *LoopContextAnalyzer) VisitVariableDecl(decl *ast.VariableDecl) any { return decl }

func (l *LoopContextAnalyzer) VisitConstantDecl(decl *ast.ConstantDecl) any { return decl }

func (l *LoopContextAnalyzer) VisitTypeDecl(decl *ast.TypeDecl) any { return decl }

func (l *LoopContextAnalyzer) VisitProcedureHeading(heading *ast.ProcedureHeading) any {
	return heading
}

func (l *LoopContextAnalyzer) VisitBasicType(basic *ast.BasicType) any { return basic }

func (l *LoopContextAnalyzer) VisitArrayType(array *ast.ArrayType) any { return array }

func (l *LoopContextAnalyzer) VisitPointerType(pointer *ast.PointerType) any { return pointer }

func (l *LoopContextAnalyzer) VisitProcedureType(procedure *ast.ProcedureType) any { return procedure }

func (l *LoopContextAnalyzer) VisitRecordType(record *ast.RecordType) any { return record }

func (l *LoopContextAnalyzer) VisitEnumType(enum *ast.EnumType) any { return enum }

func (l *LoopContextAnalyzer) VisitNamedType(named *ast.NamedType) any { return named }

func (l *LoopContextAnalyzer) VisitMetaSection(section *ast.MetaSection) any {
	return section
}

func (l *LoopContextAnalyzer) VisitIdentifierDef(def *ast.IdentifierDef) any { return def }

func (l *LoopContextAnalyzer) VisitIndexOp(op *ast.IndexOp) any { return op }

func (l *LoopContextAnalyzer) VisitPtrDeref(deref *ast.PtrDeref) any { return deref }

func (l *LoopContextAnalyzer) VisitDotOp(op *ast.DotOp) any { return op }

func (l *LoopContextAnalyzer) VisitTypeGuard(guard *ast.TypeGuard) any { return guard }

func (l *LoopContextAnalyzer) VisitProcedureBody(body *ast.ProcedureBody) any {
	for _, decl := range body.DeclSeq {
		decl.Accept(l)
	}

	for _, s := range body.StmtSeq {
		s.Accept(l)
	}

	return body
}

func (l *LoopContextAnalyzer) VisitFieldList(list *ast.FieldList) any { return list }

func (l *LoopContextAnalyzer) VisitLenList(list *ast.LenList) any { return list }

func (l *LoopContextAnalyzer) VisitElseIfBranch(branch *ast.ElseIfBranch) any {
	for _, s := range branch.ThenPath {
		s.Accept(l)
	}

	return branch
}

func (l *LoopContextAnalyzer) VisitReceiver(receiver *ast.Receiver) any { return receiver }

func (l *LoopContextAnalyzer) VisitFPSection(section *ast.FPSection) any { return section }

func (l *LoopContextAnalyzer) VisitFormalParams(params *ast.FormalParams) any { return params }

func (l *LoopContextAnalyzer) VisitCase(c *ast.Case) any {
	for _, s := range c.StmtSeq {
		s.Accept(l)
	}

	return c
}

func (l *LoopContextAnalyzer) VisitLabelRange(labelRange *ast.LabelRange) any { return labelRange }

func (l *LoopContextAnalyzer) VisitGuard(guard *ast.Guard) any {
	for _, s := range guard.StmtSeq {
		s.Accept(l)
	}

	return guard
}

func (l *LoopContextAnalyzer) VisitBadExpr(expr *ast.BadExpr) any { return expr }

func (l *LoopContextAnalyzer) VisitBadDecl(decl *ast.BadDecl) any { return decl }

func (l *LoopContextAnalyzer) VisitBadStmt(stmt *ast.BadStmt) any { return stmt }

func (l *LoopContextAnalyzer) VisitBadType(badType *ast.BadType) any { return badType }
