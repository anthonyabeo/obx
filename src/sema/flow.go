package sema

import (
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type FlowControlAnalyzer struct {
	ctx *report.Context

	label string
}

func NewFlowControlAnalyzer(ctx *report.Context) *FlowControlAnalyzer {
	return &FlowControlAnalyzer{ctx: ctx}
}

func (l *FlowControlAnalyzer) Analyse(unit ast.CompilationUnit) {
	unit.Accept(l)
}

func (l *FlowControlAnalyzer) VisitOberon(oberon *ast.OberonX) any {
	for _, unit := range oberon.Units {
		unit.Accept(l)
	}

	return oberon
}

func (l *FlowControlAnalyzer) VisitModule(module *ast.Module) any {
	for _, decl := range module.DeclSeq {
		decl.Accept(l)
	}

	for _, stmt := range module.StmtSeq {
		stmt.Accept(l)
	}

	return module
}

func (l *FlowControlAnalyzer) VisitDefinition(def *ast.Definition) any { return def }

func (l *FlowControlAnalyzer) VisitBinaryExpr(expr *ast.BinaryExpr) any { return expr }

func (l *FlowControlAnalyzer) VisitDesignator(dsg *ast.Designator) any { return dsg }

func (l *FlowControlAnalyzer) VisitFunctionCall(call *ast.FunctionCall) any { return call }

func (l *FlowControlAnalyzer) VisitUnaryExpr(expr *ast.UnaryExpr) any { return expr }

func (l *FlowControlAnalyzer) VisitQualifiedIdent(ident *ast.QualifiedIdent) any { return ident }

func (l *FlowControlAnalyzer) VisitSet(set *ast.Set) any { return set }

func (l *FlowControlAnalyzer) VisitBasicLit(lit *ast.BasicLit) any { return lit }

func (l *FlowControlAnalyzer) VisitExprRange(rng *ast.ExprRange) any { return rng }

func (l *FlowControlAnalyzer) VisitNil(n *ast.Nil) any { return n }

func (l *FlowControlAnalyzer) VisitIfStmt(stmt *ast.IfStmt) any {
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

func (l *FlowControlAnalyzer) VisitAssignmentStmt(stmt *ast.AssignmentStmt) any { return stmt }

func (l *FlowControlAnalyzer) VisitReturnStmt(stmt *ast.ReturnStmt) any { return stmt }

func (l *FlowControlAnalyzer) VisitProcedureCall(call *ast.ProcedureCall) any { return call }

func (l *FlowControlAnalyzer) VisitRepeatStmt(stmt *ast.RepeatStmt) any {
	tmp := l.label
	defer func() { l.label = tmp }()

	l.label = newLoopLabel("repeat")
	for _, s := range stmt.StmtSeq {
		s.Accept(l)
	}

	stmt.Label = l.label

	return stmt
}

func (l *FlowControlAnalyzer) VisitWhileStmt(stmt *ast.WhileStmt) any {
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

func (l *FlowControlAnalyzer) VisitLoopStmt(stmt *ast.LoopStmt) any {
	tmp := l.label
	defer func() { l.label = tmp }()

	l.label = newLoopLabel("loop")

	for _, s := range stmt.StmtSeq {
		s.Accept(l)
	}

	stmt.Label = l.label

	return stmt
}

func (l *FlowControlAnalyzer) VisitCaseStmt(stmt *ast.CaseStmt) any {
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

func (l *FlowControlAnalyzer) VisitForStmt(stmt *ast.ForStmt) any {
	tmp := l.label
	defer func() { l.label = tmp }()

	l.label = newLoopLabel("for")

	for _, s := range stmt.StmtSeq {
		s.Accept(l)
	}

	stmt.Label = l.label

	return stmt
}

func (l *FlowControlAnalyzer) VisitExitStmt(stmt *ast.ExitStmt) any {
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

func (l *FlowControlAnalyzer) VisitWithStmt(stmt *ast.WithStmt) any {
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

func (l *FlowControlAnalyzer) VisitImport(i *ast.Import) any { return i }

func (l *FlowControlAnalyzer) VisitProcedureDecl(decl *ast.ProcedureDecl) any {
	if decl.Body != nil {
		for _, s := range decl.Body.StmtSeq {
			s.Accept(l)
		}
	}

	return decl
}

func (l *FlowControlAnalyzer) VisitVariableDecl(decl *ast.VariableDecl) any { return decl }

func (l *FlowControlAnalyzer) VisitConstantDecl(decl *ast.ConstantDecl) any { return decl }

func (l *FlowControlAnalyzer) VisitTypeDecl(decl *ast.TypeDecl) any { return decl }

func (l *FlowControlAnalyzer) VisitProcedureHeading(heading *ast.ProcedureHeading) any {
	return heading
}

func (l *FlowControlAnalyzer) VisitBasicType(basic *ast.BasicType) any { return basic }

func (l *FlowControlAnalyzer) VisitArrayType(array *ast.ArrayType) any { return array }

func (l *FlowControlAnalyzer) VisitPointerType(pointer *ast.PointerType) any { return pointer }

func (l *FlowControlAnalyzer) VisitProcedureType(procedure *ast.ProcedureType) any { return procedure }

func (l *FlowControlAnalyzer) VisitRecordType(record *ast.RecordType) any { return record }

func (l *FlowControlAnalyzer) VisitEnumType(enum *ast.EnumType) any { return enum }

func (l *FlowControlAnalyzer) VisitNamedType(named *ast.NamedType) any { return named }

func (l *FlowControlAnalyzer) VisitMetaSection(section *ast.MetaSection) any {
	return section
}

func (l *FlowControlAnalyzer) VisitIdentifierDef(def *ast.IdentifierDef) any { return def }

func (l *FlowControlAnalyzer) VisitIndexOp(op *ast.IndexOp) any { return op }

func (l *FlowControlAnalyzer) VisitPtrDeref(deref *ast.PtrDeref) any { return deref }

func (l *FlowControlAnalyzer) VisitDotOp(op *ast.DotOp) any { return op }

func (l *FlowControlAnalyzer) VisitTypeGuard(guard *ast.TypeGuard) any { return guard }

func (l *FlowControlAnalyzer) VisitProcedureBody(body *ast.ProcedureBody) any {
	for _, decl := range body.DeclSeq {
		decl.Accept(l)
	}

	for _, s := range body.StmtSeq {
		s.Accept(l)
	}

	return body
}

func (l *FlowControlAnalyzer) VisitFieldList(list *ast.FieldList) any { return list }

func (l *FlowControlAnalyzer) VisitLenList(list *ast.LenList) any { return list }

func (l *FlowControlAnalyzer) VisitElseIfBranch(branch *ast.ElseIfBranch) any {
	for _, s := range branch.ThenPath {
		s.Accept(l)
	}

	return branch
}

func (l *FlowControlAnalyzer) VisitReceiver(receiver *ast.Receiver) any { return receiver }

func (l *FlowControlAnalyzer) VisitFPSection(section *ast.FPSection) any { return section }

func (l *FlowControlAnalyzer) VisitFormalParams(params *ast.FormalParams) any { return params }

func (l *FlowControlAnalyzer) VisitCase(c *ast.Case) any {
	for _, s := range c.StmtSeq {
		s.Accept(l)
	}

	return c
}

func (l *FlowControlAnalyzer) VisitLabelRange(labelRange *ast.LabelRange) any { return labelRange }

func (l *FlowControlAnalyzer) VisitGuard(guard *ast.Guard) any {
	for _, s := range guard.StmtSeq {
		s.Accept(l)
	}

	return guard
}

func (l *FlowControlAnalyzer) VisitBadExpr(expr *ast.BadExpr) any { return expr }

func (l *FlowControlAnalyzer) VisitBadDecl(decl *ast.BadDecl) any { return decl }

func (l *FlowControlAnalyzer) VisitBadStmt(stmt *ast.BadStmt) any { return stmt }

func (l *FlowControlAnalyzer) VisitBadType(badType *ast.BadType) any { return badType }
