package sema

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type NamesResolver struct {
	ctx *report.Context
	obx *ast.OberonX
}

func NewNameResolver(ctx *report.Context) *NamesResolver {
	return &NamesResolver{ctx: ctx}
}

func (n *NamesResolver) Resolve(unit ast.CompilationUnit) {
	unit.Accept(n)
}

func (n *NamesResolver) VisitOberon(oberon *ast.OberonX) any {
	for _, unit := range oberon.Units {
		unit.Accept(n)
	}

	return oberon
}

func (n *NamesResolver) VisitModule(module *ast.Module) any {
	for _, meta := range module.MetaParams {
		meta.Accept(n)
	}

	for _, imp := range module.ImportList {
		imp.Accept(n)
	}

	for _, decl := range module.DeclSeq {
		decl.Accept(n)
	}

	for _, stmt := range module.StmtSeq {
		stmt.Accept(n)
	}

	return module
}

func (n *NamesResolver) VisitMetaSection(sec *ast.MetaSection) any {
	sec.TyConst.Accept(n)
	for _, id := range sec.Ids {
		id.Accept(n)
	}

	return sec
}

func (n *NamesResolver) VisitDefinition(def *ast.Definition) any {
	for _, imp := range def.ImportList {
		imp.Accept(n)
	}

	for _, decl := range def.DeclSeq {
		decl.Accept(n)
	}

	return def
}

func (n *NamesResolver) VisitIdentifierDef(def *ast.IdentifierDef) any {
	sym := n.ctx.Env.Lookup(def.Name)
	if sym == nil {
		n.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("identifier '%s' not found", def.Name),
			Range:    n.ctx.Source.Span(n.ctx.FileName, def.StartOffset, def.EndOffset),
		})

		return nil
	}

	sym.SetMangledName(ast.Mangle(sym))
	def.Symbol = sym

	return def
}

func (n *NamesResolver) VisitBinaryExpr(expr *ast.BinaryExpr) any {
	expr.Left.Accept(n)
	expr.Right.Accept(n)

	return expr
}

func (n *NamesResolver) VisitDesignator(dsg *ast.Designator) any {
	dsg.QIdent.Accept(n)
	for _, selector := range dsg.Select {
		selector.Accept(n)
	}

	return dsg
}

func (n *NamesResolver) VisitFunctionCall(call *ast.FunctionCall) any {
	call.Callee.Accept(n)
	for _, param := range call.ActualParams {
		param.Accept(n)
	}

	return call
}

func (n *NamesResolver) VisitUnaryExpr(expr *ast.UnaryExpr) any {
	expr.Operand.Accept(n)
	return expr
}

func (n *NamesResolver) VisitQualifiedIdent(ident *ast.QualifiedIdent) any {
	var sym ast.Symbol
	if ident.Prefix != "" {
		if n.ctx.Envs[ident.Prefix] == nil {
			n.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("environment for module '%s' not found", ident.Prefix),
				Range:    n.ctx.Source.Span(n.ctx.FileName, ident.StartOffset, ident.EndOffset),
			})
			return ident
		}

		sym = n.ctx.Envs[ident.Prefix].Lookup(ident.Name)
		if sym == nil || sym.Props() != ast.Exported {
			n.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("identifier '%s' not found or is not exported", ident.Name),
				Range:    n.ctx.Source.Span(n.ctx.FileName, ident.StartOffset, ident.EndOffset),
			})
		}
	} else {
		sym = n.ctx.Env.Lookup(ident.Name)
		if sym == nil {
			n.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("identifier '%s' not found", ident.Name),
				Range:    n.ctx.Source.Span(n.ctx.FileName, ident.StartOffset, ident.EndOffset),
			})
		}
	}

	ident.Symbol = sym
	return ident
}

func (n *NamesResolver) VisitSet(set *ast.Set) any {
	for _, expr := range set.Elem {
		expr.Accept(n)
	}

	return set
}

func (n *NamesResolver) VisitBasicLit(lit *ast.BasicLit) any { return lit }

func (n *NamesResolver) VisitExprRange(rng *ast.ExprRange) any {
	rng.Low.Accept(n)
	rng.High.Accept(n)
	return rng
}

func (n *NamesResolver) VisitNil(n2 *ast.Nil) any { return n2 }

func (n *NamesResolver) VisitIfStmt(stmt *ast.IfStmt) any {
	stmt.BoolExpr.Accept(n)
	for _, statement := range stmt.ThenPath {
		statement.Accept(n)
	}

	for _, branch := range stmt.ElseIfBranches {
		branch.Accept(n)
	}

	for _, statement := range stmt.ElsePath {
		statement.Accept(n)
	}

	return stmt
}

func (n *NamesResolver) VisitAssignmentStmt(stmt *ast.AssignmentStmt) any {
	stmt.LValue.Accept(n)
	stmt.RValue.Accept(n)
	return stmt
}

func (n *NamesResolver) VisitReturnStmt(stmt *ast.ReturnStmt) any {
	if stmt.Value != nil {
		stmt.Value.Accept(n)
	}

	return stmt
}

func (n *NamesResolver) VisitProcedureCall(call *ast.ProcedureCall) any {
	call.Callee.Accept(n)

	for _, arg := range call.ActualParams {
		arg.Accept(n)
	}

	return call
}

func (n *NamesResolver) VisitRepeatStmt(stmt *ast.RepeatStmt) any {
	for _, statement := range stmt.StmtSeq {
		statement.Accept(n)
	}

	stmt.BoolExpr.Accept(n)
	return stmt
}

func (n *NamesResolver) VisitWhileStmt(stmt *ast.WhileStmt) any {
	stmt.BoolExpr.Accept(n)
	for _, statement := range stmt.StmtSeq {
		statement.Accept(n)
	}

	for _, elsIf := range stmt.ElsIfs {
		elsIf.Accept(n)
	}

	return stmt
}

func (n *NamesResolver) VisitLoopStmt(stmt *ast.LoopStmt) any {
	for _, statement := range stmt.StmtSeq {
		statement.Accept(n)
	}

	return stmt
}

func (n *NamesResolver) VisitCaseStmt(stmt *ast.CaseStmt) any {
	stmt.Expr.Accept(n)
	for _, c := range stmt.Cases {
		c.Accept(n)
	}

	for _, statement := range stmt.Else {
		statement.Accept(n)
	}

	return stmt
}

func (n *NamesResolver) VisitForStmt(stmt *ast.ForStmt) any {
	stmt.CtlVar.Accept(n)
	stmt.InitVal.Accept(n)
	stmt.FinalVal.Accept(n)
	if stmt.By != nil {
		stmt.By.Accept(n)
	}

	for _, statement := range stmt.StmtSeq {
		statement.Accept(n)
	}

	return stmt
}

func (n *NamesResolver) VisitExitStmt(stmt *ast.ExitStmt) any { return stmt }

func (n *NamesResolver) VisitWithStmt(stmt *ast.WithStmt) any {
	for _, arm := range stmt.Arms {
		arm.Accept(n)
	}

	for _, statement := range stmt.Else {
		statement.Accept(n)
	}

	return stmt
}

func (n *NamesResolver) VisitImport(i *ast.Import) any { return i }

func (n *NamesResolver) VisitProcedureDecl(decl *ast.ProcedureDecl) any {
	tmp := n.ctx.Env
	n.ctx.Env = decl.Env
	defer func() { n.ctx.Env = tmp }()

	decl.Head.Accept(n)
	if decl.Body != nil {
		decl.Body.Accept(n)
	}

	return decl
}

func (n *NamesResolver) VisitVariableDecl(decl *ast.VariableDecl) any {
	for _, def := range decl.IdentList {
		def.Accept(n)
	}

	decl.Type.Accept(n)

	return decl
}

func (n *NamesResolver) VisitConstantDecl(decl *ast.ConstantDecl) any {
	decl.Name.Accept(n)
	decl.Value.Accept(n)
	return decl
}

func (n *NamesResolver) VisitTypeDecl(decl *ast.TypeDecl) any {
	decl.Name.Accept(n)
	decl.DenotedType.Accept(n)
	return decl
}

func (n *NamesResolver) VisitProcedureHeading(heading *ast.ProcedureHeading) any {
	if heading.Rcv != nil {
		heading.Rcv.Accept(n)
	}

	heading.Name.Accept(n)

	if heading.FP != nil {
		heading.FP.Accept(n)
	}

	return heading
}

func (n *NamesResolver) VisitProcedureBody(body *ast.ProcedureBody) any {
	for _, decl := range body.DeclSeq {
		decl.Accept(n)
	}

	for _, stmt := range body.StmtSeq {
		stmt.Accept(n)
	}

	return body
}

func (n *NamesResolver) VisitBasicType(ty *ast.BasicType) any { return ty }

func (n *NamesResolver) VisitArrayType(ty *ast.ArrayType) any {
	ty.LenList.Accept(n)
	ty.ElemType.Accept(n)
	return ty
}

func (n *NamesResolver) VisitPointerType(ty *ast.PointerType) any {
	ty.Base.Accept(n)
	return ty
}

func (n *NamesResolver) VisitProcedureType(ty *ast.ProcedureType) any {
	if ty.FP != nil {
		ty.FP.Accept(n)
	}

	return ty
}

func (n *NamesResolver) VisitRecordType(ty *ast.RecordType) any {
	if ty.Base != nil {
		ty.Base.Accept(n)
	}

	for _, field := range ty.Fields {
		field.Accept(n)
	}

	return ty
}

func (n *NamesResolver) VisitEnumType(ty *ast.EnumType) any { return ty }

func (n *NamesResolver) VisitNamedType(ty *ast.NamedType) any {
	ty.Name.Accept(n)
	return ty
}

func (n *NamesResolver) VisitIndexOp(op *ast.IndexOp) any {
	for _, expr := range op.List {
		expr.Accept(n)
	}

	return op
}

func (n *NamesResolver) VisitPtrDeref(deref *ast.PtrDeref) any { return deref }

func (n *NamesResolver) VisitDotOp(op *ast.DotOp) any { return op }

func (n *NamesResolver) VisitTypeGuard(guard *ast.TypeGuard) any {
	guard.Ty.Accept(n)
	return guard
}

func (n *NamesResolver) VisitFieldList(list *ast.FieldList) any {
	for _, def := range list.List {
		def.Accept(n)
	}

	list.Type.Accept(n)

	return list
}

func (n *NamesResolver) VisitLenList(list *ast.LenList) any {
	for _, expr := range list.List {
		expr.Accept(n)
	}

	return list
}

func (n *NamesResolver) VisitElseIfBranch(br *ast.ElseIfBranch) any {
	br.BoolExpr.Accept(n)
	for _, stmt := range br.ThenPath {
		stmt.Accept(n)
	}

	return br
}

func (n *NamesResolver) VisitReceiver(rcv *ast.Receiver) any {
	rcv.Type.Accept(n)
	sym := n.ctx.Env.Lookup(rcv.Var)
	if sym == nil {
		n.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("unknown name %s in receiver", rcv.Var),
			Range:    n.ctx.Source.Span(n.ctx.FileName, rcv.StartOffset, rcv.EndOffset),
		})

		return nil
	}

	sym.SetMangledName(ast.Mangle(sym))

	return rcv
}

func (n *NamesResolver) VisitFPSection(sec *ast.FPSection) any {
	for _, name := range sec.Names {
		name.Accept(n)
	}

	sec.Type.Accept(n)
	return sec
}

func (n *NamesResolver) VisitFormalParams(params *ast.FormalParams) any {
	for _, param := range params.Params {
		param.Accept(n)
	}

	params.RetType.Accept(n)

	return params
}

func (n *NamesResolver) VisitCase(c *ast.Case) any {
	for _, labelRange := range c.CaseLabelList {
		labelRange.Accept(n)
	}

	for _, stmt := range c.StmtSeq {
		stmt.Accept(n)
	}

	return c
}

func (n *NamesResolver) VisitLabelRange(rng *ast.LabelRange) any {
	rng.Low.Accept(n)
	rng.High.Accept(n)

	return rng
}

func (n *NamesResolver) VisitGuard(guard *ast.Guard) any {
	guard.Expr.Accept(n)
	guard.Type.Accept(n)
	for _, stmt := range guard.StmtSeq {
		stmt.Accept(n)
	}

	return guard
}

func (n *NamesResolver) VisitBadExpr(expr *ast.BadExpr) any { return expr }

func (n *NamesResolver) VisitBadDecl(decl *ast.BadDecl) any { return decl }

func (n *NamesResolver) VisitBadStmt(stmt *ast.BadStmt) any { return stmt }

func (n *NamesResolver) VisitBadType(ty *ast.BadType) any { return ty }
