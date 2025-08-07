package ast

import (
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type AstPrinter struct {
	obx *ast.OberonX
	ctx *report.Context
}

func NewAstPrinter(obx *ast.OberonX, ctx *report.Context) *AstPrinter {

	return &AstPrinter{obx: obx, ctx: ctx}
}

func (v *AstPrinter) Print() any {
	return v.VisitOberon(v.obx)
}

func (v *AstPrinter) VisitOberon(n *ast.OberonX) any {
	var list []ast.CompilationUnit
	for _, unit := range n.Units {
		list = append(list, unit)
	}

	return map[string]any{
		"type":  "OberonX",
		"units": visitList(list, v),
	}
}

func (v *AstPrinter) VisitModule(n *ast.Module) any {
	return map[string]any{
		"type":         "Module",
		"name":         n.BName,
		"meta":         visitList(n.MetaParams, v),
		"imports":      visitList(n.ImportList, v),
		"declarations": visitList(n.DeclSeq, v),
		"statements":   visitList(n.StmtSeq, v),
		"range":        formatRange(v.ctx.Source.Span(v.ctx.FileName, n.StartOffset, n.EndOffset)),
	}
}

func (v *AstPrinter) VisitMetaSection(n *ast.MetaSection) any {
	var mode string
	if n.Mode != token.ILLEGAL {
		mode = n.Mode.String()
	}

	return map[string]any{
		"type":      "MetaSection",
		"mode":      mode,
		"names":     n.Ids,
		"typeConst": n.TyConst.Accept(v),
		"range":     formatRange(v.ctx.Source.Span(v.ctx.FileName, n.StartOffset, n.EndOffset)),
	}
}

func (v *AstPrinter) VisitDefinition(n *ast.Definition) any {
	return map[string]any{
		"type":         "Definition",
		"name":         n.Name,
		"imports":      visitList(n.ImportList, v),
		"declarations": visitList(n.DeclSeq, v),
		"range":        formatRange(v.ctx.Source.Span(v.ctx.FileName, n.StartOffset, n.EndOffset)),
	}
}

func (v *AstPrinter) VisitIdentifierDef(n *ast.IdentifierDef) any {
	return map[string]any{
		"type":  "IdentifierDef",
		"name":  n.Name,
		"props": n.Props.String(),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, n.StartOffset, n.EndOffset)),
	}
}

func (v *AstPrinter) VisitBinaryExpr(n *ast.BinaryExpr) any {
	return map[string]any{
		"type":  "BinaryExpr",
		"op":    n.Op.String(),
		"left":  n.Left.Accept(v),
		"right": n.Right.Accept(v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, n.StartOffset, n.EndOffset)),
	}
}

func (v *AstPrinter) VisitBasicLit(n *ast.BasicLit) any {
	return map[string]any{
		"type":  "BasicLit",
		"kind":  n.Kind.String(),
		"value": n.Val,
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, n.StartOffset, n.EndOffset)),
	}
}

func (v *AstPrinter) VisitDesignator(d *ast.Designator) any {
	return map[string]any{
		"type":      "Designator",
		"base":      d.QIdent.Accept(v),
		"selectors": visitSelector(d.Select, v),
		"range":     formatRange(v.ctx.Source.Span(v.ctx.FileName, d.StartOffset, d.EndOffset)),
	}
}
func (v *AstPrinter) VisitFunctionCall(f *ast.FunctionCall) any {
	return map[string]any{
		"type":      "FunctionCall",
		"callee":    f.Callee.Accept(v),
		"arguments": visitList(f.ActualParams, v),
		"range":     formatRange(v.ctx.Source.Span(v.ctx.FileName, f.StartOffset, f.EndOffset)),
	}
}

func (v *AstPrinter) VisitUnaryExpr(e *ast.UnaryExpr) any {
	return map[string]any{
		"type":    "UnaryExpr",
		"op":      e.Op.String(),
		"operand": e.Operand.Accept(v),
		"range":   formatRange(v.ctx.Source.Span(v.ctx.FileName, e.StartOffset, e.EndOffset)),
	}
}

func (v *AstPrinter) VisitQualifiedIdent(q *ast.QualifiedIdent) any {
	return map[string]any{
		"type":   "QualifiedIdentifier",
		"module": q.Prefix,
		"name":   q.Name,
		"range":  formatRange(v.ctx.Source.Span(v.ctx.FileName, q.StartOffset, q.EndOffset)),
	}
}

func (v *AstPrinter) VisitSet(s *ast.Set) any {
	return map[string]any{
		"type":  "Set",
		"elems": visitList(s.Elem, v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, s.StartOffset, s.EndOffset)),
	}
}

func (v *AstPrinter) VisitExprRange(r *ast.ExprRange) any {
	return map[string]any{
		"type":  "ExprRange",
		"low":   r.Low.Accept(v),
		"high":  r.High.Accept(v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, r.StartOffset, r.EndOffset)),
	}
}

func (v *AstPrinter) VisitNil(n *ast.Nil) any {
	return map[string]any{
		"type":  "Nil",
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, n.StartOffset, n.EndOffset)),
	}
}

func (v *AstPrinter) VisitIfStmt(stmt *ast.IfStmt) any {
	return map[string]any{
		"type":           "IfStmt",
		"condition":      stmt.BoolExpr.Accept(v),
		"thenBody":       visitList(stmt.ThenPath, v),
		"elseifBranches": visitList(stmt.ElseIfBranches, v),
		"elseBody":       visitList(stmt.ElsePath, v),
		"range":          formatRange(v.ctx.Source.Span(v.ctx.FileName, stmt.StartOffset, stmt.EndOffset)),
	}
}

func (v *AstPrinter) VisitAssignmentStmt(s *ast.AssignmentStmt) any {
	return map[string]any{
		"type":  "AssignmentStmt",
		"lhs":   s.LValue.Accept(v),
		"rhs":   s.RValue.Accept(v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, s.StartOffset, s.EndOffset)),
	}
}

func (v *AstPrinter) VisitReturnStmt(s *ast.ReturnStmt) any {
	var value any
	if s.Value != nil {
		value = s.Value.Accept(v)
	}

	return map[string]any{
		"type":  "ReturnStmt",
		"value": value,
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, s.StartOffset, s.EndOffset)),
	}
}

func (v *AstPrinter) VisitProcedureCall(p *ast.ProcedureCall) any {
	return map[string]any{
		"type":   "ProcedureCall",
		"callee": p.Callee.Accept(v),
		"args":   visitList(p.ActualParams, v),
		"range":  formatRange(v.ctx.Source.Span(v.ctx.FileName, p.StartOffset, p.EndOffset)),
	}
}

func (v *AstPrinter) VisitRepeatStmt(s *ast.RepeatStmt) any {
	return map[string]any{
		"type":      "RepeatStmt",
		"body":      visitList(s.StmtSeq, v),
		"condition": s.BoolExpr.Accept(v),
		"range":     formatRange(v.ctx.Source.Span(v.ctx.FileName, s.StartOffset, s.EndOffset)),
	}
}

func (v *AstPrinter) VisitWhileStmt(s *ast.WhileStmt) any {
	return map[string]any{
		"type":      "WhileStmt",
		"condition": s.BoolExpr.Accept(v),
		"body":      visitList(s.StmtSeq, v),
		"elseifs":   visitList(s.ElsIfs, v),
		"range":     formatRange(v.ctx.Source.Span(v.ctx.FileName, s.StartOffset, s.EndOffset)),
	}
}

func (v *AstPrinter) VisitLoopStmt(s *ast.LoopStmt) any {
	return map[string]any{
		"type":  "LoopStmt",
		"body":  visitList(s.StmtSeq, v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, s.StartOffset, s.EndOffset)),
	}
}

func (v *AstPrinter) VisitCaseStmt(s *ast.CaseStmt) any {
	return map[string]any{
		"type":  "CaseStmt",
		"expr":  s.Expr.Accept(v),
		"cases": visitList(s.Cases, v),
		"else":  visitList(s.Else, v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, s.StartOffset, s.EndOffset)),
	}
}

func (v *AstPrinter) VisitForStmt(s *ast.ForStmt) any {
	var step any
	if s.By != nil {
		step = s.By.Accept(v)
	}
	return map[string]any{
		"type":     "ForStmt",
		"ctrlVar":  s.CtlVar,
		"initVal":  s.InitVal.Accept(v),
		"finalVal": s.FinalVal.Accept(v),
		"step":     step,
		"body":     visitList(s.StmtSeq, v),
		"range":    formatRange(v.ctx.Source.Span(v.ctx.FileName, s.StartOffset, s.EndOffset)),
	}
}

func (v *AstPrinter) VisitExitStmt(s *ast.ExitStmt) any {
	return map[string]any{
		"type":  "ExitStmt",
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, s.StartOffset, s.EndOffset)),
	}
}

func (v *AstPrinter) VisitWithStmt(s *ast.WithStmt) any {
	return map[string]any{
		"type":   "WithStmt",
		"guards": visitList(s.Arms, v),
		"range":  formatRange(v.ctx.Source.Span(v.ctx.FileName, s.StartOffset, s.EndOffset)),
	}
}

func (v *AstPrinter) VisitImport(i *ast.Import) any {
	return map[string]any{
		"type":  "Import",
		"alias": i.Alias,
		"name":  i.Name,
		"path":  i.ImportPath,
		"meta":  visitList(i.Meta, v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, i.StartOffset, i.EndOffset)),
	}
}

func (v *AstPrinter) VisitProcedureDecl(p *ast.ProcedureDecl) any {
	return map[string]any{
		"type":    "ProcedureDecl",
		"heading": p.Head.Accept(v),
		"body":    p.Body.Accept(v),
		"range":   formatRange(v.ctx.Source.Span(v.ctx.FileName, p.StartOffset, p.EndOffset)),
	}
}

func (v *AstPrinter) VisitVariableDecl(d *ast.VariableDecl) any {
	return map[string]any{
		"type":     "VariableDecl",
		"names":    visitList(d.IdentList, v),
		"typeExpr": d.Type.Accept(v),
		"range":    formatRange(v.ctx.Source.Span(v.ctx.FileName, d.StartOffset, d.EndOffset)),
	}
}

func (v *AstPrinter) VisitConstantDecl(d *ast.ConstantDecl) any {
	return map[string]any{
		"type":  "ConstantDecl",
		"name":  d.Name.Accept(v),
		"value": d.Value.Accept(v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, d.StartOffset, d.EndOffset)),
	}
}

func (v *AstPrinter) VisitTypeDecl(d *ast.TypeDecl) any {
	return map[string]any{
		"type":     "TypeDecl",
		"name":     d.Name.Accept(v),
		"typeExpr": d.DenotedType.Accept(v),
		"range":    formatRange(v.ctx.Source.Span(v.ctx.FileName, d.StartOffset, d.EndOffset)),
	}
}
func (v *AstPrinter) VisitProcedureHeading(h *ast.ProcedureHeading) any {
	var params, rcv any
	if h.FP != nil {
		params = h.FP.Accept(v)
	}

	if h.Rcv != nil {
		rcv = h.Rcv.Accept(v)
	}

	return map[string]any{
		"type":     "ProcedureHeading",
		"name":     h.Name.Accept(v),
		"receiver": rcv,
		"params":   params,
		"range":    formatRange(v.ctx.Source.Span(v.ctx.FileName, h.StartOffset, h.EndOffset)),
	}
}

func (v *AstPrinter) VisitBasicType(t *ast.BasicType) any {
	return map[string]any{
		"type":  "BasicType",
		"name":  t.Kind.String(),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, t.StartOffset, t.EndOffset)),
	}
}

func (v *AstPrinter) VisitArrayType(t *ast.ArrayType) any {
	var lenList any
	if t.LenList != nil {
		lenList = t.LenList.Accept(v)
	}

	return map[string]any{
		"type":    "ArrayType",
		"length":  lenList,
		"element": t.ElemType.Accept(v),
		"range":   formatRange(v.ctx.Source.Span(v.ctx.FileName, t.StartOffset, t.EndOffset)),
	}
}

func (v *AstPrinter) VisitPointerType(t *ast.PointerType) any {
	return map[string]any{
		"type":  "PointerType",
		"base":  t.Base.Accept(v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, t.StartOffset, t.EndOffset)),
	}
}

func (v *AstPrinter) VisitProcedureType(t *ast.ProcedureType) any {
	var params any
	if t.FP != nil {
		params = t.FP.Accept(v)
	}

	return map[string]any{
		"type":   "ProcedureType",
		"params": params,
		"range":  formatRange(v.ctx.Source.Span(v.ctx.FileName, t.StartOffset, t.EndOffset)),
	}
}

func (v *AstPrinter) VisitRecordType(t *ast.RecordType) any {
	var super any
	if t.Base != nil {
		super = t.Base.Accept(v)
	}

	return map[string]any{
		"type":   "RecordType",
		"super":  super,
		"fields": visitList(t.Fields, v),
		"range":  formatRange(v.ctx.Source.Span(v.ctx.FileName, t.StartOffset, t.EndOffset)),
	}
}

func (v *AstPrinter) VisitEnumType(t *ast.EnumType) any {
	return map[string]any{
		"type":    "EnumType",
		"options": t.Variants,
		"range":   formatRange(v.ctx.Source.Span(v.ctx.FileName, t.StartOffset, t.EndOffset)),
	}
}

func (v *AstPrinter) VisitNamedType(t *ast.NamedType) any {
	return map[string]any{
		"type":  "NamedType",
		"name":  t.Name.Accept(v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, t.StartOffset, t.EndOffset)),
	}
}

func (v *AstPrinter) VisitIndexOp(op *ast.IndexOp) any {
	return map[string]any{
		"type":  "IndexOp",
		"list":  visitList(op.List, v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, op.StartOffset, op.EndOffset)),
	}
}

func (v *AstPrinter) VisitPtrDeref(deref *ast.PtrDeref) any {
	return map[string]any{
		"type":  "PtrDeref",
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, deref.StartOffset, deref.EndOffset)),
	}
}

func (v *AstPrinter) VisitDotOp(op *ast.DotOp) any {
	return map[string]any{
		"type":  "DotOp",
		"field": op.Field,
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, op.StartOffset, op.EndOffset)),
	}
}

func (v *AstPrinter) VisitTypeGuard(guard *ast.TypeGuard) any {
	return map[string]any{
		"type":  "TypeGuard",
		"expr":  guard.Ty.Accept(v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, guard.StartOffset, guard.EndOffset)),
	}
}

func (v *AstPrinter) VisitBadExpr(expr *ast.BadExpr) any {
	return map[string]any{
		"type":  "BadExpr",
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, expr.StartOffset, expr.EndOffset)),
	}
}

func (v *AstPrinter) VisitBadDecl(decl *ast.BadDecl) any {
	return map[string]any{
		"type":  "BadDecl",
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, decl.StartOffset, decl.EndOffset)),
	}
}

func (v *AstPrinter) VisitBadStmt(stmt *ast.BadStmt) any {
	return map[string]any{
		"type":  "BadStmt",
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, stmt.StartOffset, stmt.EndOffset)),
	}
}

func (v *AstPrinter) VisitBadType(ty *ast.BadType) any {
	return map[string]any{
		"type":  "BadType",
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, ty.StartOffset, ty.EndOffset)),
	}
}

func (v *AstPrinter) VisitFieldList(fl *ast.FieldList) any {
	return map[string]any{
		"type":     "FieldList",
		"fields":   visitList(fl.List, v),
		"typeExpr": fl.Type.Accept(v),
		"range":    formatRange(v.ctx.Source.Span(v.ctx.FileName, fl.StartOffset, fl.EndOffset)),
	}
}

func (v *AstPrinter) VisitLenList(ll *ast.LenList) any {
	return map[string]any{
		"type":     "LenList",
		"modifier": ll.Modifier.String(),
		"elements": visitList(ll.List, v),
		"range":    formatRange(v.ctx.Source.Span(v.ctx.FileName, ll.StartOffset, ll.EndOffset)),
	}
}

func (v *AstPrinter) VisitElseIfBranch(br *ast.ElseIfBranch) any {
	return map[string]any{
		"type":      "ElseIfBranch",
		"condition": br.BoolExpr.Accept(v),
		"body":      visitList(br.ThenPath, v),
		"range":     formatRange(v.ctx.Source.Span(v.ctx.FileName, br.StartOffset, br.EndOffset)),
	}
}

func (v *AstPrinter) VisitFormalParams(fp *ast.FormalParams) any {
	return map[string]any{
		"type":    "FormalParams",
		"params":  visitList(fp.Params, v),
		"retType": fp.RetType.Accept(v),
		"range":   formatRange(v.ctx.Source.Span(v.ctx.FileName, fp.StartOffset, fp.EndOffset)),
	}
}

func (v *AstPrinter) VisitReceiver(r *ast.Receiver) any {
	var mode string
	if r.Kind != token.ILLEGAL {
		mode = r.Kind.String()
	}

	return map[string]any{
		"type":     "Receiver",
		"modifier": mode,
		"name":     r.Name.Accept(v),
		"typeExpr": r.Type.Accept(v),
		"range":    formatRange(v.ctx.Source.Span(v.ctx.FileName, r.StartOffset, r.EndOffset)),
	}
}

func (v *AstPrinter) VisitFPSection(sec *ast.FPSection) any {
	var mode string
	if sec.Kind != token.ILLEGAL {
		mode = sec.Kind.String()
	}

	return map[string]any{
		"type":     "FPSection",
		"modifier": mode,
		"names":    sec.Names,
		"typeExpr": sec.Type.Accept(v),
		"range":    formatRange(v.ctx.Source.Span(v.ctx.FileName, sec.StartOffset, sec.EndOffset)),
	}
}

func (v *AstPrinter) VisitCase(c *ast.Case) any {
	return map[string]any{
		"type":   "Case",
		"labels": visitList(c.CaseLabelList, v),
		"body":   visitList(c.StmtSeq, v),
		"range":  formatRange(v.ctx.Source.Span(v.ctx.FileName, c.StartOffset, c.EndOffset)),
	}
}

func (v *AstPrinter) VisitLabelRange(l *ast.LabelRange) any {
	return map[string]any{
		"type":  "LabelRange",
		"low":   l.Low.Accept(v),
		"high":  l.High.Accept(v),
		"range": formatRange(v.ctx.Source.Span(v.ctx.FileName, l.StartOffset, l.EndOffset)),
	}
}

func (v *AstPrinter) VisitGuard(g *ast.Guard) any {
	return map[string]any{
		"type":     "Guard",
		"expr":     g.Expr.Accept(v),
		"typeExpr": g.Type.Accept(v),
		"body":     visitList(g.StmtSeq, v),
		"range":    formatRange(v.ctx.Source.Span(v.ctx.FileName, g.StartOffset, g.EndOffset)),
	}
}

func (v *AstPrinter) VisitProcedureBody(pb *ast.ProcedureBody) any {
	return map[string]any{
		"type":         "ProcedureBody",
		"declarations": visitList(pb.DeclSeq, v),
		"statements":   visitList(pb.StmtSeq, v),
		"range":        formatRange(v.ctx.Source.Span(v.ctx.FileName, pb.StartOffset, pb.EndOffset)),
	}
}
