package pprint

import (
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type AstPrinter struct {
	obx *ast.Oberon
}

func NewAstPrinter(obx *ast.Oberon) *AstPrinter {
	return &AstPrinter{obx}
}

func (v *AstPrinter) Print() any {
	return v.VisitOberon(v.obx)
}

func (v *AstPrinter) VisitOberon(n *ast.Oberon) any {
	var list []ast.CompilationUnit
	for _, unit := range n.Units() {
		list = append(list, unit)
	}

	return map[string]any{
		"type":  "Oberon",
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
		"position":     formatPosition(n.Pos),
		"range":        formatRange(n.Rng),
	}
}

func (v *AstPrinter) VisitMetaSection(ms *ast.MetaSection) any {
	var mode string
	if ms.Mode != token.ILLEGAL {
		mode = ms.Mode.String()
	}

	return map[string]any{
		"type":      "MetaSection",
		"mode":      mode,
		"names":     ms.Ids,
		"typeConst": ms.TyConst.Accept(v),
		"position":  formatPosition(ms.Pos),
		"range":     formatRange(ms.Rng),
	}
}

func (v *AstPrinter) VisitDefinition(def *ast.Definition) any {
	return map[string]any{
		"type":         "Definition",
		"name":         def.Name,
		"imports":      visitList(def.ImportList, v),
		"declarations": visitList(def.DeclSeq, v),
		"position":     formatPosition(def.Pos),
		"range":        formatRange(def.Rng),
	}
}

func (v *AstPrinter) VisitIdentifierDef(id *ast.IdentifierDef) any {
	return map[string]any{
		"type":     "IdentifierDef",
		"name":     id.Name,
		"props":    id.Props.String(),
		"position": formatPosition(id.Pos),
		"range":    formatRange(id.Rng),
	}
}

func (v *AstPrinter) VisitBinaryExpr(n *ast.BinaryExpr) any {
	return map[string]any{
		"type":     "BinaryExpr",
		"op":       n.Op.String(),
		"left":     n.Left.Accept(v),
		"right":    n.Right.Accept(v),
		"position": formatPosition(n.Pos),
		"range":    formatRange(n.Rng),
	}
}

func (v *AstPrinter) VisitBasicLit(n *ast.BasicLit) any {
	return map[string]any{
		"type":     "BasicLit",
		"kind":     n.Kind.String(),
		"value":    n.Val,
		"position": formatPosition(n.Pos),
		"range":    formatRange(n.Rng),
	}
}

func (v *AstPrinter) VisitDesignator(d *ast.Designator) any {
	return map[string]any{
		"type":      "Designator",
		"base":      d.QIdent.Accept(v),
		"selectors": visitList(d.Select, v),
		"position":  formatPosition(d.Pos),
		"range":     formatRange(d.Rng),
	}
}
func (v *AstPrinter) VisitFunctionCall(f *ast.FunctionCall) any {
	return map[string]any{
		"type":      "FunctionCall",
		"callee":    f.Callee.Accept(v),
		"arguments": visitList(f.ActualParams, v),
		"position":  formatPosition(f.Pos),
		"range":     formatRange(f.Rng),
	}
}

func (v *AstPrinter) VisitUnaryExpr(e *ast.UnaryExpr) any {
	return map[string]any{
		"type":     "UnaryExpr",
		"op":       e.Op.String(),
		"operand":  e.Operand.Accept(v),
		"position": formatPosition(e.Pos),
		"range":    formatRange(e.Rng),
	}
}

func (v *AstPrinter) VisitQualifiedIdent(q *ast.QualifiedIdent) any {
	return map[string]any{
		"type":     "QualifiedIdentifier",
		"module":   q.Prefix,
		"name":     q.Name,
		"position": formatPosition(q.Pos),
		"range":    formatRange(q.Rng),
	}
}

func (v *AstPrinter) VisitSet(s *ast.Set) any {
	return map[string]any{
		"type":     "Set",
		"elems":    visitList(s.Elem, v),
		"position": formatPosition(s.Pos),
		"range":    formatRange(s.Rng),
	}
}

func (v *AstPrinter) VisitExprRange(r *ast.ExprRange) any {
	return map[string]any{
		"type":     "ExprRange",
		"low":      r.Low.Accept(v),
		"high":     r.High.Accept(v),
		"position": formatPosition(r.Pos),
		"range":    formatRange(r.Rng),
	}
}
func (v *AstPrinter) VisitNil(n *ast.Nil) any {
	return map[string]any{
		"type":     "Nil",
		"position": formatPosition(n.Pos),
		"range":    formatRange(n.Rng),
	}
}

func (v *AstPrinter) VisitIfStmt(stmt *ast.IfStmt) any {
	return map[string]any{
		"type":           "IfStmt",
		"condition":      stmt.BoolExpr.Accept(v),
		"thenBody":       visitList(stmt.ThenPath, v),
		"elseifBranches": visitList(stmt.ElseIfBranches, v),
		"elseBody":       visitList(stmt.ElsePath, v),
		"position":       formatPosition(stmt.Pos),
		"range":          formatRange(stmt.Rng),
	}
}

func (v *AstPrinter) VisitAssignmentStmt(s *ast.AssignmentStmt) any {
	return map[string]any{
		"type":     "AssignmentStmt",
		"lhs":      s.LValue.Accept(v),
		"rhs":      s.RValue.Accept(v),
		"position": formatPosition(s.Pos),
		"range":    formatRange(s.Rng),
	}
}

func (v *AstPrinter) VisitReturnStmt(s *ast.ReturnStmt) any {
	var value any
	if s.Value != nil {
		value = s.Value.Accept(v)
	}

	return map[string]any{
		"type":     "ReturnStmt",
		"value":    value,
		"position": formatPosition(s.Pos),
		"range":    formatRange(s.Rng),
	}
}

func (v *AstPrinter) VisitProcedureCall(p *ast.ProcedureCall) any {
	return map[string]any{
		"type":     "ProcedureCall",
		"callee":   p.Callee.Accept(v),
		"args":     visitList(p.ActualParams, v),
		"position": formatPosition(p.Pos),
		"range":    formatRange(p.Rng),
	}
}

func (v *AstPrinter) VisitRepeatStmt(s *ast.RepeatStmt) any {
	return map[string]any{
		"type":      "RepeatStmt",
		"body":      visitList(s.StmtSeq, v),
		"condition": s.BoolExpr.Accept(v),
		"position":  formatPosition(s.Pos),
		"range":     formatRange(s.Rng),
	}
}

func (v *AstPrinter) VisitWhileStmt(s *ast.WhileStmt) any {
	return map[string]any{
		"type":      "WhileStmt",
		"condition": s.BoolExpr.Accept(v),
		"body":      visitList(s.StmtSeq, v),
		"elseifs":   visitList(s.ElsIfs, v),
		"position":  formatPosition(s.Position()),
		"range":     formatRange(s.Range()),
	}
}

func (v *AstPrinter) VisitLoopStmt(s *ast.LoopStmt) any {
	return map[string]any{
		"type":     "LoopStmt",
		"body":     visitList(s.StmtSeq, v),
		"position": formatPosition(s.Pos),
		"range":    formatRange(s.Rng),
	}
}

func (v *AstPrinter) VisitCaseStmt(s *ast.CaseStmt) any {
	return map[string]any{
		"type":     "CaseStmt",
		"expr":     s.Expr.Accept(v),
		"cases":    visitList(s.Cases, v),
		"else":     visitList(s.Else, v),
		"position": formatPosition(s.Pos),
		"range":    formatRange(s.Rng),
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
		"position": formatPosition(s.Position()),
		"range":    formatRange(s.Range()),
	}
}

func (v *AstPrinter) VisitExitStmt(s *ast.ExitStmt) any {
	return map[string]any{
		"type":     "ExitStmt",
		"position": formatPosition(s.Pos),
		"range":    formatRange(s.Rng),
	}
}

func (v *AstPrinter) VisitWithStmt(s *ast.WithStmt) any {
	return map[string]any{
		"type":     "WithStmt",
		"guards":   visitList(s.Arms, v),
		"position": formatPosition(s.Pos),
		"range":    formatRange(s.Rng),
	}
}

func (v *AstPrinter) VisitImport(i *ast.Import) any {
	return map[string]any{
		"type":     "Import",
		"alias":    i.Alias,
		"name":     i.Name,
		"path":     i.ImportPath,
		"meta":     visitList(i.Meta, v),
		"position": formatPosition(i.Pos),
		"range":    formatRange(i.Rng),
	}
}

func (v *AstPrinter) VisitProcedureDecl(p *ast.ProcedureDecl) any {
	return map[string]any{
		"type":     "ProcedureDecl",
		"heading":  p.Head.Accept(v),
		"body":     p.Body.Accept(v),
		"position": formatPosition(p.Pos),
		"range":    formatRange(p.Rng),
	}
}

func (v *AstPrinter) VisitVariableDecl(d *ast.VariableDecl) any {
	return map[string]any{
		"type":     "VariableDecl",
		"names":    visitList(d.IdentList, v),
		"typeExpr": d.Type.Accept(v),
		"position": formatPosition(d.Pos),
		"range":    formatRange(d.Rng),
	}
}

func (v *AstPrinter) VisitConstantDecl(d *ast.ConstantDecl) any {
	return map[string]any{
		"type":     "ConstantDecl",
		"name":     d.Name.Accept(v),
		"value":    d.Value.Accept(v),
		"position": formatPosition(d.Pos),
		"range":    formatRange(d.Rng),
	}
}

func (v *AstPrinter) VisitTypeDecl(d *ast.TypeDecl) any {
	return map[string]any{
		"type":     "TypeDecl",
		"name":     d.Name.Accept(v),
		"typeExpr": d.DenotedType.Accept(v),
		"position": formatPosition(d.Pos),
		"range":    formatRange(d.Rng),
	}
}
func (v *AstPrinter) VisitProcedureHeading(h *ast.ProcedureHeading) any {
	var params, rcv any
	if h.FP == nil {
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
		"position": formatPosition(h.Position()),
		"range":    formatRange(h.Range()),
	}
}

func (v *AstPrinter) VisitBasicType(t *ast.BasicType) any {
	return map[string]any{
		"type":     "BasicType",
		"name":     t.Nname,
		"position": formatPosition(t.Pos),
		"range":    formatRange(t.Rng),
	}
}

func (v *AstPrinter) VisitArrayType(t *ast.ArrayType) any {
	var lenList any
	if t.LenList != nil {
		lenList = t.LenList.Accept(v)
	}

	return map[string]any{
		"type":     "ArrayType",
		"length":   lenList,
		"element":  t.ElemType.Accept(v),
		"position": formatPosition(t.Pos),
		"range":    formatRange(t.Rng),
	}
}

func (v *AstPrinter) VisitPointerType(t *ast.PointerType) any {
	return map[string]any{
		"type":     "PointerType",
		"base":     t.Base.Accept(v),
		"position": formatPosition(t.Pos),
		"range":    formatRange(t.Rng),
	}
}

func (v *AstPrinter) VisitProcedureType(t *ast.ProcedureType) any {
	var params any
	if t.FP != nil {
		params = t.FP.Accept(v)
	}

	return map[string]any{
		"type":     "ProcedureType",
		"params":   params,
		"position": formatPosition(t.Pos),
		"range":    formatRange(t.Rng),
	}
}

func (v *AstPrinter) VisitRecordType(t *ast.RecordType) any {
	var super any
	if t.Base != nil {
		super = t.Base.Accept(v)
	}

	return map[string]any{
		"type":     "RecordType",
		"super":    super,
		"fields":   visitList(t.Fields, v),
		"position": formatPosition(t.Pos),
		"range":    formatRange(t.Rng),
	}
}

func (v *AstPrinter) VisitEnumType(t *ast.EnumType) any {
	return map[string]any{
		"type":     "EnumType",
		"options":  t.Variants,
		"position": formatPosition(t.Pos),
		"range":    formatRange(t.Rng),
	}
}

func (v *AstPrinter) VisitNamedType(t *ast.NamedType) any {
	return map[string]any{
		"type":     "NamedType",
		"name":     t.Name.Accept(v),
		"position": formatPosition(t.Pos),
		"range":    formatRange(t.Rng),
	}
}

func (v *AstPrinter) VisitIndexOp(op *ast.IndexOp) any {
	return map[string]any{
		"type":     "IndexOp",
		"list":     visitList(op.List, v),
		"position": formatPosition(op.Pos),
		"range":    formatRange(op.Rng),
	}
}

func (v *AstPrinter) VisitPtrDeref(deref *ast.PtrDeref) any {
	return map[string]any{
		"type":     "PtrDeref",
		"position": formatPosition(deref.Pos),
		"range":    formatRange(deref.Rng),
	}
}

func (v *AstPrinter) VisitDotOp(op *ast.DotOp) any {
	return map[string]any{
		"type":     "DotOp",
		"field":    op.Field,
		"position": formatPosition(op.Pos),
		"range":    formatRange(op.Rng),
	}
}

func (v *AstPrinter) VisitTypeGuard(guard *ast.TypeGuard) any {
	return map[string]any{
		"type":     "TypeGuard",
		"expr":     guard.Ty.Accept(v),
		"position": formatPosition(guard.Pos),
		"range":    formatRange(guard.Rng),
	}
}

func (v *AstPrinter) VisitBadExpr(expr *ast.BadExpr) any {
	return map[string]any{
		"type":     "BadExpr",
		"position": formatPosition(expr.Pos),
		"range":    formatRange(expr.Rng),
	}
}

func (v *AstPrinter) VisitBadDecl(decl *ast.BadDecl) any {
	return map[string]any{
		"type":     "BadDecl",
		"position": formatPosition(decl.Pos),
		"range":    formatRange(decl.Rng),
	}
}

func (v *AstPrinter) VisitBadStmt(stmt *ast.BadStmt) any {
	return map[string]any{
		"type":     "BadStmt",
		"position": formatPosition(stmt.Pos),
		"range":    formatRange(stmt.Rng),
	}
}

func (v *AstPrinter) VisitBadType(ty *ast.BadType) any {
	return map[string]any{
		"type":     "BadType",
		"position": formatPosition(ty.Pos),
		"range":    formatRange(ty.Rng),
	}
}

func (v *AstPrinter) VisitFieldList(fl *ast.FieldList) any {
	return map[string]any{
		"type":     "FieldList",
		"fields":   visitList(fl.List, v),
		"typeExpr": fl.Type.Accept(v),
		"position": formatPosition(fl.Pos),
		"range":    formatRange(fl.Rng),
	}
}

func (v *AstPrinter) VisitLenList(ll *ast.LenList) any {
	return map[string]any{
		"type":     "LenList",
		"modifier": ll.Modifier.String(),
		"elements": visitList(ll.List, v),
		"position": formatPosition(ll.Pos),
		"range":    formatRange(ll.Rng),
	}
}

func (v *AstPrinter) VisitElseIfBranch(br *ast.ElseIfBranch) any {
	return map[string]any{
		"type":      "ElseIfBranch",
		"condition": br.BoolExpr.Accept(v),
		"body":      visitList(br.ThenPath, v),
		"position":  formatPosition(br.Pos),
		"range":     formatRange(br.Rng),
	}
}

func (v *AstPrinter) VisitFormalParams(fp *ast.FormalParams) any {
	return map[string]any{
		"type":     "FormalParams",
		"params":   visitList(fp.Params, v),
		"retType":  fp.RetType.Accept(v),
		"position": formatPosition(fp.Pos),
		"range":    formatRange(fp.Rng),
	}
}

func (v *AstPrinter) VisitReceiver(r *ast.Receiver) any {
	var mode string
	if r.Mod != token.ILLEGAL {
		mode = r.Mod.String()
	}

	return map[string]any{
		"type":     "Receiver",
		"modifier": mode,
		"name":     r.Var,
		"typeExpr": r.Type.Accept(v),
		"position": formatPosition(r.Pos),
		"range":    formatRange(r.Rng),
	}
}

func (v *AstPrinter) VisitFPSection(sec *ast.FPSection) any {
	var mode string
	if sec.Mod != token.ILLEGAL {
		mode = sec.Mod.String()
	}

	return map[string]any{
		"type":     "FPSection",
		"modifier": mode,
		"names":    sec.Names,
		"typeExpr": sec.Type.Accept(v),
		"position": formatPosition(sec.Pos),
		"range":    formatRange(sec.Rng),
	}
}

func (v *AstPrinter) VisitCase(c *ast.Case) any {
	return map[string]any{
		"type":     "Case",
		"labels":   visitList(c.CaseLabelList, v),
		"body":     visitList(c.StmtSeq, v),
		"position": formatPosition(c.Pos),
		"range":    formatRange(c.Rng),
	}
}

func (v *AstPrinter) VisitLabelRange(l *ast.LabelRange) any {
	return map[string]any{
		"type":     "LabelRange",
		"low":      l.Low.Accept(v),
		"high":     l.High.Accept(v),
		"position": formatPosition(l.Pos),
		"range":    formatRange(l.Rng),
	}
}

func (v *AstPrinter) VisitGuard(g *ast.Guard) any {
	return map[string]any{
		"type":     "Guard",
		"expr":     g.Expr.Accept(v),
		"typeExpr": g.Type.Accept(v),
		"position": formatPosition(g.Pos),
		"body":     visitList(g.StmtSeq, v),
		"range":    formatRange(g.Rng),
	}
}

func (v *AstPrinter) VisitProcedureBody(pb *ast.ProcedureBody) any {
	return map[string]any{
		"type":         "ProcedureBody",
		"declarations": visitList(pb.DeclSeq, v),
		"statements":   visitList(pb.StmtSeq, v),
		"position":     formatPosition(pb.Pos),
		"range":        formatRange(pb.Rng),
	}
}
