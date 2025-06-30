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
	if def.Name == "_" {
		return def
	}

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
	if dsg.QIdent.Accept(n) != nil {
		dsg.Symbol = dsg.QIdent.Symbol
	}

	if dsg.QIdent.Symbol == nil {
		return dsg
	}

	symbol := dsg.QIdent.Symbol
	astType := symbol.AstType()

	for _, selector := range dsg.Select {
		if astType == nil {
			n.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("'%s' must have a denoted type", dsg.QIdent),
				Range:    n.ctx.Source.Span(n.ctx.FileName, dsg.QIdent.StartOffset, dsg.QIdent.EndOffset),
			})

			continue
		}

		astType = n.underlying(astType)

		switch s := selector.(type) {
		case *ast.DotOp:
			var rec *ast.RecordType

			switch tn := astType.(type) {
			case *ast.RecordType:
				rec = tn
			case *ast.PointerType:
				ptr, ok := n.underlying(tn.Base).(*ast.RecordType)
				if !ok {
					continue
				}

				rec = ptr
			default:
				continue
			}

			sym := rec.Env.Lookup(s.Field)
			if sym == nil {
				n.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("record '%s' has no field named '%s'", dsg.QIdent, s.Field),
					Range:    n.ctx.Source.Span(n.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
				})

				continue
			}

			sym.SetMangledName(ast.Mangle(sym))
			s.Symbol = sym
			dsg.Symbol = sym

			symbol = sym
			astType = sym.AstType()
		case *ast.IndexOp:
			var arr *ast.ArrayType

			switch tn := astType.(type) {
			case *ast.ArrayType:
				arr = tn
			case *ast.PointerType:
				ptr, ok := tn.Base.(*ast.ArrayType)
				if !ok {
					n.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("cannot index non-array type '%s'", dsg.QIdent),
						Range:    n.ctx.Source.Span(n.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
					})

					continue
				}
				arr = ptr
			default:
				n.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("cannot index non-array type '%s'", dsg.QIdent),
					Range:    n.ctx.Source.Span(n.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
				})
			}

			for _, expr := range s.List {
				expr.Accept(n)
			}

			astType = arr.ElemType
		case *ast.PtrDeref:
			switch p := astType.(type) {
			case *ast.PointerType:
				astType = p.Base
			case *ast.ProcedureType:
				astType = symbol.AstType()
			default:
				n.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("dereference/super operator not applicable to '%s'", dsg.QIdent),
					Range:    n.ctx.Source.Span(n.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
				})

				continue
			}
		case *ast.TypeGuard:
			s.Ty.Accept(n)
			ty, ok := s.Ty.(*ast.QualifiedIdent)
			if !ok || ty.Symbol == nil || ty.Symbol.Kind() != ast.TypeSymbolKind {
				n.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("type-guard expression must be a (qualified) identifier: '%v'", dsg.QIdent),
					Range:    n.ctx.Source.Span(n.ctx.FileName, s.Ty.Pos(), s.Ty.End()),
				})

				return nil
			}

			symbol = ty.Symbol
			astType = ty.Symbol.AstType()
		}
	}

	return dsg
}

func (n *NamesResolver) VisitFunctionCall(call *ast.FunctionCall) any {
	call.Callee.Accept(n)

	if call.Callee.Symbol == nil {
		n.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("cannot resolve name '%v'", call.Callee),
			Range:    n.ctx.Source.Span(n.ctx.FileName, call.Callee.StartOffset, call.Callee.EndOffset),
		})
	}

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
	if sym, ok := n.ctx.SymbolOverrides[ident.Name]; ok {
		ident.Symbol = sym
		return ident
	}

	if ident.Prefix == "" {
		sym := n.ctx.Env.Lookup(ident.Name)
		if sym == nil {
			n.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("undeclared identifier: '%s'", ident.Name),
				Range:    n.ctx.Source.Span(n.ctx.FileName, ident.StartOffset, ident.EndOffset),
			})

			return nil
		}

		ident.Symbol = sym
		return ident
	}

	sym := n.ctx.Env.Lookup(ident.Prefix)
	if sym == nil || sym.Kind() != ast.ModuleSymbolKind {
		n.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("'%s' is not defined as a module", ident.Prefix),
			Range:    n.ctx.Source.Span(n.ctx.FileName, ident.StartOffset, ident.EndOffset),
		})

		return ident
	}

	sym = sym.(*ast.ModuleSymbol).Env.Lookup(ident.Name)
	if sym == nil || sym.Props() != ast.Exported {
		n.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("identifier '%s' not found or is not exported", ident.Name),
			Range:    n.ctx.Source.Span(n.ctx.FileName, ident.StartOffset, ident.EndOffset),
		})

		return ident
	}

	sym.SetMangledName(ast.Mangle(sym))
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

func (n *NamesResolver) VisitNil(kneel *ast.Nil) any { return kneel }

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

	dsg, isDsg := stmt.Expr.(*ast.Designator)
	if !isDsg {
		n.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("'%s' is not a valid CASE selector type", stmt.Expr),
			Range:    n.ctx.Source.Span(n.ctx.FileName, stmt.Expr.Pos(), stmt.Expr.End()),
		})

		return stmt
	}

	caseExprName := dsg.QIdent.Name
	caseExprType := n.underlying(dsg.Symbol.AstType())
	for _, c := range stmt.Cases {
		for _, labelRange := range c.CaseLabelList {
			labelRange.Accept(n)

			switch caseExprType.(type) {
			case *ast.RecordType, *ast.PointerType:
				if _, ok := labelRange.Low.(*ast.Nil); ok {
					continue
				}

				label, ok := labelRange.Low.(*ast.Designator)
				if !ok {
					n.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("'%s' is not a valid CASE label", labelRange.Low),
						Range:    n.ctx.Source.Span(n.ctx.FileName, stmt.Expr.Pos(), stmt.Expr.End()),
					})

					return stmt
				}

				switch n.underlying(label.Symbol.AstType()).(type) {
				case *ast.RecordType, *ast.PointerType:
					n.ctx.SymbolOverrides[caseExprName] = ast.NewVariableSymbol(caseExprName, label.Symbol.Props(), label.Symbol.AstType())
				default:
					continue
				}
			default:
				continue
			}
		}

		for _, statement := range c.StmtSeq {
			statement.Accept(n)
		}

		delete(n.ctx.SymbolOverrides, caseExprName)
	}

	for _, statement := range stmt.Else {
		statement.Accept(n)
	}

	return stmt
}

func (n *NamesResolver) VisitCase(c *ast.Case) any { return c }

func (n *NamesResolver) VisitLabelRange(rng *ast.LabelRange) any {
	rng.Low.Accept(n)
	if rng.High != nil {
		rng.High.Accept(n)
	}

	return rng
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

func (n *NamesResolver) VisitGuard(guard *ast.Guard) any {
	guard.Expr.Accept(n)
	guard.Type.Accept(n)

	if !IsValidGuardExpr(guard.Expr) {

	}

	expr := guard.Expr.(*ast.Designator)
	ty := guard.Type.(*ast.Designator)

	n.ctx.SymbolOverrides[guard.Expr.String()] = ast.NewVariableSymbol(
		expr.Symbol.Name(), expr.Symbol.Props(), ty.Symbol.AstType())

	for _, stmt := range guard.StmtSeq {
		stmt.Accept(n)
	}

	delete(n.ctx.SymbolOverrides, guard.Expr.String())

	return guard
}

func (n *NamesResolver) VisitImport(i *ast.Import) any {
	name := i.Alias
	if name == "" {
		name = i.Name
	}

	sym := n.ctx.Env.Lookup(name)
	if sym == nil || sym.Kind() != ast.ModuleSymbolKind {
		n.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("'%s' is not a known module", name),
			Range:    n.ctx.Source.Span(n.ctx.FileName, i.StartOffset, i.EndOffset),
		})

		return i
	}

	sym.(*ast.ModuleSymbol).Env = n.ctx.Envs[name]

	return i
}

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

func (n *NamesResolver) VisitProcedureHeading(heading *ast.ProcedureHeading) any {
	var rcvName string
	if heading.Rcv != nil {
		heading.Rcv.Accept(n)
		rcvName = heading.Rcv.Type.String() + "."
	}

	//heading.Name.Accept(n)
	sym := n.ctx.Env.Lookup(rcvName + heading.Name.Name)
	if sym == nil {
		n.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("identifier '%s' not found", heading.Name.Name),
			Range:    n.ctx.Source.Span(n.ctx.FileName, heading.Name.StartOffset, heading.Name.EndOffset),
		})

		return nil
	}

	sym.SetMangledName(ast.Mangle(sym))
	heading.Name.Symbol = sym

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

func (n *NamesResolver) VisitVariableDecl(decl *ast.VariableDecl) any {
	for _, id := range decl.IdentList {
		def := id.Accept(n)
		if def == nil {
			n.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("'%s' is not a known variable", id.Name),
				Range:    n.ctx.Source.Span(n.ctx.FileName, id.StartOffset, id.EndOffset),
			})
		}
	}

	decl.Type.Accept(n)

	return decl
}

func (n *NamesResolver) VisitReceiver(rcv *ast.Receiver) any {
	rcv.Type.Accept(n)
	rcv.Name.Accept(n)

	rcv.Name.Symbol.SetMangledName(ast.Mangle(rcv.Name.Symbol))

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

	if params.RetType != nil {
		params.RetType.Accept(n)
	}

	return params
}

func (n *NamesResolver) VisitConstantDecl(decl *ast.ConstantDecl) any {
	if def := decl.Name.Accept(n); def == nil {
		n.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("undeclared constant '%s'", decl.Name.Name),
			Range:    n.ctx.Source.Span(n.ctx.FileName, decl.Name.StartOffset, decl.Name.EndOffset),
		})
	}

	decl.Value.Accept(n)

	return decl
}

func (n *NamesResolver) VisitTypeDecl(decl *ast.TypeDecl) any {
	decl.Name.Accept(n)
	decl.DenotedType.Accept(n)
	return decl
}

func (n *NamesResolver) VisitBasicType(ty *ast.BasicType) any { return ty }

func (n *NamesResolver) VisitArrayType(ty *ast.ArrayType) any {
	if ty.LenList != nil {
		ty.LenList.Accept(n)
	}

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
		field.Env = ty.Env
		field.Accept(n)
	}

	return ty
}

func (n *NamesResolver) VisitEnumType(ty *ast.EnumType) any {
	for _, variant := range ty.Variants {
		sym := n.ctx.Env.Lookup(variant.Name)
		if sym == nil {
			n.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("undeclared enum variant '%s'", variant.Name),
				Range:    n.ctx.Source.Span(n.ctx.FileName, variant.StartOffset, variant.EndOffset),
			})

			continue
		}

		variant.Symbol = sym
	}

	return ty
}

func (n *NamesResolver) VisitNamedType(ty *ast.NamedType) any {
	ty.Name.Accept(n)
	ty.Symbol = ty.Name.Symbol
	return ty
}

func (n *NamesResolver) VisitFieldList(list *ast.FieldList) any {
	for _, def := range list.List {
		sym := list.Env.Lookup(def.Name)
		if sym == nil {
			n.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("field '%s' not found in record", def.Name),
				Range:    n.ctx.Source.Span(n.ctx.FileName, def.StartOffset, def.EndOffset),
			})

			continue
		}

		sym.SetMangledName(ast.Mangle(sym))
		def.Symbol = sym
	}

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

func (n *NamesResolver) VisitBadExpr(expr *ast.BadExpr) any { return expr }

func (n *NamesResolver) VisitBadDecl(decl *ast.BadDecl) any { return decl }

func (n *NamesResolver) VisitBadStmt(stmt *ast.BadStmt) any { return stmt }

func (n *NamesResolver) VisitBadType(ty *ast.BadType) any { return ty }
