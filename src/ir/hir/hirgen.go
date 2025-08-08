package hir

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
)

type Generator struct {
	ctx *report.Context
	obx *ast.OberonX
}

func NewGenerator(ctx *report.Context, obx *ast.OberonX) *Generator {
	return &Generator{ctx: ctx, obx: obx}
}

func (g Generator) Generate() *Program {
	program := &Program{}

	for _, unit := range g.obx.Units {
		mod := unit.Accept(g).(*Module)
		program.Modules = append(program.Modules, mod)
	}

	return program
}

func (g Generator) VisitModule(module *ast.Module) any {
	name := module.BName
	decls := g.visitDeclSeq(module.DeclSeq)
	stmts := g.visitStmtSeq(module.StmtSeq)
	// TODO metaparams

	return &Module{
		Name:    name,
		IsEntry: module.IsEntry,
		Decls:   decls,
		Init: &Function{
			Name: fmt.Sprintf("__init_%s", name),
			Body: stmts,
		},
	}
}

func (g Generator) VisitMetaSection(section *ast.MetaSection) any { return section }

func (g Generator) VisitDefinition(definition *ast.Definition) any {
	name := definition.BName
	decls := g.visitDeclSeq(definition.DeclSeq)

	return &Module{
		Name:  name,
		Decls: decls,
	}
}

func (g Generator) VisitIdentifierDef(def *ast.IdentifierDef) any { return def }

func (g Generator) VisitBinaryExpr(expr *ast.BinaryExpr) any {
	return &BinaryExpr{
		Left:     expr.Left.Accept(g).(Expr),
		Right:    expr.Right.Accept(g).(Expr),
		Op:       expr.Op,
		SemaType: expr.SemaType,
	}
}

func (g Generator) VisitDesignator(dsg *ast.Designator) any {
	Dsg := dsg.QIdent.Accept(g).(Expr)

	for _, selector := range dsg.Select {
		switch sel := selector.(type) {
		case *ast.DotOp:
			Dsg = &FieldAccess{
				Record:   Dsg,
				Field:    sel.Field,
				SemaType: sel.Symbol.Type(),
			}
		case *ast.IndexOp:
			Dsg = &IndexExpr{
				Array:    Dsg,
				Index:    g.visitExprList(sel.List),
				SemaType: Dsg.Type(),
			}
		case *ast.PtrDeref:
			Dsg = &DerefExpr{
				Pointer:  Dsg,
				SemaType: Dsg.Type(),
			}
		case *ast.TypeGuard:
			// TODO implement this
		}
	}

	return Dsg
}

func (g Generator) VisitFunctionCall(call *ast.FunctionCall) any {
	callee := call.Callee.Accept(g).(*FunctionRef)

	return &FuncCall{
		Func:    callee,
		Args:    g.visitExprList(call.ActualParams),
		RetType: call.SemaType,
	}
}

func (g Generator) VisitUnaryExpr(expr *ast.UnaryExpr) any {
	return &UnaryExpr{
		Op:       expr.Op,
		Operand:  expr.Operand.Accept(g).(Expr),
		SemaType: expr.SemaType,
	}
}

func (g Generator) VisitQualifiedIdent(ident *ast.QualifiedIdent) any {
	switch ident.Symbol.Kind() {
	case ast.VariableSymbolKind:
		return &VariableRef{
			Name:       ident.Name,
			Mangled:    ident.Symbol.MangledName(),
			SemaType:   ident.SemaType,
			Module:     ident.Prefix,
			IsExported: ident.Symbol.Props() == ast.Exported || ident.Symbol.Props() == ast.ExportedReadOnly,
			IsReadOnly: ident.Symbol.Props() == ast.ReadOnly,
			Offset:     ident.Symbol.Offset(),
			Size:       ident.SemaType.Width(),
		}
	case ast.ConstantSymbolKind:
		sym := ident.Symbol.(*ast.ConstantSymbol)
		return &ConstantRef{
			Name:       ident.Name,
			Value:      sym.Value.Accept(g).(Expr),
			Mangled:    sym.MangledName(),
			SemaType:   ident.SemaType,
			Module:     ident.Prefix,
			IsExported: sym.Props() == ast.Exported || ident.Symbol.Props() == ast.ExportedReadOnly,
			IsReadOnly: sym.Props() == ast.ReadOnly,
			Offset:     sym.Offset(),
			Size:       ident.SemaType.Width(),
		}
	case ast.ProcedureSymbolKind:
		sym := ident.Symbol.(*ast.ProcedureSymbol)

		return &FunctionRef{
			Name:       ident.Name,
			Mangled:    sym.MangledName(),
			Kind:       sym.ProcKind,
			Module:     ident.Prefix,
			SemaType:   ident.SemaType,
			IsExported: sym.Props() == ast.Exported || ident.Symbol.Props() == ast.ExportedReadOnly,
			IsReadOnly: sym.Props() == ast.ReadOnly,
			Offset:     sym.Offset(),
			//Size:       ident.SemaType.Width(),
		}
	//case ast.TypeSymbolKind:
	//case ast.ModuleSymbolKind:
	//case ast.FieldSymbolKind:
	//case ast.DefinitionSymbolKind:
	case ast.ParamSymbolKind:
		sym := ident.Symbol.(*ast.ParamSymbol)
		var mod ParamKind
		switch sym.Mod {
		case token.IN:
			mod = InParam
		case token.VAR:
			mod = VarParam
		default:
			mod = ValueParam
		}

		return &Param{
			Name: ident.Name,
			Kind: mod,
			Typ:  ident.SemaType,
		}
	default:
		panic("unhandled symbol kind")
	}
}

func (g Generator) VisitSet(set *ast.Set) any {
	var elems []Expr
	for _, expression := range set.Elem {
		elems = append(elems, expression.Accept(g).(Expr))
	}

	return &SetExpr{Elems: elems, SemaType: set.SemaType}
}

func (g Generator) VisitBasicLit(lit *ast.BasicLit) any {
	return &Literal{
		Kind:     lit.Kind,
		Value:    lit.Val,
		SemaType: lit.SemaType,
	}
}

func (g Generator) VisitExprRange(r *ast.ExprRange) any {
	low := r.Low.Accept(g).(Expr)
	high := r.High.Accept(g).(Expr)
	return &RangeExpr{Low: low, High: high}
}

func (g Generator) VisitNil(n *ast.Nil) any {
	return &Literal{
		Kind:     token.NIL,
		Value:    "nil",
		SemaType: n.SemaType,
	}
}

func (g Generator) VisitIfStmt(stmt *ast.IfStmt) any {
	// Convert the main IF condition and THEN block
	cond := stmt.BoolExpr.Accept(g).(Expr)
	then := g.visitStmtSeq(stmt.ThenPath)

	// Convert ELSE block
	var elseBody *CompoundStmt
	if stmt.ElsePath != nil {
		elseBody = g.visitStmtSeq(stmt.ElsePath)
	}

	var elifs []*ElseIfBranch
	for _, branch := range stmt.ElseIfBranches {
		brCond := branch.BoolExpr.Accept(g).(Expr)
		brThen := g.visitStmtSeq(stmt.ThenPath)

		elifs = append(elifs, &ElseIfBranch{Cond: brCond, Body: brThen})
	}

	return &IfStmt{
		Cond:    cond,
		Then:    then,
		Else:    elseBody,
		ElseIfs: elifs,
	}
}

func (g Generator) VisitElseIfBranch(branch *ast.ElseIfBranch) any { return branch }

func (g Generator) VisitAssignmentStmt(stmt *ast.AssignmentStmt) any {
	return &AssignStmt{
		Left:  stmt.LValue.Accept(g).(Expr),
		Right: stmt.RValue.Accept(g).(Expr),
	}
}

func (g Generator) VisitReturnStmt(stmt *ast.ReturnStmt) any {
	var result Expr
	if stmt.Value != nil {
		result = stmt.Value.Accept(g).(Expr)
	}
	return &ReturnStmt{Result: result}
}

func (g Generator) VisitProcedureCall(call *ast.ProcedureCall) any {
	callee := call.Callee.Accept(g).(*FunctionRef)

	return &FuncCall{
		Func: callee,
		Args: g.visitExprList(call.ActualParams),
	}
}

func (g Generator) VisitRepeatStmt(stmt *ast.RepeatStmt) any {
	// Process REPEAT body
	body := g.visitStmtSeq(stmt.StmtSeq)

	// Generate condition expression
	cond := stmt.BoolExpr.Accept(g).(Expr)

	// Emit exit condition
	// exit if cond
	exit := &IfStmt{
		Cond: cond,
		Then: &CompoundStmt{[]Stmt{&ExitStmt{LoopLabel: stmt.Label}}},
	}

	body.Stmts = append(body.Stmts, exit)

	return &LoopStmt{Body: body, Label: stmt.Label}
}

func (g Generator) VisitWhileStmt(stmt *ast.WhileStmt) any {
	body := &CompoundStmt{}

	// WHILE <cond>
	cond := stmt.BoolExpr.Accept(g).(Expr)
	negated := &UnaryExpr{Op: token.NOT, Operand: cond}
	body.Stmts = append(body.Stmts, &IfStmt{
		Cond: negated,
		Then: &CompoundStmt{[]Stmt{&ExitStmt{LoopLabel: stmt.Label}}},
	})

	// Body
	for _, s := range stmt.StmtSeq {
		body.Stmts = append(body.Stmts, s.Accept(g).(Stmt))
	}

	// ELSIF branches
	for _, branch := range stmt.ElsIfs {
		bCond := branch.BoolExpr.Accept(g).(Expr)
		negB := &UnaryExpr{Op: token.NOT, Operand: bCond}

		body.Stmts = append(body.Stmts, &IfStmt{
			Cond: negB,
			Then: &CompoundStmt{
				[]Stmt{
					&ExitStmt{LoopLabel: stmt.Label},
				},
			},
		})

		for _, s := range branch.ThenPath {
			body.Stmts = append(body.Stmts, s.Accept(g).(Stmt))
		}
	}

	return &LoopStmt{Label: stmt.Label, Body: body}
}

func (g Generator) VisitLoopStmt(stmt *ast.LoopStmt) any {
	body := g.visitStmtSeq(stmt.StmtSeq)
	return &LoopStmt{Body: body}
}

func (g Generator) VisitCaseStmt(stmt *ast.CaseStmt) any {
	expr := stmt.Expr.Accept(g).(Expr)

	var cases []*Case
	for _, c := range stmt.Cases {
		cases = append(cases, c.Accept(g).(*Case))
	}

	var elseBody []Stmt
	for _, s := range stmt.Else {
		elseBody = append(elseBody, s.Accept(g).(Stmt))
	}

	return &CaseStmt{
		Expr:  expr,
		Cases: cases,
		Else:  &CompoundStmt{elseBody},
	}
}

func (g Generator) VisitCase(c *ast.Case) any {
	var labels []*LabelRange

	for _, r := range c.CaseLabelList {
		rng := r.Accept(g).(*LabelRange)
		labels = append(labels, rng)
	}

	var stmts []Stmt
	for _, stmt := range c.StmtSeq {
		stmts = append(stmts, stmt.Accept(g).(Stmt))
	}

	return &Case{
		Labels: labels,
		Body:   &CompoundStmt{stmts},
	}
}

func (g Generator) VisitLabelRange(r *ast.LabelRange) any {
	low := r.Low.Accept(g).(Expr)
	high := low
	if r.High != nil {
		high = r.High.Accept(g).(Expr)
	}

	return &LabelRange{Low: low, High: high}
}

func (g Generator) VisitForStmt(stmt *ast.ForStmt) any {
	cvar := &VariableRef{
		Name:       stmt.CtlVar.Symbol.Name(),
		Mangled:    stmt.CtlVar.Symbol.MangledName(),
		SemaType:   stmt.CtlVar.SemaType,
		IsExported: stmt.CtlVar.Props == ast.Exported || stmt.CtlVar.Props == ast.ExportedReadOnly,
		IsReadOnly: stmt.CtlVar.Props == ast.ReadOnly,
	}
	init := stmt.InitVal.Accept(g).(Expr)
	final := stmt.FinalVal.Accept(g).(Expr)

	// Default step is 1
	var step Expr = &Literal{Value: "1", SemaType: stmt.CtlVar.SemaType}
	if stmt.By != nil {
		step = stmt.By.Accept(g).(Expr)
	}

	// Initialize control variable: cvar = init
	initAssign := &AssignStmt{
		Left:  cvar,
		Right: init,
	}

	// Condition: cvar > final => break
	breakIf := &IfStmt{
		Cond: &BinaryExpr{
			Op:    token.GREAT,
			Left:  cvar,
			Right: final,
		},
		Then: &CompoundStmt{[]Stmt{&ExitStmt{LoopLabel: stmt.Label}}},
	}

	// Generate loop body
	var bodyStmts []Stmt
	for _, s := range stmt.StmtSeq {
		bodyStmts = append(bodyStmts, s.Accept(g).([]Stmt)...)
	}

	// Increment: cvar = cvar + step
	incr := &AssignStmt{
		Left: cvar,
		Right: &BinaryExpr{
			Op:    token.PLUS,
			Left:  cvar,
			Right: step,
		},
	}

	// Full loop body: condition, body, increment
	body := append([]Stmt{breakIf}, append(bodyStmts, incr)...)

	return &CompoundStmt{[]Stmt{
		initAssign,
		&LoopStmt{Body: &CompoundStmt{body}},
	}}
}

func (g Generator) VisitExitStmt(stmt *ast.ExitStmt) any {
	return &ExitStmt{LoopLabel: stmt.Label}
}

func (g Generator) VisitWithStmt(stmt *ast.WithStmt) any {
	var guards []*WithGuard

	for _, arm := range stmt.Arms {
		guard := arm.Accept(g).(*WithGuard)
		guards = append(guards, guard)
	}

	return &WithStmt{
		Guards: guards,
		Else:   g.visitStmtSeq(stmt.Else),
	}

}

func (g Generator) VisitGuard(guard *ast.Guard) any {
	guardExpr := guard.Expr.Accept(g).(Expr)
	guardType := guard.Type.Accept(g).(Expr)

	return &WithGuard{
		Expr: guardExpr,
		Type: guardType,
		Body: g.visitStmtSeq(guard.StmtSeq),
	}
}

func (g Generator) VisitImport(i *ast.Import) any { return i }

func (g Generator) VisitProcedureDecl(decl *ast.ProcedureDecl) any {
	name := decl.Head.Name.Name

	// === Parameters ===
	var (
		params     []*Param
		resultType types.Type
		locals     []Decl
		body       *CompoundStmt
	)

	// Handle receiver (for type-bound procedures)
	if decl.Head.Rcv != nil {
		params = append(params, decl.Head.Rcv.Accept(g).(*Param))
	}

	// Handle formal parameters and Return type
	if decl.Head.FP != nil {
		params = decl.Head.FP.Accept(g).([]*Param)
		if decl.Head.FP.RetType != nil {
			ty := decl.Head.Name.SemaType.(*types.ProcedureType)
			resultType = ty.Result
		}
	}

	// === Body ===
	if decl.Body != nil {
		locals = g.visitDeclSeq(decl.Body.DeclSeq)
		body = g.visitStmtSeq(decl.Body.StmtSeq)
	}

	var decls []Decl
	decls = append(decls, &Function{
		Name:       name,
		Mangled:    decl.Head.Name.Symbol.MangledName(),
		Params:     params,
		Result:     resultType,
		Locals:     locals,
		Body:       body,
		IsExport:   decl.Head.Name.Props == ast.Exported || decl.Head.Name.Props == ast.ExportedReadOnly,
		IsReadOnly: decl.Head.Name.Props == ast.ReadOnly,
	})

	return decls
}

func (g Generator) VisitProcedureHeading(head *ast.ProcedureHeading) any { return head }

func (g Generator) VisitProcedureBody(body *ast.ProcedureBody) any { return body }

func (g Generator) VisitReceiver(recv *ast.Receiver) any {
	return &Param{
		Name: recv.Name.Name,
		Typ:  recv.Type.Type(),
		Kind: g.emitParamKind(recv.Kind),
	}
}

func (g Generator) VisitFormalParams(params *ast.FormalParams) any {
	var formalParams []*Param

	for _, sec := range params.Params {
		sections := sec.Accept(g).([]*Param)
		formalParams = append(formalParams, sections...)
	}

	return formalParams
}

func (g Generator) VisitFPSection(sec *ast.FPSection) any {
	var params []*Param

	kind := g.emitParamKind(sec.Kind)

	for _, id := range sec.Names {
		params = append(params, &Param{
			Name: id.Name,
			Typ:  id.SemaType,
			Kind: kind, // "VAR", "IN", etc
		})
	}

	return params
}

func (g Generator) VisitVariableDecl(decl *ast.VariableDecl) any {
	var decls []Decl

	for _, id := range decl.IdentList {
		decls = append(decls, &Variable{
			Name:       id.Name,
			Mangled:    id.Symbol.MangledName(),
			Type:       id.SemaType,
			Size:       id.SemaType.Width(),
			Offset:     id.Symbol.Offset(),
			IsExport:   id.Props == ast.Exported,
			IsReadOnly: id.Props == ast.ReadOnly,
		})
	}

	return decls
}

func (g Generator) VisitConstantDecl(decl *ast.ConstantDecl) any {
	var decls []Decl

	decls = append(decls, &Constant{
		Name:       decl.Name.Name,
		Mangled:    decl.Name.Symbol.MangledName(),
		Type:       decl.Name.SemaType,
		Value:      decl.Value.Accept(g).(Expr),
		Size:       decl.Name.SemaType.Width(),
		Offset:     decl.Name.Symbol.Offset(),
		IsExport:   decl.Name.Props == ast.Exported || decl.Name.Props == ast.ExportedReadOnly,
		IsReadOnly: decl.Name.Props == ast.ReadOnly,
	})

	return decls
}

func (g Generator) VisitTypeDecl(decl *ast.TypeDecl) any {
	var decls []Decl

	decls = append(decls, &Type{
		Name:     decl.Name.Name,
		Mangled:  decl.Name.Symbol.MangledName(),
		Type:     decl.Name.SemaType,
		IsExport: decl.Name.Props == ast.Exported || decl.Name.Props == ast.ExportedReadOnly,
	})

	return decls
}

func (g Generator) VisitBasicType(ty *ast.BasicType) any { return ty }

func (g Generator) VisitArrayType(ty *ast.ArrayType) any { return ty }

func (g Generator) VisitLenList(list *ast.LenList) any { return list }

func (g Generator) VisitPointerType(ty *ast.PointerType) any { return ty }

func (g Generator) VisitProcedureType(ty *ast.ProcedureType) any { return ty }

func (g Generator) VisitRecordType(ty *ast.RecordType) any { return ty }

func (g Generator) VisitFieldList(list *ast.FieldList) any { return list }

func (g Generator) VisitEnumType(ty *ast.EnumType) any { return ty }

func (g Generator) VisitNamedType(ty *ast.NamedType) any { return ty }

func (g Generator) VisitBadExpr(expr *ast.BadExpr) any { return expr }

func (g Generator) VisitBadDecl(decl *ast.BadDecl) any { return decl }

func (g Generator) VisitBadStmt(stmt *ast.BadStmt) any { return stmt }

func (g Generator) VisitBadType(ty *ast.BadType) any { return ty }
