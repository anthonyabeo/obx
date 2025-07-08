package hir

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
)

type Generator struct {
	ctx *report.Context
}

func (g Generator) VisitOberon(obx *ast.OberonX) any {
	for _, unit := range obx.Units {
		unit.Accept(g)
	}

	return nil
}

func (g Generator) VisitModule(module *ast.Module) any {
	name := module.BName
	var (
		globals    []Decl
		procedures []*Procedure
		stmts      *CompoundStmt
	)

	for _, declaration := range module.DeclSeq {
		switch decl := declaration.(type) {
		case *ast.ProcedureDecl:
			procedures = append(procedures, g.VisitProcedureDecl(decl).(*Procedure))
		case *ast.VariableDecl:
			globals = append(globals, decl.Accept(g).([]Decl)...)
		default:
			globals = append(globals, decl.Accept(g).(Decl))
		}
	}

	stmts = g.visitStmtSeq(module.StmtSeq)

	return &Module{
		Name:       name,
		Imports:    nil,
		Globals:    globals,
		Procedures: procedures,
		Init: &Procedure{
			Name: fmt.Sprintf("__init_%s", name),
			Body: stmts,
		},
	}
}

func (g Generator) VisitMetaSection(section *ast.MetaSection) any {
	//TODO implement me
	panic("implement me")
}

func (g Generator) VisitDefinition(definition *ast.Definition) any {
	//TODO implement me
	panic("implement me")
}

func (g Generator) VisitIdentifierDef(def *ast.IdentifierDef) any {
	//TODO implement me
	panic("implement me")
}

func (g Generator) VisitBinaryExpr(expr *ast.BinaryExpr) any {
	return &BinaryExpr{
		Left:  expr.Left.Accept(g).(Expr),
		Right: expr.Right.Accept(g).(Expr),
		Op:    g.emitOp(expr.Op),
		Ty:    g.emitType(expr.SemaType),
	}
}

func (g Generator) visitExprList(list []ast.Expression) (l []Expr) {
	for _, expression := range list {
		expr := expression.Accept(g).(Expr)
		l = append(l, expr)
	}

	return l
}

func (g Generator) VisitDesignator(dsg *ast.Designator) any {
	Dsg := dsg.QIdent.Accept(g).(Expr)

	for _, selector := range dsg.Select {
		switch sel := selector.(type) {
		case *ast.DotOp:
			Dsg = &FieldAccess{
				Record: Dsg,
				Field:  sel.Field,
				Ty:     g.emitType(sel.Symbol.Type()),
			}
		case *ast.IndexOp:
			Dsg = &IndexExpr{
				Array: Dsg,
				Index: g.visitExprList(sel.List),
				Ty:    Dsg.Type(),
			}
		case *ast.PtrDeref:
			Dsg = &DerefExpr{
				Pointer: Dsg,
				Ty:      Dsg.Type(),
			}
		case *ast.TypeGuard:
		}
	}

	return Dsg
}

func (g Generator) VisitFunctionCall(call *ast.FunctionCall) any {
	callee := call.Callee.Accept(g).(Expr)

	return &FuncCallExpr{
		Proc: callee,
		Args: g.visitExprList(call.ActualParams),
		Ty:   g.emitType(call.SemaType),
	}
}

func (g Generator) VisitUnaryExpr(expr *ast.UnaryExpr) any {
	return &UnaryExpr{
		Op: g.emitOp(expr.Op),
		E:  expr.Operand.Accept(g).(Expr),
		Ty: g.emitType(expr.SemaType),
	}
}

func (g Generator) VisitQualifiedIdent(ident *ast.QualifiedIdent) any {
	if len(ident.Prefix) != 0 {

	}

	return &VarExpr{
		Name: ident.Name,
		Ty:   g.emitType(ident.SemaType),
	}
}

func (g Generator) VisitSet(set *ast.Set) any {
	var elems []Expr
	for _, expression := range set.Elem {
		elems = append(elems, expression.Accept(g).(Expr))
	}

	return &SetExpr{Elems: elems, Ty: g.emitType(set.SemaType)}
}

func (g Generator) VisitBasicLit(lit *ast.BasicLit) any {
	switch lit.Kind {
	case token.BYTE_LIT, token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
		val, err := strconv.ParseInt(lit.Val, 10, 64)
		if err != nil {
			g.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  err.Error(),
				Range:    g.ctx.Source.Span(g.ctx.FileName, lit.StartOffset, lit.EndOffset),
			})
		}

		return &IntConst{
			Value: val,
			Ty:    g.emitType(lit.SemaType),
		}
	case token.REAL_LIT, token.LONGREAL_LIT:
		val, err := strconv.ParseFloat(lit.Val, 64)
		if err != nil {
			g.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  err.Error(),
				Range:    g.ctx.Source.Span(g.ctx.FileName, lit.StartOffset, lit.EndOffset),
			})
		}

		return &RealConst{
			Value: val,
			Ty:    g.emitType(lit.SemaType),
		}

	case token.CHAR_LIT, token.WCHAR_LIT:
		return &CharConst{
			Value: rune(lit.Val[0]),
			Ty:    g.emitType(lit.SemaType),
		}
	case token.TRUE, token.FALSE:
		val := true
		if strings.ToLower(lit.Val) == "false" {
			val = false
		}

		return &BoolConst{
			Value: val,
			Ty:    g.emitType(lit.SemaType),
		}

	case token.STR_LIT, token.HEX_STR_LIT:
		return &StringConst{
			Value: lit.Val,
			Ty:    g.emitType(lit.SemaType),
		}

	default:
		return nil
	}
}

func (g Generator) VisitExprRange(r *ast.ExprRange) any {
	low := r.Low.Accept(g).(Expr)
	high := r.High.Accept(g).(Expr)
	return &RangeExpr{Low: low, High: high}
}

func (g Generator) VisitNil(n *ast.Nil) any { return &NilConst{} }

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
		Lhs: stmt.LValue.Accept(g).(Expr),
		Rhs: stmt.RValue.Accept(g).(Expr),
	}
}

func (g Generator) VisitReturnStmt(stmt *ast.ReturnStmt) any {
	var result Expr
	if stmt.Value != nil {
		result = stmt.Value.Accept(g).(Expr)
	}
	return []Stmt{&ReturnStmt{Result: result}}
}

func (g Generator) VisitProcedureCall(call *ast.ProcedureCall) any {
	callee := call.Callee.Accept(g).(Expr)

	return &CallStmt{
		Proc: callee,
		Args: g.visitExprList(call.ActualParams),
	}
}

func (g Generator) VisitRepeatStmt(stmt *ast.RepeatStmt) any {
	body := &CompoundStmt{}

	// Process REPEAT body
	for _, s := range stmt.StmtSeq {
		body.Stmts = append(body.Stmts, s.Accept(g).(Stmt))
	}

	// Generate condition expression
	cond := stmt.BoolExpr.Accept(g).(Expr)

	// Emit exit condition
	// exit if cond
	exit := &IfStmt{
		Cond: cond,
		Then: &CompoundStmt{[]Stmt{&ExitStmt{loopLabel: stmt.Label}}},
	}

	body.Stmts = append(body.Stmts, exit)

	return &LoopStmt{Body: body, label: stmt.Label}
}

func (g Generator) VisitWhileStmt(stmt *ast.WhileStmt) any {
	body := &CompoundStmt{}

	// WHILE <cond>
	cond := stmt.BoolExpr.Accept(g).(Expr)
	negated := &UnaryExpr{Op: Not, E: cond}
	body.Stmts = append(body.Stmts, &IfStmt{
		Cond: negated,
		Then: &CompoundStmt{[]Stmt{&ExitStmt{loopLabel: stmt.Label}}},
	})

	// Body
	for _, s := range stmt.StmtSeq {
		body.Stmts = append(body.Stmts, s.Accept(g).(Stmt))
	}

	// ELSIF branches
	for _, branch := range stmt.ElsIfs {
		bCond := branch.BoolExpr.Accept(g).(Expr)
		negB := &UnaryExpr{Op: Not, E: bCond}

		body.Stmts = append(body.Stmts, &IfStmt{
			Cond: negB,
			Then: &CompoundStmt{
				[]Stmt{
					&ExitStmt{loopLabel: stmt.Label},
				},
			},
		})

		for _, s := range branch.ThenPath {
			body.Stmts = append(body.Stmts, s.Accept(g).(Stmt))
		}
	}

	return &LoopStmt{label: stmt.Label, Body: body}
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
	cvar := &VarExpr{Name: stmt.CtlVar.Name, Ty: g.emitType(stmt.CtlVar.SemaType)}
	init := stmt.InitVal.Accept(g).(Expr)
	final := stmt.FinalVal.Accept(g).(Expr)

	// Default step is 1
	var step Expr = &IntConst{Value: 1}
	if stmt.By != nil {
		step = stmt.By.Accept(g).(Expr)
	}

	// Initialize control variable: cvar = init
	initAssign := &AssignStmt{
		Lhs: cvar,
		Rhs: init,
	}

	// Condition: cvar > final => break
	breakIf := &IfStmt{
		Cond: &BinaryExpr{
			Op:    GT,
			Left:  cvar,
			Right: final,
		},
		Then: &CompoundStmt{[]Stmt{&ExitStmt{}}},
	}

	// Generate loop body
	var bodyStmts []Stmt
	for _, s := range stmt.StmtSeq {
		bodyStmts = append(bodyStmts, s.Accept(g).([]Stmt)...)
	}

	// Increment: cvar = cvar + step
	incr := &AssignStmt{
		Lhs: cvar,
		Rhs: &BinaryExpr{
			Op:    Add,
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
	return &ExitStmt{loopLabel: stmt.Label}
}

func (g Generator) VisitWithStmt(stmt *ast.WithStmt) any {
	var guards []*WithGuard

	for _, arm := range stmt.Arms {
		guard := arm.Accept(g).(*WithGuard)
		guards = append(guards, guard)
	}

	return []Stmt{
		&WithStmt{
			Guards: guards,
			Else:   g.visitStmtSeq(stmt.Else),
		},
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

func (g Generator) VisitImport(i *ast.Import) any {
	//TODO implement me
	panic("implement me")
}

func (g Generator) VisitProcedureDecl(decl *ast.ProcedureDecl) any {
	name := decl.Head.Name.Name

	// === Parameters ===
	var (
		params     []*Param
		resultType Type
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
			resultType = decl.Head.FP.RetType.Accept(g).(Type)
		}
	}

	// === Body ===
	if decl.Body != nil {
		locals = g.visitDeclSeq(decl.Body.DeclSeq)
		body = g.visitStmtSeq(decl.Body.StmtSeq)
	}

	return &Procedure{
		Name:       name,
		Params:     params,
		Result:     resultType,
		Locals:     locals,
		Body:       body,
		IsExported: decl.Head.Name.Symbol.Props() == ast.Exported,
	}
}

func (g Generator) VisitProcedureHeading(head *ast.ProcedureHeading) any { return head }

func (g Generator) VisitProcedureBody(body *ast.ProcedureBody) any { return body }

func (g Generator) VisitReceiver(recv *ast.Receiver) any {
	return &Param{
		Name: recv.Name.Name,
		Type: g.emitType(recv.Type.Type()),
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

	typ := sec.Type.Accept(g).(Type)
	kind := g.emitParamKind(sec.Kind)

	for _, id := range sec.Names {
		params = append(params, &Param{
			Name: id.Name,
			Type: typ,
			Kind: kind, // "VAR", "IN", etc
		})
	}

	return params
}

func (g Generator) VisitVariableDecl(decl *ast.VariableDecl) any {
	var decls []Decl

	ty := decl.Type.Accept(g).(Type)

	for _, id := range decl.IdentList {
		decls = append(decls, &VarDecl{
			Name: id.Name,
			Type: ty,
		})
	}

	return decls
}

func (g Generator) VisitConstantDecl(decl *ast.ConstantDecl) any {
	return &ConstDecl{
		Name:  decl.Name.Name,
		Type:  g.emitType(decl.Name.SemaType),
		Value: decl.Value.Accept(g).(ConstValue),
	}
}

func (g Generator) VisitTypeDecl(decl *ast.TypeDecl) any {
	hirType := decl.DenotedType.Accept(g).(Type)

	return &TypeDecl{
		Name: decl.Name.Name,
		Type: hirType,
	}
}

func (g Generator) VisitBasicType(ty *ast.BasicType) any {
	switch ty.Kind {
	case token.BYTE:
		return &IntType{Bits: 8, Signed: false}
	case token.INT8, token.INT16, token.INT32, token.INT64, token.SHORTINT, token.INTEGER, token.LONGINT:
		var size int
		switch ty.Kind {
		case token.INT8:
			size = 8
		case token.INT16:
			size = 16
		case token.INT32:
			size = 32
		case token.INT64:
			size = 64
		case token.SHORTINT:
			size = 16
		case token.INTEGER:
			size = 32
		case token.LONGINT:
			size = 64
		default:
			size = 1
		}

		return &IntType{Bits: size, Signed: true}
	case token.REAL, token.LONGREAL:
		size := 4
		if ty.Kind == token.LONGREAL {
			size = 8
		}
		return &RealType{Bits: size}
	case token.CHAR, token.WCHAR:
		return &CharType{}
	case token.BOOLEAN:
		return &BoolType{}
	case token.SET:
		return &SetType{}
	case token.NIL:
		return &NilType{}
	default:
		return &UnknownType{}
	}
}

func (g Generator) VisitArrayType(ty *ast.ArrayType) any {
	elemType := ty.ElemType.Accept(g).(Type)

	if ty.LenList == nil || len(ty.LenList.List) == 0 {
		// Open array: create single dimension with Length = -1
		return &ArrayType{
			Length: -1,
			Base:   elemType,
		}
	}

	dims := g.VisitLenList(ty.LenList).([]int64)

	// Wrap innermost to outermost
	typ := elemType
	for i := len(dims) - 1; i >= 0; i-- {
		typ = &ArrayType{
			Length: dims[i],
			Base:   typ,
		}
	}

	return typ

}

func (g Generator) VisitLenList(list *ast.LenList) any {
	if list == nil {
		return nil
	}

	isVar := list.Modifier == token.VAR
	var lengths []int64

	for _, expr := range list.List {
		expr.Accept(g)

		if isVar {
			lengths = append(lengths, -1)
		} else {
			// Constant-length expression
			val := ast.EvalConstExpr(expr)
			n := val.(int64)
			lengths = append(lengths, n)
		}
	}

	return lengths
}

func (g Generator) VisitPointerType(ty *ast.PointerType) any {
	return &PointerType{Base: ty.Base.Accept(g).(Type)}
}

func (g Generator) VisitProcedureType(ty *ast.ProcedureType) any {
	proc := &ProcedureType{IsTypeBoundType: ty.IsTypeBound}
	if ty.FP != nil {
		proc.Params = ty.FP.Accept(g).([]*Param)
		if ty.FP.RetType != nil {
			proc.Result = ty.FP.RetType.Accept(g).(Type)
		}
	}

	return proc
}

func (g Generator) VisitRecordType(ty *ast.RecordType) any {
	var fields []*Field
	for _, field := range ty.Env.Elems() {
		fields = append(fields, &Field{
			Name: field.Name(),
			Type: g.emitType(field.Type()),
		})
	}

	return &RecordType{
		Base:   ty.Base.Accept(g).(*RecordType),
		Fields: fields,
	}
}

func (g Generator) VisitFieldList(list *ast.FieldList) any { return list }

func (g Generator) VisitEnumType(ty *ast.EnumType) any {
	names := map[string]int{}

	for i, variant := range ty.Variants {
		names[variant.Name] = i
	}

	return &types.EnumType{Variants: names}
}

func (g Generator) VisitNamedType(ty *ast.NamedType) any {
	ty.Name.Accept(g)
	return &NamedType{Name: ty.Name.String(), Symbol: ty.Symbol}
}

func (g Generator) VisitBadExpr(expr *ast.BadExpr) any { return expr }

func (g Generator) VisitBadDecl(decl *ast.BadDecl) any { return decl }

func (g Generator) VisitBadStmt(stmt *ast.BadStmt) any { return stmt }

func (g Generator) VisitBadType(ty *ast.BadType) any { return ty }
