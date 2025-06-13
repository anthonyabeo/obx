package sema

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
	"math"
	"strconv"
)

type TypeChecker struct {
	ctx *report.Context
}

func NewTypeChecker(ctx *report.Context) *TypeChecker {
	return &TypeChecker{ctx: ctx}
}

func (t *TypeChecker) TypeCheck(unit ast.CompilationUnit) {
	unit.Accept(t)
}

func (t *TypeChecker) VisitOberon(x *ast.OberonX) any {
	for _, unit := range x.Units {
		unit.Accept(t)
	}

	return x
}

func (t *TypeChecker) VisitModule(module *ast.Module) any {
	for _, meta := range module.MetaParams {
		meta.Accept(t)
	}

	for _, imp := range module.ImportList {
		imp.Accept(t)
	}

	for _, decl := range module.DeclSeq {
		decl.Accept(t)
	}

	for _, stmt := range module.StmtSeq {
		stmt.Accept(t)
	}

	return module
}

func (t *TypeChecker) VisitMetaSection(section *ast.MetaSection) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitDefinition(def *ast.Definition) any {
	for _, i := range def.ImportList {
		i.Accept(t)
	}

	for _, declaration := range def.DeclSeq {
		declaration.Accept(t)
	}

	return def
}

func (t *TypeChecker) VisitIdentifierDef(def *ast.IdentifierDef) any {
	if def.Symbol == nil {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "unresolved identifier: " + def.Name,
			Range:    t.ctx.Source.Span(t.ctx.FileName, def.StartOffset, def.EndOffset),
		})

		return nil
	}

	return def
}

func (t *TypeChecker) VisitBinaryExpr(expr *ast.BinaryExpr) any {
	expr.Left.Accept(t)
	expr.Right.Accept(t)

	tx := expr.Left.Type()
	ty := expr.Right.Type()
	op := expr.Op

	ok, typ := types.ExpressionCompatible(op, tx, ty)
	if !ok {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("incompatible types for binary operator %s: %s and %s", op, tx.String(), ty.String()),
			Range:    t.ctx.Source.Span(t.ctx.FileName, expr.StartOffset, expr.EndOffset),
		})

		expr.SemaType = types.UnknownType
		return expr
	}

	expr.SemaType = typ

	return expr
}

func (t *TypeChecker) VisitDesignator(dsg *ast.Designator) any {
	dsg.QIdent.Accept(t)

	typ := dsg.QIdent.SemaType

	for _, sel := range dsg.Select {
		switch s := sel.(type) {
		case *ast.IndexOp:
			base := types.Underlying(typ)

			switch arr := base.(type) {
			case *types.ArrayType:
				for _, expr := range s.List {
					expr.Accept(t)
					if !types.IsInteger(expr.Type()) {
						t.ctx.Reporter.Report(report.Diagnostic{
							Severity: report.Error,
							Message:  fmt.Sprintf("array index '%s' does not resolve to an integer", expr),
							Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Pos(), expr.End()),
						})
					}
				}

				typ = arr.Base
			case *types.PointerType:
				a, ok := types.Underlying(arr.Base).(*types.ArrayType)
				if !ok {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("cannot index non-array pointer: '%v'", arr),
						Range:    t.ctx.Source.Span(t.ctx.FileName, dsg.StartOffset, dsg.EndOffset),
					})
				}

				for _, expr := range s.List {
					expr.Accept(t)
					if !types.SameType(expr.Type(), types.IntegerType) {
						t.ctx.Reporter.Report(report.Diagnostic{
							Severity: report.Error,
							Message:  fmt.Sprintf("array index '%s' does not resolve to an integer", expr),
							Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Pos(), expr.End()),
						})
					}
				}

				typ = a.Base
			default:
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("cannot index non-array: '%v'", arr),
					Range:    t.ctx.Source.Span(t.ctx.FileName, dsg.StartOffset, dsg.EndOffset),
				})
			}
		case *ast.DotOp:
			rec, ok := types.Underlying(typ).(*types.RecordType)
			if !ok {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("cannot select field '%v' of non-record: '%v'", s.Field, dsg.QIdent),
					Range:    t.ctx.Source.Span(t.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
				})

				return dsg
			}

			var field *types.Field
			for _, f := range rec.Fields {
				if f.Name == s.Field {
					field = &f
				}
			}

			if field == nil {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("record field '%v' not found", s.Field),
					Range:    t.ctx.Source.Span(t.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
				})

				return dsg
			}

			typ = field.Type
		case *ast.PtrDeref:
			ptr, ok := typ.(*types.PointerType)
			if !ok {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("cannot dereference non-pointer variable: '%v'", dsg.QIdent),
					Range:    t.ctx.Source.Span(t.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
				})

				return dsg
			}

			typ = ptr.Base
		case *ast.TypeGuard:
			// Get base type of the designator before the type guard
			base := dsg.SemaType

			NamedTy, ok := s.Ty.(*ast.QualifiedIdent)
			if !ok {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("name identifier: '%v'", dsg.QIdent),
					Range:    t.ctx.Source.Span(t.ctx.FileName, s.Ty.Pos(), s.Ty.End()),
				})
			}

			ty := NamedTy.Symbol.TypeNode().Accept(t).(types.Type)
			target := ty // s.Type is an ast.Type node

			// 1. Check if base is allowed (must be VAR param of record, or pointer to record)
			isPointerToRecord := types.IsPointerToRecord(base)
			isVarParamRecord := ast.IsVarParam(dsg) && types.IsRecord(base)

			if !(isPointerToRecord || isVarParamRecord) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("type guard only allowed on VAR parameters of record "+
						"type or pointer to record types (got %s)", base),
					Range: t.ctx.Source.Span(t.ctx.FileName, dsg.Pos(), dsg.End()),
				})

				return dsg
			}

			// 2. Ensure T is a record or pointer to record
			if !(types.IsPointerToRecord(target) || types.IsRecord(target)) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("type guard target must be a (pointer to) record (got %s)", target),
				})
				return dsg
			}

			// 3. Ensure target is an extension of base
			if !types.IsExtensionOf(target, base) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("type guard target %s is not an extension of %s", target, base),
				})
				return dsg
			}

			dsg.SemaType = target
		}
	}

	dsg.SemaType = typ
	return dsg
}

func (t *TypeChecker) VisitFunctionCall(call *ast.FunctionCall) any {
	call.Callee.Accept(t)

	calleeType := call.Callee.Type()
	procType, ok := calleeType.(*types.ProcedureType)
	if !ok {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("cannot call '%v' of non-procedure", call.Callee),
			Range:    t.ctx.Source.Span(t.ctx.FileName, call.Callee.StartOffset, call.Callee.EndOffset),
		})
		return call
	}

	if len(call.ActualParams) != len(procType.Params) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message: fmt.Sprintf("wrong number of arguments for '%v'. expected %d arguments, got %d",
				call.Callee, len(procType.Params), len(call.ActualParams)),
		})

		return call
	}

	for i, arg := range call.ActualParams {
		arg.Accept(t)

		formal := procType.Params[i]
		actual := arg.Type()

		if !types.ParameterCompatible(actual, formal) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("argument %d: of type, '%s' incompatible with parameter of type '%s'",
					i+1, actual.String(), formal.Type.String()),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.Callee.StartOffset, call.Callee.EndOffset),
			})
		}
	}

	call.SemaType = procType.Result

	return call
}

func (t *TypeChecker) VisitUnaryExpr(expr *ast.UnaryExpr) any {
	expr.Operand.Accept(t)

	tx := expr.Operand.Type()
	op := expr.Op

	switch op {
	case token.PLUS, token.MINUS:
		if !types.IsNumeric(tx) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("unary operator '%s' not applicable to type '%s'", op, tx),
				Range:    t.ctx.Source.Span(t.ctx.FileName, expr.StartOffset, expr.EndOffset),
			})
			expr.SemaType = types.UnknownType
		} else {
			switch e := expr.Operand.(type) {
			case *ast.BasicLit:
				if e.Kind == token.REAL || e.Kind == token.LONGREAL {
					value, err := strconv.ParseFloat(e.Val, 10)
					if err != nil {
						t.ctx.Reporter.Report(report.Diagnostic{
							Severity: report.Fatal,
							Message:  fmt.Sprintf("could not parse '%s' to real", e.Val),
							Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Operand.Pos(), expr.Operand.End()),
						})
					}

					f32 := float32(value)
					back := float64(f32)
					if back == value {
						expr.SemaType = types.RealType
					} else {
						expr.SemaType = types.LongRealType
					}
				} else {
					value, err := strconv.ParseInt(e.Val, 10, 64)
					if err != nil {
						t.ctx.Reporter.Report(report.Diagnostic{
							Severity: report.Fatal,
							Message:  fmt.Sprintf("could not parse '%s' to int", e.Val),
							Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Operand.Pos(), expr.Operand.End()),
						})
					}

					switch {
					case -value >= 0 && -value <= 255:
						expr.SemaType = types.ByteType
					case -value >= -128 && -value <= 127:
						expr.SemaType = types.Int8Type
					case -value >= -32768 && -value <= 32767:
						expr.SemaType = types.Int16Type
					case -value >= -2147483648 && -value <= 2147483647:
						expr.SemaType = types.Int32Type
					default:
						expr.SemaType = types.Int64Type
					}
				}

			default:
				expr.SemaType = tx
			}

		}

	case token.NOT:
		if !types.IsBoolean(tx) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("unary operator '%s' not applicable to type '%s'", op, tx),
				Range:    t.ctx.Source.Span(t.ctx.FileName, expr.StartOffset, expr.EndOffset),
			})

			expr.SemaType = types.UnknownType
		} else {
			expr.SemaType = types.BooleanType
		}
	default:
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("unsupported unary operator: %s", op),
			Range:    t.ctx.Source.Span(t.ctx.FileName, expr.StartOffset, expr.EndOffset),
		})
		expr.SemaType = types.UnknownType
	}

	return expr
}

func (t *TypeChecker) VisitQualifiedIdent(ident *ast.QualifiedIdent) any {
	if ident.Symbol == nil {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "unresolved identifier: " + ident.Name,
			Range:    t.ctx.Source.Span(t.ctx.FileName, ident.StartOffset, ident.EndOffset),
		})

		ident.SemaType = types.UnknownType
		return nil
	}

	ident.SemaType = ident.Symbol.Type()
	return ident
}

func (t *TypeChecker) VisitSet(n *ast.Set) any {
	for _, elem := range n.Elem {
		switch e := elem.(type) {
		case *ast.ExprRange:
			e.Accept(t)
			t.assertInteger(e.Low, "range start must be integer")
			t.assertInteger(e.High, "range end must be integer")
			t.assertConst(e.Low)
			t.assertConst(e.High)

			low := t.EvalConstUint32(e.Low)
			high := t.EvalConstUint32(e.High)

			if low > high {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("invalid set range: low (%d) > high (%d)", low, high),
					Range:    t.ctx.Source.Span(t.ctx.FileName, elem.Pos(), elem.End()),
				})
			}

		case ast.Expression:
			e.Accept(t)
			t.assertInteger(e, "set element must be integer")
			t.assertConst(e)

			val := t.EvalConstUint32(e)
			_ = val // validated by EvalConstUint32

		default:
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "invalid set element",
				Range:    t.ctx.Source.Span(t.ctx.FileName, elem.Pos(), elem.End()),
			})
		}
	}

	n.SemaType = types.SetType
	return n.SemaType
}

func (t *TypeChecker) VisitBasicLit(lit *ast.BasicLit) any {
	switch lit.Kind {
	case token.BYTE_LIT:
		lit.SemaType = types.ByteType
	case token.INT8_LIT:
		lit.SemaType = types.Int8Type
	case token.INT16_LIT:
		lit.SemaType = types.Int16Type
	case token.INT32_LIT:
		lit.SemaType = types.Int32Type
	case token.INT64_LIT:
		lit.SemaType = types.Int64Type
	case token.REAL_LIT:
		lit.SemaType = types.RealType
	case token.LONGREAL_LIT:
		lit.SemaType = types.LongRealType
	case token.CHAR_LIT:
		lit.SemaType = types.CharType
	case token.HEX_STR_LIT:
		lit.SemaType = &types.StringType{}
	case token.STR_LIT:
		lit.SemaType = &types.StringType{Length: len(lit.Val)}
	case token.TRUE:
		lit.SemaType = types.BooleanType
	case token.FALSE:
		lit.SemaType = types.BooleanType
	default:
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "Type Error: unknown literal kind: " + lit.Val,
			Range:    t.ctx.Source.Span(t.ctx.FileName, lit.StartOffset, lit.EndOffset),
		})

		lit.SemaType = types.UnknownType
	}

	return lit
}

func (t *TypeChecker) VisitExprRange(n *ast.ExprRange) any {
	tLow := n.Low.Accept(t).(types.Type)
	tHigh := n.High.Accept(t).(types.Type)

	if !types.EqualType(tLow, tHigh) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("type mismatchin set range: expected %s but got %s", tLow, tHigh),
			Range:    t.ctx.Source.Span(t.ctx.FileName, n.StartOffset, n.EndOffset),
		})
	}

	n.SemaType = tLow
	return tLow
}

func (t *TypeChecker) VisitNil(n *ast.Nil) any {
	n.SemaType = types.NilType
	return n
}

func (t *TypeChecker) VisitIfStmt(stmt *ast.IfStmt) any {
	stmt.BoolExpr.Accept(t)
	if !types.IsBoolean(stmt.BoolExpr.Type()) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message: fmt.Sprintf("condition expression '%s' evaluates to '%s' type. expecting boolean",
				stmt.BoolExpr, stmt.BoolExpr),
			Range: t.ctx.Source.Span(t.ctx.FileName, stmt.BoolExpr.Pos(), stmt.BoolExpr.End()),
		})
	}

	for _, statement := range stmt.ThenPath {
		statement.Accept(t)
	}

	for _, branch := range stmt.ElseIfBranches {
		branch.Accept(t)
	}

	for _, statement := range stmt.ElsePath {
		statement.Accept(t)
	}

	return stmt
}

func (t *TypeChecker) VisitElseIfBranch(branch *ast.ElseIfBranch) any {
	branch.BoolExpr.Accept(t)
	if !types.IsBoolean(branch.BoolExpr.Type()) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message: fmt.Sprintf("condition expression '%s' evaluates to '%s' type. expecting boolean",
				branch.BoolExpr, branch.BoolExpr),
			Range: t.ctx.Source.Span(t.ctx.FileName, branch.BoolExpr.Pos(), branch.BoolExpr.End()),
		})
	}

	for _, statement := range branch.ThenPath {
		statement.Accept(t)
	}

	return branch
}

func (t *TypeChecker) VisitAssignmentStmt(stmt *ast.AssignmentStmt) any {
	stmt.LValue.Accept(t)
	stmt.RValue.Accept(t)

	tv := stmt.LValue.Type() // variable type
	te := stmt.RValue.Type() // expression type

	// Ensure the LHS is a valid variable designator
	if !t.isAssignable(stmt.LValue) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("left-hand side of assignment assignment, '%s', is not assignable", stmt.LValue),
			Range:    t.ctx.Source.Span(t.ctx.FileName, stmt.LValue.Pos(), stmt.LValue.End()),
		})
		return stmt
	}

	if !types.AssignmentCompatible(te, tv) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message: fmt.Sprintf("cannot assign expression (%s: %s)  to variable (%s: %s)",
				stmt.RValue, te, stmt.LValue, tv.String()),
			Range: t.ctx.Source.Span(t.ctx.FileName, stmt.StartOffset, stmt.EndOffset),
		})
	}

	return stmt
}

func (t *TypeChecker) VisitReturnStmt(stmt *ast.ReturnStmt) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitProcedureCall(call *ast.ProcedureCall) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitRepeatStmt(stmt *ast.RepeatStmt) any {
	for _, statement := range stmt.StmtSeq {
		statement.Accept(t)
	}

	stmt.BoolExpr.Accept(t)
	if !types.IsBoolean(stmt.BoolExpr.Type()) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message: fmt.Sprintf("condition expression '%s' evaluates to '%s' type. expecting boolean",
				stmt.BoolExpr, stmt.BoolExpr),
			Range: t.ctx.Source.Span(t.ctx.FileName, stmt.BoolExpr.Pos(), stmt.BoolExpr.End()),
		})
	}

	return stmt
}

func (t *TypeChecker) VisitWhileStmt(stmt *ast.WhileStmt) any {
	stmt.BoolExpr.Accept(t)
	if !types.IsBoolean(stmt.BoolExpr.Type()) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message: fmt.Sprintf("condition expression '%s' evaluates to '%s' type. expecting boolean",
				stmt.BoolExpr, stmt.BoolExpr),
			Range: t.ctx.Source.Span(t.ctx.FileName, stmt.BoolExpr.Pos(), stmt.BoolExpr.End()),
		})
	}

	for _, statement := range stmt.StmtSeq {
		statement.Accept(t)
	}

	for _, elsIf := range stmt.ElsIfs {
		elsIf.Accept(t)
	}

	return stmt
}

func (t *TypeChecker) VisitLoopStmt(stmt *ast.LoopStmt) any {
	for _, statement := range stmt.StmtSeq {
		statement.Accept(t)
	}

	return stmt
}

func (t *TypeChecker) VisitCaseStmt(stmt *ast.CaseStmt) any {
	stmt.Expr.Accept(t)
	caseType := stmt.Expr.Type()

	switch ty := caseType.(type) {
	case *types.BasicType:
		t.checkConstantCase(stmt, ty)
	case *types.EnumType:
		t.checkConstantCase(stmt, ty)
	case *types.PointerType:
		if _, ok := ty.Base.(*types.RecordType); ok {
			t.checkTypeCase(stmt, ty)
			return stmt
		}
	case *types.RecordType:
		t.checkTypeCase(stmt, ty)
		return stmt
	default:
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("invalid CASE expression type '%s'", caseType),
			Range:    t.ctx.Source.Span(t.ctx.FileName, stmt.Expr.Pos(), stmt.Expr.End()),
		})
	}

	return stmt
}

func (t *TypeChecker) VisitCase(c *ast.Case) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitLabelRange(labelRange *ast.LabelRange) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitForStmt(stmt *ast.ForStmt) any {
	stmt.CtlVar.Accept(t)
	ctlType := stmt.CtlVar.Type()

	if !(types.IsInteger(ctlType) || types.IsEnum(ctlType)) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message: fmt.Sprintf("control variable %s must be of integer or enumeration type",
				stmt.CtlVar.Name),
			Range: t.ctx.Source.Span(t.ctx.FileName, stmt.CtlVar.Pos(), stmt.CtlVar.End()),
		})

		return stmt
	}

	// Type check InitVal and FinalVal
	stmt.InitVal.Accept(t)
	stmt.FinalVal.Accept(t)

	if !types.AssignmentCompatible(stmt.InitVal.Type(), ctlType) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message: fmt.Sprintf("initial value, '%s' cannot be assiged to control variable '%s'",
				stmt.InitVal, stmt.CtlVar),
			Range: t.ctx.Source.Span(t.ctx.FileName, stmt.CtlVar.Pos(), stmt.InitVal.End()),
		})
	}

	if !types.AssignmentCompatible(stmt.FinalVal.Type(), ctlType) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message: fmt.Sprintf("final value, '%s' cannot be assiged to control variable '%s'",
				stmt.FinalVal, stmt.CtlVar),
			Range: t.ctx.Source.Span(t.ctx.FileName, stmt.CtlVar.Pos(), stmt.FinalVal.End()),
		})
	}

	// BY expression only allowed for integers
	if stmt.By != nil {
		if !types.IsInteger(ctlType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "BY expression only allowed for integer control variables",
				Range:    t.ctx.Source.Span(t.ctx.FileName, stmt.By.Pos(), stmt.By.End()),
			})
		} else {
			stmt.By.Accept(t)
			if !ast.IsConstExpr(stmt.By) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "BY expression must be a constant",
					Range:    t.ctx.Source.Span(t.ctx.FileName, stmt.By.Pos(), stmt.By.End()),
				})
			}

			if ast.IsZeroConstExpr(stmt.By) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "BY expression must be nonzero",
					Range:    t.ctx.Source.Span(t.ctx.FileName, stmt.By.Pos(), stmt.By.End()),
				})
			}

			if !types.AssignmentCompatible(stmt.By.Type(), ctlType) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("BY expression (%s: %s), not assignable to (%s: %s)",
						stmt.By, stmt.By.Type(), stmt.CtlVar, ctlType),
					Range: t.ctx.Source.Span(t.ctx.FileName, stmt.CtlVar.Pos(), stmt.By.End()),
				})
			}
		}
	}

	for _, s := range stmt.StmtSeq {
		s.Accept(t)
	}

	return stmt
}

func (t *TypeChecker) VisitExitStmt(stmt *ast.ExitStmt) any { return stmt }

func (t *TypeChecker) VisitWithStmt(stmt *ast.WithStmt) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitGuard(guard *ast.Guard) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitImport(i *ast.Import) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitProcedureDecl(decl *ast.ProcedureDecl) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitVariableDecl(decl *ast.VariableDecl) any {
	ty := decl.Type.Accept(t).(types.Type)
	for _, id := range decl.IdentList {
		id.Symbol.SetType(ty)
		id.SemaType = ty
	}

	return decl
}

func (t *TypeChecker) VisitConstantDecl(decl *ast.ConstantDecl) any {
	if !ast.IsConstExpr(decl.Value) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "operands of constant expression must be constants",
			Range:    t.ctx.Source.Span(t.ctx.FileName, decl.Value.Pos(), decl.Value.End()),
		})
	}

	decl.Value.Accept(t)
	if types.IsUnknownType(decl.Value.Type()) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("unknown type for value, '%s', in constant declaration", decl.Value),
			Range:    t.ctx.Source.Span(t.ctx.FileName, decl.StartOffset, decl.EndOffset),
		})

		return decl
	}

	ty := decl.Value.Type()

	decl.Name.Symbol.SetType(ty)
	decl.Name.SemaType = ty

	return decl
}

func (t *TypeChecker) VisitTypeDecl(decl *ast.TypeDecl) any {
	ty := decl.DenotedType.Accept(t).(types.Type)
	decl.Name.SemaType = ty

	return decl
}

func (t *TypeChecker) VisitProcedureHeading(heading *ast.ProcedureHeading) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitProcedureBody(body *ast.ProcedureBody) any {
	for _, declaration := range body.DeclSeq {
		declaration.Accept(t)
	}

	for _, statement := range body.StmtSeq {
		statement.Accept(t)
	}

	return body
}

func (t *TypeChecker) VisitReceiver(receiver *ast.Receiver) any {
	//TODO implement me
	panic("implement me")
}

func (t *TypeChecker) VisitFPSection(section *ast.FPSection) any {
	typ := section.Type.Accept(t).(types.Type)

	var kind string
	switch section.Mod {
	case token.VAR:
		kind = "VAR"
	case token.IN:
		kind = "IN"
	default:
		kind = "VALUE"
	}

	var names []string
	for _, id := range section.Names {
		names = append(names, id.Name)
	}

	return &types.FormalParam{
		Kind:  kind,
		Names: names,
		Type:  typ,
	}
}

func (t *TypeChecker) VisitFormalParams(params *ast.FormalParams) any {
	var p []*types.FormalParam
	for _, sec := range params.Params {
		fp := t.VisitFPSection(sec).(*types.FormalParam)
		p = append(p, fp)
	}
	return params
}

func (t *TypeChecker) VisitBasicType(ty *ast.BasicType) any {
	typ, ok := types.PredefinedTypes[ty.Kind]
	if !ok {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "unknown type: " + ty.Kind.String(),
			Range:    t.ctx.Source.Span(t.ctx.FileName, ty.StartOffset, ty.EndOffset),
		})

		return types.UnknownType
	}

	return typ
}

func (t *TypeChecker) VisitArrayType(ty *ast.ArrayType) any {
	elemType := ty.ElemType.Accept(t).(types.Type)

	if ty.LenList == nil || len(ty.LenList.List) == 0 {
		// Open array: create single dimension with Length = -1
		return &types.ArrayType{
			Length: -1,
			Base:   elemType,
		}
	}

	dims := t.VisitLenList(ty.LenList).([]int64)

	// Wrap innermost to outermost
	typ := elemType
	for i := len(dims) - 1; i >= 0; i-- {
		typ = &types.ArrayType{
			Length: dims[i],
			Base:   typ,
		}
	}

	return typ
}

func (t *TypeChecker) VisitLenList(list *ast.LenList) any {
	if list == nil {
		return nil
	}

	isVar := list.Modifier == token.VAR
	var lengths []int64

	for _, expr := range list.List {
		expr.Accept(t)

		if isVar {
			// Variable-length expression
			if !types.IsInteger(expr.Type()) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("VAR array length must be of integer type, %s", expr),
					Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Pos(), expr.End()),
				})
			}
			lengths = append(lengths, -1)
		} else {
			// Constant-length expression
			val := ast.EvalConstExpr(expr)
			n, ok := val.(int64)
			if !ok || n < 0 {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "array length must be a constant non-negative integer",
					Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Pos(), expr.End()),
				})
			}
			lengths = append(lengths, n)
		}
	}

	return lengths
}

func (t *TypeChecker) VisitPointerType(ty *ast.PointerType) any {
	base := ty.Base.Accept(t).(types.Type)
	if base == nil {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("pointer base '%s' is not a type", ty.Base),
			Range:    t.ctx.Source.Span(t.ctx.FileName, ty.Base.Pos(), ty.Base.End()),
		})

		return types.UnknownType
	}

	return &types.PointerType{Base: base}
}

func (t *TypeChecker) VisitProcedureType(ty *ast.ProcedureType) any {
	proc := &types.ProcedureType{}
	if ty.FP != nil {
		proc.Params = ty.FP.Accept(t).([]types.FormalParam)
		proc.Result = ty.FP.RetType.Accept(t).(types.Type)
	}

	return proc
}

func (t *TypeChecker) VisitRecordType(ty *ast.RecordType) any {
	recTy := &types.RecordType{}
	if ty.Base != nil {
		baseTy := ty.Base.Accept(t).(*types.RecordType)
		if baseTy == nil {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("expected parent of record to be record type, got '%v'", ty.Base),
				Range:    t.ctx.Source.Span(t.ctx.FileName, ty.StartOffset, ty.EndOffset),
			})

			return types.UnknownType
		}

		recTy.Base = baseTy
	}

	for _, field := range ty.Fields {
		field.Env = ty.Env
		f := field.Accept(t).([]types.Field)
		recTy.Fields = append(recTy.Fields, f...)
	}

	return recTy
}

func (t *TypeChecker) VisitFieldList(list *ast.FieldList) any {
	typ := list.Type.Accept(t).(types.Type)

	fields := make([]types.Field, 0)
	for _, def := range list.List {
		if sym := list.Env.Lookup(def.Name); sym == nil || sym.Kind() != ast.FieldSymbolKind {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("could not resolve field '%s'.", def.Name),
				Range:    t.ctx.Source.Span(t.ctx.FileName, def.StartOffset, def.EndOffset),
			})
		}

		fields = append(fields, types.Field{
			Name: def.Name,
			Type: typ,
		})

	}

	return fields
}

func (t *TypeChecker) VisitEnumType(ty *ast.EnumType) any {
	names := map[string]int{}

	for i, variant := range ty.Variants {
		if _, exists := names[variant]; exists {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("enum variant '%s' is defined more than once", variant),
				Range:    t.ctx.Source.Span(t.ctx.FileName, ty.StartOffset, ty.EndOffset),
			})
		} else {
			names[variant] = i
		}
	}

	return &types.EnumType{Variants: names}
}

func (t *TypeChecker) VisitNamedType(ty *ast.NamedType) any {
	ty.Name.Accept(t)
	return &types.NamedType{Name: ty.Name.String(), Def: ty.Name.SemaType}
}

func (t *TypeChecker) VisitBadExpr(expr *ast.BadExpr) any { return expr }

func (t *TypeChecker) VisitBadDecl(decl *ast.BadDecl) any { return decl }

func (t *TypeChecker) VisitBadStmt(stmt *ast.BadStmt) any { return stmt }

func (t *TypeChecker) VisitBadType(ty *ast.BadType) any { return ty }

func (t *TypeChecker) isAssignable(e ast.Expression) bool {
	designator, ok := e.(*ast.Designator)
	if !ok {
		return false
	}

	sym := designator.QIdent.Symbol
	if sym == nil {
		return false
	}

	switch sym := sym.(type) {
	case *ast.VariableSymbol:
		return true
	case *ast.ParamSymbol:
		return sym.Mod == token.VAR
	default:
		return false
	}
}

func (t *TypeChecker) checkConstantCase(n *ast.CaseStmt, exprType types.Type) {
	seen := map[interface{}]ast.Node{}
	for _, branch := range n.Cases {
		for _, label := range branch.CaseLabelList {
			if !ast.IsConstExpr(label.Low) {

				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "CASE label must be a constant",
					Range:    t.ctx.Source.Span(t.ctx.FileName, label.Pos(), label.End()),
				})
				continue
			}

			value := ast.EvalConstExpr(label.Low)
			if !types.TypeIncludes(exprType, label.Low.Type()) && !types.EqualType(exprType, label.Low.Type()) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "CASE label type incompatible with case expression",
					Range:    t.ctx.Source.Span(t.ctx.FileName, label.Pos(), label.End()),
				})
			}

			if prev, ok := seen[value]; ok {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("duplicate CASE label (previous at offset %d)", prev.Pos()),
				})
			}
			seen[value] = label
		}
	}
}

func (t *TypeChecker) checkTypeCase(n *ast.CaseStmt, exprType types.Type) {
	base := types.BaseRecord(exprType)
	seen := map[string]ast.Node{}

	for _, branch := range n.Cases {
		if len(branch.CaseLabelList) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "type CASE branch must have exactly one type label",
				Range:    t.ctx.Source.Span(t.ctx.FileName, branch.StartOffset, branch.EndOffset),
			})
			continue
		}
		label := branch.CaseLabelList[0]
		labelType := label.Low.Type()

		if types.IsNil(labelType) {
			if _, ok := exprType.(*types.PointerType); !ok {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "NIL is only valid in pointer-based CASE statements",
					Range:    t.ctx.Source.Span(t.ctx.FileName, label.Pos(), label.End()),
				})
			}
			continue
		}

		if _, ok := labelType.(types.Type); !ok {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "CASE label must be a type",
				Range:    t.ctx.Source.Span(t.ctx.FileName, label.Pos(), label.End()),
			})
		}

		if !types.IsExtensionOf(labelType, base) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "label type must extend the case expression's type",
				Range:    t.ctx.Source.Span(t.ctx.FileName, label.StartOffset, label.EndOffset),
			})

		}

		if name := labelType.String(); seen[name] != nil {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "duplicate CASE label type",
				Range:    t.ctx.Source.Span(t.ctx.FileName, label.Pos(), label.End()),
			})
		} else {
			seen[name] = label
		}
	}
}

func (t *TypeChecker) assertInteger(n ast.Node, msg string) {
	ty := t.TypeOf(n)
	if !types.IsInteger(ty) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  msg,
			Range:    t.ctx.Source.Span(t.ctx.FileName, n.Pos(), n.Pos()),
		})
	}
}

func (t *TypeChecker) TypeOf(n ast.Node) types.Type {
	switch e := n.(type) {
	case ast.Expression:
		return e.Type()
	case *ast.IdentifierDef:
		if e.Symbol != nil {
			return e.SemaType
		}

		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "unresolved identifier",
			Range:    t.ctx.Source.Span(t.ctx.FileName, e.Pos(), e.Pos()),
		})

		return types.UnknownType
	case *ast.Designator:
		return e.Type()
	case *ast.QualifiedIdent:
		return e.Type()
	default:
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "node has no type",
			Range:    t.ctx.Source.Span(t.ctx.FileName, e.Pos(), e.Pos()),
		})
		return types.UnknownType
	}
}

func (t *TypeChecker) EvalConstUint32(expr ast.Expression) uint32 {
	val := ast.EvalConstExpr(expr)
	switch v := val.(type) {
	case int:
		if v < 0 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "set value must be unsigned integer",
				Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Pos(), expr.End()),
			})
		}
		return uint32(v)
	case uint32:
		return v
	case uint64:
		if v > math.MaxUint32 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "set value exceeds uint32 max",
				Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Pos(), expr.End()),
			})
		}
		return uint32(v)
	case int64:
		if v < 0 || v > math.MaxUint32 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "set value outside of [0..uint32] range ",
				Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Pos(), expr.End()),
			})
		}
		return uint32(v)
	default:
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "expected constant unsigned integer",
			Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Pos(), expr.End()),
		})
		return 0 // unreachable
	}
}

func (t *TypeChecker) assertConst(expr ast.Expression) {
	if !ast.IsConstExpr(expr) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "expression must be constant",
			Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Pos(), expr.End()),
		})
	}
}
