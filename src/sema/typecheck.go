package sema

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
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

	ty := def.Symbol.AstType().Accept(t).(types.Type)

	def.SemaType = ty
	def.Symbol.SetType(ty)

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
			var rec *types.RecordType

			under := types.Underlying(typ)
			switch u := under.(type) {
			case *types.RecordType:
				rec = u
			case *types.PointerType:
				base, ok := types.Underlying(u.Base).(*types.RecordType)
				if !ok {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("cannot select field '%v' of pointer to non-record type '%v'", s.Field, dsg.QIdent),
						Range:    t.ctx.Source.Span(t.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
					})

					return dsg
				}

				rec = base
			default:
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("cannot select field '%v' of non-record '%v'", s.Field, dsg.QIdent),
					Range:    t.ctx.Source.Span(t.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
				})

				return dsg
			}

			field := rec.GetField(s.Field)
			if field == nil {
				//t.ctx.Reporter.Report(report.Diagnostic{
				//	Severity: report.Error,
				//	Message:  fmt.Sprintf("record '%v' has no field named '%v'", dsg.QIdent, s.Field),
				//	Range:    t.ctx.Source.Span(t.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
				//})

				return dsg
			}

			typ = field.Type
		case *ast.PtrDeref:
			switch ty := typ.(type) {
			case *types.PointerType:
				typ = ty.Base
			case *types.ProcedureType:
			default:
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("dereference/super operator not applicable to '%s'", dsg.QIdent),
					Range:    t.ctx.Source.Span(t.ctx.FileName, dsg.QIdent.StartOffset, s.EndOffset),
				})

				return dsg
			}
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

			ty := NamedTy.Symbol.AstType().Accept(t).(types.Type)
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

func (t *TypeChecker) checkPredeclaredFunction(call *ast.FunctionCall, pre *ast.ProcedureSymbol) {
	switch strings.ToLower(pre.Name()) {
	case "abs":
		var argType types.Type = types.UnknownType
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
		} else {
			call.ActualParams[0].Accept(t)
			argType = call.ActualParams[0].Type()
			if !types.IsNumeric(argType) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects a numeric argument, got %s",
						pre.Name(), argType),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
			}
		}
		call.SemaType = argType // ABS returns the same type as its argument
	case "cap":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()

		if !types.IsChar(argType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects a CHAR argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		call.SemaType = types.CharType
	case "bitand":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		for _, arg := range call.ActualParams {
			arg.Accept(t)
			argType := arg.Type()
			if argType != types.Int32Type && argType != types.Int64Type {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects INT32 or INT64 arguments, "+
						"got %s", pre.Name(), argType),
					Range: t.ctx.Source.Span(t.ctx.FileName, arg.Pos(), arg.End()),
				})
				return
			}
		}

		// BitAnd returns an Int32Type if both arguments are Int32Type, otherwise it returns Int64Type
		call.SemaType = types.Int32Type
		if call.ActualParams[0].Type() == types.Int64Type || call.ActualParams[1].Type() == types.Int64Type {
			call.SemaType = types.Int64Type
		}
	case "bitasr":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := arg1.Type()
		arg2Type := arg2.Type()

		if arg1Type != types.Int32Type && arg1Type != types.Int64Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be INT32 or INT64, "+
					"got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		if arg2Type != types.Int32Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be INT32, "+
					"got %s", pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
			return
		}

		// BitAst returns an Int32Type if the first argument is Int32Type, otherwise it returns Int64Type
		call.SemaType = types.Int32Type
		if arg1Type == types.Int64Type {
			call.SemaType = types.Int64Type
		}
	case "bitnot":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()
		if argType != types.Int32Type && argType != types.Int64Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects an INT32 or INT64 argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		// BitNot returns an Int32Type if the argument is Int32Type, otherwise it returns Int64Type
		call.SemaType = types.Int32Type
		if argType == types.Int64Type {
			call.SemaType = types.Int64Type
		}
	case "bitor":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		for _, arg := range call.ActualParams {
			arg.Accept(t)
			argType := arg.Type()
			if argType != types.Int32Type && argType != types.Int64Type {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects INT32 or INT64 arguments, "+
						"got %s", pre.Name(), argType),
					Range: t.ctx.Source.Span(t.ctx.FileName, arg.Pos(), arg.End()),
				})
				return
			}
		}

		// result is INT64 if x or y is INT64, else INT32
		call.SemaType = types.Int32Type
		if call.ActualParams[0].Type() == types.Int64Type || call.ActualParams[1].Type() == types.Int64Type {
			call.SemaType = types.Int64Type
		}
	case "bits":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()
		if argType != types.Int32Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects an INT32 argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		// Bits returns a set of bits, which is an Int32Type
		call.SemaType = types.SetType
	case "bitshl":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := arg1.Type()
		arg2Type := arg2.Type()

		if arg1Type != types.Int32Type && arg1Type != types.Int64Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be INT32 or INT64, "+
					"got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		if arg2Type != types.Int32Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be INT32, "+
					"got %s", pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
			return
		}

		// BitShl returns an Int32Type if the first argument is Int32Type, otherwise it returns Int64Type
		call.SemaType = types.Int32Type
		if arg1Type == types.Int64Type {
			call.SemaType = types.Int64Type
		}
	case "bitshr":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := arg1.Type()
		arg2Type := arg2.Type()
		if arg1Type != types.Int32Type && arg1Type != types.Int64Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be INT32 or INT64, "+
					"got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		if arg2Type != types.Int32Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be INT32, "+
					"got %s", pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
			return
		}

		// BitShr returns an Int32Type if the first argument is Int32Type, otherwise it returns Int64Type
		call.SemaType = types.Int32Type
		if arg1Type == types.Int64Type {
			call.SemaType = types.Int64Type
		}
	case "bitxor":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		for _, arg := range call.ActualParams {
			arg.Accept(t)
			argType := arg.Type()
			if argType != types.Int32Type && argType != types.Int64Type {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects INT32 or INT64 arguments, "+
						"got %s", pre.Name(), argType),
					Range: t.ctx.Source.Span(t.ctx.FileName, arg.Pos(), arg.End()),
				})
				return
			}
		}

		// BitXor returns an Int32Type if x or y is Int32Type, else Int64Type
		call.SemaType = types.Int32Type
		if call.ActualParams[0].Type() == types.Int64Type || call.ActualParams[1].Type() == types.Int64Type {
			call.SemaType = types.Int64Type
		}
	case "cast":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		T, ok := call.ActualParams[0].Accept(t).(types.Type)
		if !ok {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects a type as first argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		E, ok := call.ActualParams[1].(ast.Expression)
		if !ok {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects an expression as the second argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[1].Pos(), call.ActualParams[1].End()),
			})
			return
		}
		E.Accept(t)
		exprType := E.Type()

		T = types.Underlying(T)
		exprType = types.Underlying(exprType)

		if types.IsEnum(T) && types.IsOrdinal(exprType) || types.IsInteger(T) && types.IsInteger(exprType) {
			call.SemaType = T
			return
		}

		if !types.IsEnum(T) && !types.IsInteger(T) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects first argument to be integer or enum",
					pre.Name()),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		if !types.IsOrdinal(exprType) && !types.IsInteger(exprType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expected second argument to be"+
					" ordinal number or integer", pre.Name()),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})

			return
		}

		// TODO: T, x: cpointer to cstruct or void -> T
		// TODO: T: integer type, x: cpointer to void -> T
		// TODO: T: cpointer to void, x: integer type -> T
	case "chr":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()
		if !types.IsInteger(argType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects an integer "+
					"argument, got %s", pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		call.SemaType = types.CharType
	case "default":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		T, ok := call.ActualParams[0].Accept(t).(types.Type)
		if !ok {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects a type as argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		call.SemaType = T // Default returns the type of the argument
	case "floor":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}
		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()
		if !types.IsReal(argType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects a REAL or LONGREAL argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		call.SemaType = types.Int32Type
		if argType == types.LongRealType {
			call.SemaType = types.Int64Type
		}
	case "flt":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})

			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()

		if argType != types.Int32Type && argType != types.Int64Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects an INT32 or INT64 argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		call.SemaType = types.RealType
		if argType == types.Int64Type {
			call.SemaType = types.LongRealType
		}
	case "ldcmd":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		// both arguments are strings. the first is the module name and the second is the procedure name
		for _, arg := range call.ActualParams {
			arg.Accept(t)
			argType := arg.Type()
			if !types.IsCharArrayOrString(argType) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects string arguments, got %s",
						pre.Name(), argType),
					Range: t.ctx.Source.Span(t.ctx.FileName, arg.Pos(), arg.End()),
				})
				return
			}
		}

		// TODO: Implement dynamic module loading and procedure invocation. Needs triage
		call.SemaType = types.NilType
	case "ldmod":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()
		if !types.IsCharArrayOrString(argType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects a string argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		call.SemaType = types.BooleanType
	case "len":
		if len(call.ActualParams) == 0 || len(call.ActualParams) > 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects one or two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		if len(call.ActualParams) == 1 {
			call.ActualParams[0].Accept(t)
			T := call.ActualParams[0].Type()

			switch T.(type) {
			case *types.ArrayType:
				call.SemaType = types.Int32Type
			case *types.StringType:
				call.SemaType = types.Int32Type
			default:
				call.SemaType = types.UnknownType
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects an array or string argument, got %s",
						pre.Name(), T),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
			}
		}

		if len(call.ActualParams) == 2 {
			x, xOk := call.ActualParams[0].(ast.Expression)
			y, yOk := call.ActualParams[1].(ast.Expression)
			if !xOk || !yOk {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("predeclared procedure '%s' expects two expressions", pre.Name()),
					Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
				})
				return
			}

			x.Accept(t)
			y.Accept(t)
			tx := x.Type()
			ty := y.Type()

			if !types.IsArray(tx) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be an array got %s",
						pre.Name(), tx.String()),
					Range: t.ctx.Source.Span(t.ctx.FileName, x.Pos(), x.End()),
				})
				return
			}

			if ty != types.Int32Type {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be INT32, got %s",
						pre.Name(), ty.String()),
					Range: t.ctx.Source.Span(t.ctx.FileName, y.Pos(), y.End()),
				})
				return
			}

			dim := tx.(*types.ArrayType).Dimensions()
			value, err := t.EvalConstUint32(y)
			if err != nil {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("dimension value %s should be integer constant >= 0", y.String()),
					Range:    t.ctx.Source.Span(t.ctx.FileName, y.Pos(), y.End()),
				})
			}

			if int(value) >= dim {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("ARRAY has fewer than %d dimensions", value+1),
					Range:    t.ctx.Source.Span(t.ctx.FileName, y.Pos(), y.End()),
				})
				return
			}

			call.SemaType = types.Int32Type
		}
	case "long":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()

		if argType != types.Int8Type && argType != types.ByteType && argType != types.Int16Type &&
			argType != types.Int32Type && argType != types.RealType && argType != types.CharType {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects INT8, BYTE, INT16, INT32, "+
					"REAL, or CHAR argument, got %s", pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
		}

		switch argType {
		case types.Int8Type, types.ByteType:
			call.SemaType = types.Int16Type
		case types.Int16Type:
			call.SemaType = types.Int32Type
		case types.Int32Type:
			call.SemaType = types.Int64Type
		case types.RealType:
			call.SemaType = types.LongRealType
		case types.CharType:
			call.SemaType = types.WCharType // Long of a char is a wchar
		}
	case "max":
		if len(call.ActualParams) == 0 || len(call.ActualParams) > 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects one or two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		if len(call.ActualParams) == 1 {
			T, ok := call.ActualParams[0].Accept(t).(types.Type)
			if !ok {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("predeclared procedure '%s': single argument must be a type", pre.Name()),
					Range:    t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
				return
			}

			switch ty := types.Underlying(T).(type) {
			case *types.BasicType:
				if ty == types.SetType {
					call.SemaType = types.Int32Type
				} else {
					call.SemaType = ty
				}
			case *types.EnumType:
				call.SemaType = types.ByteType
			default:
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects a basic type or enum, got %s",
						pre.Name(), T),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
			}
		}

		if len(call.ActualParams) == 2 {
			x, xOk := call.ActualParams[0].(ast.Expression)
			y, yOk := call.ActualParams[1].(ast.Expression)
			if !xOk || !yOk {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("predeclared procedure '%s' expects two expressions", pre.Name()),
					Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
				})
				return
			}

			x.Accept(t)
			y.Accept(t)
			tx := x.Type()
			ty := y.Type()

			if types.IsNumeric(tx) && types.IsNumeric(ty) {
				call.SemaType = types.SmallestNumericType(tx, ty)
			} else if types.IsChar(tx) && types.IsChar(ty) {
				call.SemaType = types.SmallestCharType(tx, ty)
			} else {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects both arguments to be numeric"+
						" or char types, got %s and %s", pre.Name(), tx.String(), ty.String()),
					Range: t.ctx.Source.Span(t.ctx.FileName, x.Pos(), y.End()),
				})
				return
			}
		}
	case "min":
		if len(call.ActualParams) == 0 || len(call.ActualParams) > 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects one or two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		if len(call.ActualParams) == 1 {
			T, ok := call.ActualParams[0].Accept(t).(types.Type)
			if !ok {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("predeclared procedure '%s': single argument must be a type", pre.Name()),
					Range:    t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
				return
			}

			switch ty := types.Underlying(T).(type) {
			case *types.BasicType:
				if ty == types.SetType {
					call.SemaType = types.Int32Type
				} else {
					call.SemaType = ty
				}
			case *types.EnumType:
				call.SemaType = types.ByteType
			default:
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects a basic type or enum, got %s",
						pre.Name(), T),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
			}

		}

		if len(call.ActualParams) == 2 {
			x, xOk := call.ActualParams[0].(ast.Expression)
			y, yOk := call.ActualParams[1].(ast.Expression)
			if !xOk || !yOk {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("predeclared procedure '%s' expects two expressions", pre.Name()),
					Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
				})
				return
			}

			x.Accept(t)
			y.Accept(t)
			tx := x.Type()
			ty := y.Type()

			if types.IsNumeric(tx) && types.IsNumeric(ty) {
				call.SemaType = types.SmallestNumericType(tx, ty)
			} else if types.IsChar(tx) && types.IsChar(ty) {
				call.SemaType = types.SmallestCharType(tx, ty)
			} else {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects both arguments to be numeric"+
						" or char types, got %s and %s", pre.Name(), tx.String(), ty.String()),
					Range: t.ctx.Source.Span(t.ctx.FileName, x.Pos(), y.End()),
				})
				return
			}
		}
	case "odd":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}
		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()

		if !types.IsInteger(argType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects an integer argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		// Odd returns a boolean
		call.SemaType = types.BooleanType
	case "ord":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}
		call.ActualParams[0].Accept(t)
		argType := types.Underlying(call.ActualParams[0].Type())

		if types.IsEnum(argType) {
			call.SemaType = types.Int32Type
			return
		}

		switch argType {
		case types.CharType, types.BooleanType:
			call.SemaType = types.ByteType
		case types.WCharType:
			call.SemaType = types.ShortIntType
		case types.SetType:
			call.SemaType = types.Int32Type // Ordinal of a set is an integer
		default:
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' cannot be applied to type %s", pre.Name(), argType),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
		}
	case "short":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()
		if argType != types.Int16Type && argType != types.Int32Type && argType != types.Int64Type &&
			argType != types.LongRealType && argType != types.WCharType {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects an INT16, INT32, INT64, "+
					"LONGREAL, or WCHAR argument, got %s", pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
		}

		switch argType {
		case types.Int16Type:
			call.SemaType = types.Int8Type
		case types.Int32Type:
			call.SemaType = types.Int16Type
		case types.Int64Type:
			call.SemaType = types.Int32Type
		case types.LongRealType:
			call.SemaType = types.RealType
		case types.WCharType:
			call.SemaType = types.CharType
		}
	case "size":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		if T, ok := call.ActualParams[0].Accept(t).(types.Type); !ok {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects a type as the argument, got '%v'", pre.Name(), T),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		call.SemaType = types.Int32Type
	case "strlen":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()
		if !types.IsCharArrayOrString(argType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects "+
					"an array of CHAR, array of WCHAR, or string literal as argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		call.SemaType = types.Int32Type
	case "wchr":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()
		if !types.IsInteger(argType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects an integer type as argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		call.SemaType = types.WCharType
	case "ash":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := arg1.Type()
		arg2Type := arg2.Type()

		if arg1Type != types.Int32Type && arg1Type != types.Int64Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be INT32 or INT64, "+
					"got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		if arg2Type != types.Int32Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be INT32, got %s",
					pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
			return
		}

		// Ash returns an Int32Type if the first argument is Int32Type, else Int64Type
		call.SemaType = types.Int32Type
		if arg1Type == types.Int64Type {
			call.SemaType = types.Int64Type
		}
	case "asr":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := arg1.Type()
		arg2Type := arg2.Type()

		if arg1Type != types.Int32Type && arg1Type != types.Int64Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be INT32 or INT64, "+
					"got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		if arg2Type != types.Int32Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be INT32, got %s",
					pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
			return
		}

		// Asr returns an Int32Type if the first argument is Int32Type, else Int64Type
		call.SemaType = types.Int32Type
		if arg1Type == types.Int64Type {
			call.SemaType = types.Int64Type
		}
	case "entier":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := call.ActualParams[0].Type()
		if !types.IsReal(argType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects a real type as argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		call.SemaType = types.Int64Type
	case "lsl":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := arg1.Type()
		arg2Type := arg2.Type()
		if arg1Type != types.Int32Type && arg1Type != types.Int64Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be INT32 or INT64, "+
					"got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		if arg2Type != types.Int32Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be INT32, got %s",
					pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
			return
		}

		// Lsl returns an Int32Type if the first argument is Int32Type, else Int64Type
		call.SemaType = types.Int32Type
		if arg1Type == types.Int64Type {
			call.SemaType = types.Int64Type
		}
	case "ror":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := arg1.Type()
		arg2Type := arg2.Type()

		if arg1Type != types.Int32Type || arg2Type != types.Int32Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects both arguments to be INT32, got %s and %s",
					pre.Name(), arg1Type, arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg2.End()),
			})
			return
		}

		call.SemaType = types.Int32Type
	default:
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("unknown predeclared procedure '%s'", pre.Name()),
			Range:    t.ctx.Source.Span(t.ctx.FileName, call.Callee.Pos(), call.Callee.End()),
		})
	}
}

func (t *TypeChecker) checkPredeclaredProcedure(call *ast.ProcedureCall, pre *ast.ProcedureSymbol) {
	switch strings.ToLower(pre.Name()) {
	case "assert":
		if len(call.ActualParams) == 0 || len(call.ActualParams) > 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects one or two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		if len(call.ActualParams) == 1 {
			call.ActualParams[0].Accept(t)
			T := types.Underlying(call.ActualParams[0].Type())

			if !types.IsBoolean(T) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects a boolean type, got %s",
						pre.Name(), T),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
			}

			return
		}

		if len(call.ActualParams) == 2 {
			x, xOk := call.ActualParams[0].(ast.Expression)
			y, yOk := call.ActualParams[1].(ast.Expression)
			if !xOk || !yOk {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("predeclared procedure '%s' expects two expressions", pre.Name()),
					Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
				})
				return
			}

			x.Accept(t)
			y.Accept(t)
			tx := types.Underlying(x.Type())
			ty := types.Underlying(y.Type())

			if !types.IsBoolean(tx) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be boolean, got %s",
						pre.Name(), tx),
					Range: t.ctx.Source.Span(t.ctx.FileName, x.Pos(), x.End()),
				})
			}

			if !types.IsInteger(ty) || !ast.IsConstExpr(y) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be an integer constant, got %s",
						pre.Name(), ty),
					Range: t.ctx.Source.Span(t.ctx.FileName, y.Pos(), y.End()),
				})
			}

			return
		}
	case "bytes":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := types.Underlying(arg1.Type())
		arg2Type := types.Underlying(arg2.Type())

		if !types.IsArrayOf(arg1Type, types.ByteType) && !types.IsArrayOf(arg1Type, types.CharType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be an array of byte or char, "+
					"got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		if !types.IsNumeric(arg2Type) && !types.IsSet(arg2Type) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be numeric or set, got %s",
					pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
			return
		}
	case "dec":
		if len(call.ActualParams) == 0 || len(call.ActualParams) > 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects one or two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		if len(call.ActualParams) == 1 {
			call.ActualParams[0].Accept(t)
			T := types.Underlying(call.ActualParams[0].Type())

			if !types.IsInteger(T) && !types.IsEnum(T) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects first argument to be an integer or enum type, got %s",
						pre.Name(), T),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
				return
			}

			return
		}

		if len(call.ActualParams) == 2 {
			call.ActualParams[0].Accept(t)
			call.ActualParams[1].Accept(t)
			x, xOk := call.ActualParams[0].(ast.Expression)
			y, yOk := call.ActualParams[1].(ast.Expression)
			if !xOk || !yOk {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("predeclared procedure '%s' expects two expressions", pre.Name()),
					Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
				})
				return
			}

			tx := types.Underlying(x.Type())
			ty := types.Underlying(y.Type())

			if !types.IsInteger(tx) || !t.isAssignable(x) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be an assignable integer, got %s",
						pre.Name(), tx.String()),
					Range: t.ctx.Source.Span(t.ctx.FileName, x.Pos(), x.End()),
				})
				return
			}

			if !types.IsInteger(ty) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be an integer, got %s",
						pre.Name(), ty.String()),
					Range: t.ctx.Source.Span(t.ctx.FileName, y.Pos(), y.End()),
				})
				return
			}

			return
		}
	case "excl":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := types.Underlying(arg1.Type())
		arg2Type := types.Underlying(arg2.Type())

		if !types.IsSet(arg1Type) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be a set, got %s",
					pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		value, err := t.EvalConstUint32(arg2)
		if !types.IsInteger(arg2Type) || err != nil || (value < 0 || value > math.MaxUint32) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be an integer in the range [0, MAX(SET)], got %s",
					pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
		}

		return
	case "halt":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg := call.ActualParams[0]
		arg.Accept(t)
		argType := types.Underlying(arg.Type())

		if !types.IsInteger(argType) || !ast.IsConstExpr(arg) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects an integer constant argument, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg.Pos(), arg.End()),
			})
			return
		}

		return
	case "inc":
		if len(call.ActualParams) == 0 || len(call.ActualParams) > 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects one or two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		if len(call.ActualParams) == 1 {
			call.ActualParams[0].Accept(t)
			T := types.Underlying(call.ActualParams[0].Type())

			if !types.IsInteger(T) && !types.IsEnum(T) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects an integer or enum type, got %s",
						pre.Name(), T),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
				return
			}

			return
		}

		if len(call.ActualParams) == 2 {
			call.ActualParams[0].Accept(t)
			call.ActualParams[1].Accept(t)
			x, xOk := call.ActualParams[0].(ast.Expression)
			y, yOk := call.ActualParams[1].(ast.Expression)
			if !xOk || !yOk {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("predeclared procedure '%s' expects two expressions", pre.Name()),
					Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
				})
				return
			}

			tx := types.Underlying(x.Type())
			ty := types.Underlying(y.Type())

			if !types.IsInteger(tx) || !t.isAssignable(x) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be an assignable integer, got %s",
						pre.Name(), tx.String()),
					Range: t.ctx.Source.Span(t.ctx.FileName, x.Pos(), x.End()),
				})
			}

			if !types.IsInteger(ty) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be an integer, got %s",
						pre.Name(), ty.String()),
					Range: t.ctx.Source.Span(t.ctx.FileName, y.Pos(), y.End()),
				})
			}

			return
		}
	case "incl":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := types.Underlying(arg1.Type())
		arg2Type := types.Underlying(arg2.Type())

		if !types.IsSet(arg1Type) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be a set, got %s",
					pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		value, err := t.EvalConstUint32(arg2)
		if !types.IsInteger(arg2Type) || err != nil || (value < 0 || value > math.MaxUint32) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be an integer in the range [0, MAX(SET)], got %s",
					pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
		}

		return
	case "new":
		if len(call.ActualParams) == 0 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects at least one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		if len(call.ActualParams) == 1 {
			call.ActualParams[0].Accept(t)
			argType := types.Underlying(call.ActualParams[0].Type())
			if !types.IsPointerToRecord(argType) && !types.IsFixedArray(argType) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects a pointer to a record or a fixed array, got %s",
						pre.Name(), argType),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
				return
			}

			return
		} else {
			call.ActualParams[0].Accept(t)
			argType := types.Underlying(call.ActualParams[0].Type())
			if !types.IsPointerToOpenArray(argType) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects a pointer to an open array, got %s",
						pre.Name(), argType),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
				})
				return
			}

			dim := types.Underlying(argType.(*types.PointerType).Base).(*types.ArrayType).Dimensions()
			if dim != len(call.ActualParams[1:]) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("'%s': expected %d dimension lengths for open array type, got %d",
						pre.Name(), dim, len(call.ActualParams)-1),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
				})
				return
			}

			for _, arg := range call.ActualParams[1:] {
				arg.Accept(t)
				argType = types.Underlying(arg.Type())
				if !types.IsInteger(argType) {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message: fmt.Sprintf("'%s': dimension length must be integer, got %s",
							pre.Name(), argType),
						Range: t.ctx.Source.Span(t.ctx.FileName, arg.Pos(), arg.End()),
					})
					return
				}
			}

			return
		}
	case "number":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		call.ActualParams[1].Accept(t)
		arg1Type := types.Underlying(call.ActualParams[0].Type())
		arg2Type := types.Underlying(call.ActualParams[1].Type())

		if !types.IsNumeric(arg1Type) && !types.IsSet(arg1Type) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be numeric or set, got %s",
					pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		if !t.isAssignable(call.ActualParams[0]) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be an "+
					"assignable numeric or set type, got %s",
					pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		if !types.IsArrayOf(arg2Type, types.ByteType) && !types.IsArrayOf(arg2Type, types.CharType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be an array of byte or char, "+
					"got %s", pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[1].Pos(), call.ActualParams[1].End()),
			})
			return
		}

		return
	case "pcall":
		if len(call.ActualParams) < 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects at least two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		call.ActualParams[1].Accept(t)
		arg1Type := types.Underlying(call.ActualParams[0].Type())
		arg2Type := types.Underlying(call.ActualParams[1].Type())

		if !types.IsPointerToAnyRec(arg1Type) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be a pointer to ANYREC, "+
					"got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}

		if !types.IsProperProcedure(arg2Type) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be a proper procedure, "+
					"got %s", pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[1].Pos(), call.ActualParams[1].End()),
			})
			return
		}

		if len(call.ActualParams) > 2 {
			proc := types.Underlying(call.ActualParams[1].Type()).(*types.ProcedureType)
			if len(call.ActualParams)-2 != len(proc.Params) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("predeclared procedure '%s' expects %d additional arguments, got %d",
						pre.Name(), len(proc.Params), len(call.ActualParams)-2),
					Range: t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
				})
				return
			}

			for i, arg := range call.ActualParams[2:] {
				arg.Accept(t)
				if !types.ParameterCompatible(types.Underlying(arg.Type()), proc.Params[i]) {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("%s: argument %d is incompatible with parameter of type '%s'", pre.Name(), i+1, proc.Params[i].Type.String()),
						Range:    t.ctx.Source.Span(t.ctx.FileName, arg.Pos(), arg.End()),
					})
					return
				}
			}
		}

		return
	case "raise":
		if len(call.ActualParams) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly one argument", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		call.ActualParams[0].Accept(t)
		argType := types.Underlying(call.ActualParams[0].Type())
		if !types.IsPointerToAnyRec(argType) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects a pointer to ANYREC, got %s",
					pre.Name(), argType),
				Range: t.ctx.Source.Span(t.ctx.FileName, call.ActualParams[0].Pos(), call.ActualParams[0].End()),
			})
			return
		}
	case "copy":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := types.Underlying(arg1.Type())
		arg2Type := types.Underlying(arg2.Type())

		if !types.IsCharArrayOrString(arg1Type) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be a char array or string, "+
					"got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		if !types.IsArrayOf(arg2Type, types.CharType) || !t.isAssignable(arg2) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be an assignable array of char, "+
					"got %s", pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
			return
		}
	case "pack":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := types.Underlying(arg1.Type())
		arg2Type := types.Underlying(arg2.Type())

		if !types.IsReal(arg1Type) || !t.isAssignable(arg1) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be an assignable REAL/LONGREAL, "+
					" got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg1.End()),
			})
			return
		}

		if arg2Type != types.Int32Type {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be INT32, got %s",
					pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
			return
		}
	case "unpk":
		if len(call.ActualParams) != 2 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("predeclared procedure '%s' expects exactly two arguments", pre.Name()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, call.StartOffset, call.EndOffset),
			})
			return
		}

		arg1 := call.ActualParams[0]
		arg2 := call.ActualParams[1]
		arg1.Accept(t)
		arg2.Accept(t)
		arg1Type := types.Underlying(arg1.Type())
		arg2Type := types.Underlying(arg2.Type())

		if !types.IsReal(arg1Type) || !t.isAssignable(arg1) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the first argument to be an "+
					"assignable REAL/LONGREAL, got %s", pre.Name(), arg1Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg1.Pos(), arg2.End()),
			})
			return
		}

		if arg2Type != types.Int32Type || !t.isAssignable(arg2) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("predeclared procedure '%s' expects the second argument to be an assignable INT32, got %s",
					pre.Name(), arg2Type),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg2.Pos(), arg2.End()),
			})
			return
		}
	default:
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("unknown predeclared procedure '%s'", pre.Name()),
			Range:    t.ctx.Source.Span(t.ctx.FileName, call.Callee.Pos(), call.Callee.End()),
		})
	}

}

func (t *TypeChecker) VisitFunctionCall(call *ast.FunctionCall) any {
	call.Callee.Accept(t)

	if !IsCallable(call.Callee) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("name '%s' could not be resolved to a procedure", call.Callee),
			Range:    t.ctx.Source.Span(t.ctx.FileName, call.Callee.StartOffset, call.Callee.EndOffset),
		})

		return call
	}

	// Handle predeclared function procedure calls
	if call.Callee.Symbol.Props() == ast.Predeclared {
		t.checkPredeclaredFunction(call, call.Callee.Symbol.(*ast.ProcedureSymbol))
		return call
	}

	procType, ok := types.Underlying(call.Callee.SemaType).(*types.ProcedureType)
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
				Range: t.ctx.Source.Span(t.ctx.FileName, arg.Pos(), arg.End()),
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
				if e.Kind == token.REAL_LIT || e.Kind == token.LONGREAL_LIT {
					value, err := strconv.ParseFloat(e.Val, 64)
					if err != nil {
						t.ctx.Reporter.Report(report.Diagnostic{
							Severity: report.Fatal,
							Message:  fmt.Sprintf("could not parse '%s' to real", e.Val),
							Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Operand.Pos(), expr.Operand.End()),
						})
					}

					if op == token.MINUS {
						value = -value
					}

					// Determine the type based on the value
					if value >= -3.402823466e+38 && value <= 3.402823466e+38 {
						expr.SemaType = types.RealType
					} else {
						expr.SemaType = types.LongRealType
					}
				} else if e.Kind != token.CHAR && e.Kind != token.WCHAR && e.Kind != token.SET && e.Kind != token.BOOLEAN {
					value, err := strconv.ParseInt(e.Val, 10, 64)
					if err != nil {
						t.ctx.Reporter.Report(report.Diagnostic{
							Severity: report.Fatal,
							Message:  fmt.Sprintf("could not parse '%s' to int", e.Val),
							Range:    t.ctx.Source.Span(t.ctx.FileName, expr.Operand.Pos(), expr.Operand.End()),
						})
					}

					if op == token.MINUS {
						value = -value
					}

					switch e.Kind {
					case token.BYTE_LIT:
						if value >= 0 && value <= 255 {
							expr.SemaType = tx
						} else {
							expr.SemaType = smallestTypeFor(value)
						}
					case token.INT8_LIT:
						if value >= -128 && value <= 127 {
							expr.SemaType = tx
						} else {
							expr.SemaType = smallestTypeFor(value)
						}
					case token.INT16_LIT, token.SHORTINT:
						if value >= -32768 && value <= 32767 {
							expr.SemaType = tx
						} else {
							expr.SemaType = smallestTypeFor(value)
						}
					case token.INT32_LIT, token.INTEGER:
						if value >= -2147483648 && value <= 2147483647 {
							expr.SemaType = tx
						} else {
							expr.SemaType = smallestTypeFor(value)
						}
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
	if ty, ok := t.ctx.TypeOverrides[ident.Name]; ok {
		ident.SemaType = ty
		ident.Symbol.SetType(ty)
		return ident
	}

	if ident.Prefix == "" {
		if ident.Symbol == nil {
			ident.SemaType = types.UnknownType
		} else {
			ident.SemaType = ident.Symbol.Type()
		}

		return ident
	}

	env := t.ctx.Env.ModuleScope(ident.Prefix)
	sym := env.Lookup(ident.Name)
	if sym == nil {
		ident.SemaType = types.UnknownType
	} else {
		ident.SemaType = ident.Symbol.Type()
	}

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

			low, errLow := t.EvalConstUint32(e.Low)
			high, errHigh := t.EvalConstUint32(e.High)
			if errLow != nil || errHigh != nil {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("invalid set range: %v", errLow),
					Range:    t.ctx.Source.Span(t.ctx.FileName, elem.Pos(), elem.End()),
				})
				return n
			}

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

			val, err := t.EvalConstUint32(e)
			if err != nil {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("invalid set element: %v", err),
					Range:    t.ctx.Source.Span(t.ctx.FileName, elem.Pos(), elem.End()),
				})
				return n
			}
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
	case token.WCHAR_LIT:
		lit.SemaType = types.WCharType
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

	tv := types.Underlying(stmt.LValue.Type()) // variable type
	te := types.Underlying(stmt.RValue.Type()) // expression type

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
			Message: fmt.Sprintf("expression '%s' is not assignment compatible with variable '%s'",
				stmt.RValue, stmt.LValue),
			Range: t.ctx.Source.Span(t.ctx.FileName, stmt.StartOffset, stmt.EndOffset),
		})
	}

	return stmt
}

func (t *TypeChecker) VisitReturnStmt(stmt *ast.ReturnStmt) any {
	if stmt.Value != nil {
		stmt.Value.Accept(t)
	}

	return stmt
}

func (t *TypeChecker) VisitProcedureCall(call *ast.ProcedureCall) any {
	call.Callee.Accept(t)

	if !IsCallable(call.Callee) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("name '%s' could not be resolved to a procedure", call.Callee),
			Range:    t.ctx.Source.Span(t.ctx.FileName, call.Callee.StartOffset, call.Callee.EndOffset),
		})

		return call
	}

	// Handle predeclared procedure calls
	if call.Callee.Symbol.Props() == ast.Predeclared {
		t.checkPredeclaredProcedure(call, call.Callee.Symbol.(*ast.ProcedureSymbol))
		return call
	}

	procType, ok := types.Underlying(call.Callee.SemaType).(*types.ProcedureType)
	if !ok {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("cannot call '%v', a non-procedure object", call.Callee),
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
				Message: fmt.Sprintf("argument %d: of type, '%s' is incompatible with parameter of type '%s'",
					i+1, actual.String(), formal.Type.String()),
				Range: t.ctx.Source.Span(t.ctx.FileName, arg.Pos(), arg.End()),
			})
		}
	}

	return call
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

	if !IsValidCaseDiscriminant(stmt.Expr) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("invalid type '%s' for CASE selector", caseType),
			Range:    t.ctx.Source.Span(t.ctx.FileName, stmt.Expr.Pos(), stmt.Expr.End()),
		})

		return stmt
	}

	if types.IsInteger(types.Underlying(caseType)) {
		t.checkIntegerCase(stmt, caseType)
	} else if types.IsChar(types.Underlying(caseType)) {
		t.checkCharCase(stmt)
	} else if types.IsEnum(types.Underlying(caseType)) {
		t.checkEnumCase(stmt, caseType)
	} else {
		t.checkTypeCase(stmt, caseType)
	}

	for _, statement := range stmt.Else {
		statement.Accept(t)
	}

	return stmt
}

func (t *TypeChecker) VisitCase(c *ast.Case) any { return c }

func (t *TypeChecker) VisitLabelRange(labelRange *ast.LabelRange) any {
	labelRange.Low.Accept(t)
	labelRange.SemaType = labelRange.Low.Type()

	if labelRange.High != nil {
		labelRange.High.Accept(t)

		if !types.SameType(labelRange.Low.Type(), labelRange.High.Type()) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("label range boundaries must have the same type, got '%v' and '%v'",
					labelRange.Low.Type(), labelRange.High.Type()),
				Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.Pos(), labelRange.End()),
			})

			labelRange.SemaType = types.UnknownType
			return labelRange
		}
	}

	return labelRange
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
	for _, arm := range stmt.Arms {
		arm.Accept(t)
	}

	for _, statement := range stmt.Else {
		statement.Accept(t)
	}

	return stmt
}

func (t *TypeChecker) VisitGuard(guard *ast.Guard) any {
	guard.Expr.Accept(t)
	guard.Type.Accept(t)

	if !IsValidGuardExpr(guard.Expr) {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("'%s' is not a valid guard expression", guard.Expr),
			Range:    t.ctx.Source.Span(t.ctx.FileName, guard.Expr.Pos(), guard.Expr.End()),
		})
	}

	extOf := types.IsExtensionOf(types.Underlying(guard.Type.Type()), types.Underlying(guard.Expr.Type()))
	ptrExtOf := types.IsPointerExtensionOf(types.Underlying(guard.Type.Type()), types.Underlying(guard.Expr.Type()))
	if !extOf && !ptrExtOf {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("'%v' does not extend '%v'", guard.Type, guard.Expr.Type()),
			Range:    t.ctx.Source.Span(t.ctx.FileName, guard.Type.Pos(), guard.Type.End()),
		})
	}

	t.ctx.TypeOverrides[guard.Expr.String()] = guard.Type.Type()

	for _, statement := range guard.StmtSeq {
		statement.Accept(t)
	}

	delete(t.ctx.TypeOverrides, guard.Expr.String())

	return guard
}

func (t *TypeChecker) VisitImport(i *ast.Import) any { return i }

func (t *TypeChecker) VisitProcedureDecl(decl *ast.ProcedureDecl) any {
	tmp := t.ctx.Env.CurrentScope()
	t.ctx.Env.SetCurrentScope(decl.Env)
	defer func() { t.ctx.Env.SetCurrentScope(tmp) }()

	decl.Head.Accept(t)
	if decl.Body != nil {
		decl.Body.Accept(t)
	}

	// Type-bound procedure redefinition validation
	t.checkTypeBoundRedefinition(decl)

	return decl
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
	decl.Name.Symbol.SetType(ty)

	return decl
}

func (t *TypeChecker) VisitProcedureHeading(heading *ast.ProcedureHeading) any {
	if heading.Rcv != nil {
		heading.Rcv.Accept(t)
	}

	if heading.Name.Symbol == nil {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "unresolved identifier: " + heading.Name.Name,
			Range:    t.ctx.Source.Span(t.ctx.FileName, heading.Name.StartOffset, heading.Name.EndOffset),
		})

		return nil
	}

	if heading.FP == nil {
		ty := heading.Name.Symbol.AstType().Accept(t).(types.Type)
		heading.Name.SemaType = ty
		heading.Name.Symbol.SetType(ty)
	} else {
		procType := heading.FP.Accept(t).(*types.ProcedureType)
		heading.Name.SemaType = procType
		heading.Name.Symbol.SetType(procType)
	}

	return heading
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

func (t *TypeChecker) VisitReceiver(rcv *ast.Receiver) any {
	if rcv.Name.Symbol == nil {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "unable to resolve receiver " + rcv.Name.Name,
			Range:    t.ctx.Source.Span(t.ctx.FileName, rcv.StartOffset, rcv.EndOffset),
		})
	}

	if rcv.Name.Symbol.Kind() != ast.ParamSymbolKind {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("receiver variable '%s' must be a parameter", rcv.Name.Name),
			Range:    t.ctx.Source.Span(t.ctx.FileName, rcv.StartOffset, rcv.EndOffset),
		})
	}

	ty, ok := rcv.Type.Accept(t).(*types.NamedType)
	if !ok {
		t.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "invalid type for receiver. Must be Named type",
			Range:    t.ctx.Source.Span(t.ctx.FileName, rcv.Type.Pos(), rcv.Type.End()),
		})
	}

	switch rcv.Kind {
	case token.VAR, token.IN:
		// The receiver may be either a variable (VAR or IN) parameter of record type T
		rec, ok := types.Underlying(ty.Def).(*types.RecordType)
		if !ok {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("VAR/IN receiver must have a record type, got '%v'", rec.String()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, rcv.Type.Pos(), rcv.Type.End()),
			})
		}
	default:
		// value parameter of type POINTER TO T (where T is a record type
		ptr, ok := types.Underlying(ty.Def).(*types.PointerType)
		if !ok || types.Underlying(ptr.Base).(*types.RecordType) == nil {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("value receiver must have a pointer to record type, got '%v'", ptr.String()),
				Range:    t.ctx.Source.Span(t.ctx.FileName, rcv.Type.Pos(), rcv.Type.End()),
			})
		}
	}

	rcv.Name.SemaType = ty
	rcv.Name.Symbol.SetType(ty)

	return rcv
}

func (t *TypeChecker) VisitFPSection(section *ast.FPSection) any {
	typ := section.Type.Accept(t).(types.Type)

	var kind string
	switch section.Kind {
	case token.VAR:
		kind = "VAR"
	case token.IN:
		kind = "IN"
	default:
		kind = "VALUE"
	}

	var params []*types.FormalParam
	for _, id := range section.Names {
		if id.Name == "_" {
			params = append(params, &types.FormalParam{
				Kind: kind,
				Name: "_",
				Type: typ,
			})
			continue
		}

		sym := t.ctx.Env.Lookup(id.Name)
		if sym == nil {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("undeclared parameter name '%s'", id.Name),
				Range:    t.ctx.Source.Span(t.ctx.FileName, id.Pos(), id.End()),
			})

			continue
		}

		sym.SetType(typ)
		id.Symbol = sym
		id.SemaType = typ

		params = append(params, &types.FormalParam{
			Kind: kind,
			Name: id.Name,
			Type: typ,
		})
	}

	return params
}

func (t *TypeChecker) VisitFormalParams(params *ast.FormalParams) any {
	procType := &types.ProcedureType{}

	var p []*types.FormalParam
	for _, sec := range params.Params {
		p = t.VisitFPSection(sec).([]*types.FormalParam)
		//p = append(p, fp)
	}
	procType.Params = p

	if params.RetType != nil {
		procType.Result = params.RetType.Accept(t).(types.Type)
	}

	return procType
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
	var proc *types.ProcedureType
	if ty.FP != nil {
		proc = ty.FP.Accept(t).(*types.ProcedureType)
	}

	return proc
}

func (t *TypeChecker) VisitRecordType(ty *ast.RecordType) any {
	recTy := &types.RecordType{Fields: make(map[string]*types.Field)}
	if ty.Base != nil {
		baseTy := types.Underlying(ty.Base.Accept(t).(types.Type)).(*types.RecordType)
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
		for _, ff := range f {
			recTy.Fields[ff.Name] = &ff
		}
	}

	for name, sym := range ty.Env.Elems() {
		if recTy.Fields[name] != nil {
			continue
		}

		recTy.Fields[name] = &types.Field{
			Name:       name,
			Type:       sym.AstType().Accept(t).(types.Type),
			IsExported: sym.Props() == ast.Exported,
		}
	}

	return recTy
}

func (t *TypeChecker) VisitFieldList(list *ast.FieldList) any {
	typ := list.Type.Accept(t).(types.Type)

	fields := make([]types.Field, 0)
	for _, def := range list.List {
		sym := list.Env.Lookup(def.Name)
		if sym == nil || sym.Kind() != ast.FieldSymbolKind {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("could not resolve field '%s'.", def.Name),
				Range:    t.ctx.Source.Span(t.ctx.FileName, def.StartOffset, def.EndOffset),
			})
			continue
		}

		fields = append(fields, types.Field{
			Name:       def.Name,
			Type:       typ,
			IsExported: sym.Props() == ast.Exported,
		})
	}

	return fields
}

func (t *TypeChecker) VisitEnumType(ty *ast.EnumType) any {
	names := map[string]int{}

	for i, variant := range ty.Variants {
		if _, exists := names[variant.Name]; exists {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("enum variant '%s' is defined more than once", variant),
				Range:    t.ctx.Source.Span(t.ctx.FileName, ty.StartOffset, ty.EndOffset),
			})
		} else {
			names[variant.Name] = i
		}
	}

	enum := &types.EnumType{Variants: names}

	for _, variant := range ty.Variants {
		if variant.Symbol == nil {
			variant.SemaType = types.UnknownType
			variant.Symbol.SetType(types.UnknownType)
		} else {
			variant.SemaType = enum
			variant.Symbol.SetType(enum)
		}
	}

	return enum
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
		return sym.Mod == token.VAR || sym.Mod == token.ILLEGAL
	default:
		return false
	}
}

func (t *TypeChecker) checkIntegerCase(stmt *ast.CaseStmt, caseType types.Type) {
	seen := map[interface{}]ast.Node{}

	for _, c := range stmt.Cases {
		for _, labelRange := range c.CaseLabelList {
			labelRange.Accept(t)

			if !types.TypeIncludes(caseType, labelRange.Low.Type()) /*&& !types.EqualType(caseType, labelRange.Low.Type()) */ {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("CASE label '%s' is invalid for the range of CASE expression ('%s': '%s') ",
						labelRange, stmt.Expr, caseType),
					Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.Pos(), labelRange.End()),
				})
			}

			// Case labels are constants, and no value must occur more than once
			if !ast.IsConstExpr(labelRange.Low) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "CASE label must be a constant",
					Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.Low.Pos(), labelRange.Low.End()),
				})
			}

			lowValue, isInt64 := ast.EvalConstExpr(labelRange.Low).(int64)
			if !isInt64 {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("'%v' is not a valid INTEGER", labelRange.Low),
					Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.Low.Pos(), labelRange.Low.End()),
				})

				continue
			}
			highValue := lowValue

			if labelRange.High != nil {
				// Case labels are constants, and no value must occur more than once
				if !ast.IsConstExpr(labelRange.High) {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  "CASE label must be a constant",
						Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.High.Pos(), labelRange.High.End()),
					})
				}

				if !types.TypeIncludes(caseType, labelRange.High.Type()) /*&& !types.EqualType(caseType, labelRange.Low.Type())*/ {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message: fmt.Sprintf("CASE label '%s' is invalid for the range of CASE expression ('%s': '%s') ",
							labelRange, stmt.Expr, caseType),
						Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.Pos(), labelRange.End()),
					})
				}

				if !ast.IsConstExpr(labelRange.High) {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  "high end of CASE label must be a constant",
						Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.High.Pos(), labelRange.High.End()),
					})
				}

				hv, isInt64 := ast.EvalConstExpr(labelRange.High).(int64)
				if !isInt64 {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("'%v' is not a valid INTEGER", labelRange.High),
						Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.High.Pos(), labelRange.High.End()),
					})

					continue
				}

				if hv < lowValue {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("invalid case label range: %d .. %d", lowValue, hv),
						Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.Pos(), labelRange.End()),
					})

					continue
				}

				highValue = hv
			}

			// Check each value in range
			for val := lowValue; val <= highValue; val++ {
				if prev, exists := seen[val]; exists {
					pos := t.ctx.Source.Span(t.ctx.FileName, prev.Pos(), prev.End()).Start
					location := fmt.Sprintf("%s:%d:%d", pos.File, pos.Line, pos.Column)

					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message: fmt.Sprintf("duplicate case label %d (already defined at %v)",
							val, location),
						Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.Pos(), labelRange.End()),
					})
				} else {
					seen[val] = labelRange.Low
				}
			}
		}

		for _, statement := range c.StmtSeq {
			statement.Accept(t)
		}
	}
}

func (t *TypeChecker) checkCharCase(stmt *ast.CaseStmt) {
	seen := map[interface{}]ast.Node{}
	for _, c := range stmt.Cases {
		for _, labelRange := range c.CaseLabelList {
			labelRange.Accept(t)

			// Case labels are constants, and no value must occur more than once
			if !ast.IsConstExpr(labelRange.Low) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "CASE label must be a constant",
					Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.Low.Pos(), labelRange.Low.End()),
				})
			}

			if !types.IsChar(labelRange.Low.Type()) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("CASE label must be CHAR type, got '%v'", labelRange.Low.Type()),
					Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.Low.Pos(), labelRange.Low.End()),
				})
			}

			lowRune, ok := ast.EvalConstExpr(labelRange.Low).(rune)
			if !ok {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("'%v' is not a valid LATIN-1 CHAR", labelRange.Low),
					Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.Low.Pos(), labelRange.Low.End()),
				})

				continue
			}
			highRune := lowRune

			if labelRange.High != nil {
				if !ast.IsConstExpr(labelRange.High) {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  "CASE label must be a constant",
						Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.High.Pos(), labelRange.High.End()),
					})
				}

				if !types.IsChar(labelRange.High.Type()) {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("CASE label must be CHAR type, got '%v'", labelRange.High.Type()),
						Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.High.Pos(), labelRange.High.End()),
					})
				}

				// Case labels are constants, and no value must occur more than once
				if !ast.IsConstExpr(labelRange.High) {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  "CASE label must be a constant",
						Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.High.Pos(), labelRange.High.End()),
					})

					continue
				}

				hr, ok := ast.EvalConstExpr(labelRange.High).(rune)
				if !ok {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("'%v' is not a valid LATIN-1 CHAR", labelRange.High),
						Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.High.Pos(), labelRange.High.End()),
					})

					continue
				}

				if hr < lowRune {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message: fmt.Sprintf("'%s' cannot be greater then '%s'. Invalid case label range: "+
							"%s .. %s.", string(lowRune), string(hr), string(lowRune), string(hr)),
						Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.Pos(), labelRange.End()),
					})

					continue
				}
				highRune = hr
			}

			// Check each value in range
			for val := lowRune; val <= highRune; val++ {
				if prev, exists := seen[val]; exists {
					pos := t.ctx.Source.Span(t.ctx.FileName, prev.Pos(), prev.End()).Start
					location := fmt.Sprintf("%s:%d:%d", pos.File, pos.Line, pos.Column)

					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message: fmt.Sprintf("duplicate case label '%s' (already defined at %s)",
							string(val), location),
						Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.Pos(), labelRange.End()),
					})
				} else {
					seen[val] = labelRange.Low
				}
			}

		}

		for _, statement := range c.StmtSeq {
			statement.Accept(t)
		}
	}
}

func (t *TypeChecker) checkEnumCase(stmt *ast.CaseStmt, caseType types.Type) {
	seen := map[interface{}]ast.Node{}
	for _, c := range stmt.Cases {
		for _, labelRange := range c.CaseLabelList {
			labelRange.Accept(t)

			// Case labels are constants, and no value must occur more than once
			if !ast.IsConstExpr(labelRange.Low) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "CASE label must be a constant",
					Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.Low.Pos(), labelRange.Low.End()),
				})
			}

			if !types.IsEnum(labelRange.Low.Type()) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("CASE label must be of ENUM type, '%v' got '%v'",
						caseType, labelRange.Low.Type()),
					Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.Low.Pos(), labelRange.Low.End()),
				})
			}

			enum := types.Underlying(caseType).(*types.EnumType)
			lowEnum, isEnumMember := enum.Variants[labelRange.Low.String()]
			if !isEnumMember {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message: fmt.Sprintf("CASE label, '%s' is not a member of ENUM '%s'",
						labelRange.Low.String(), caseType),
					Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.Low.Pos(), labelRange.Low.End()),
				})

				continue
			}
			highEnum := lowEnum

			if labelRange.High != nil {
				// Case labels are constants, and no value must occur more than once
				if !ast.IsConstExpr(labelRange.High) {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  "CASE label must be a constant",
						Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.High.Pos(), labelRange.High.End()),
					})
				}

				if !types.IsEnum(labelRange.High.Type()) {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("CASE label must be ENUM type, got '%v'", labelRange.High.Type()),
						Range:    t.ctx.Source.Span(t.ctx.FileName, labelRange.High.Pos(), labelRange.High.End()),
					})

					continue
				}

				enum = types.Underlying(caseType).(*types.EnumType)
				he, isEnumMember := enum.Variants[labelRange.High.String()]
				if !isEnumMember {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message: fmt.Sprintf("CASE label, '%s' is not a member of '%s'",
							labelRange.High.String(), caseType),
						Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.High.Pos(), labelRange.High.End()),
					})

					continue
				}

				if he < lowEnum {
					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message: fmt.Sprintf("Invalid case label range: %s .. %s.",
							labelRange.Low.String(), labelRange.High.String()),
						Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.Pos(), labelRange.End()),
					})

					continue
				}
				highEnum = he
			}

			// Check each value in range
			for val := lowEnum; val <= highEnum; val++ {
				if prev, exists := seen[val]; exists {
					pos := t.ctx.Source.Span(t.ctx.FileName, prev.Pos(), prev.End()).Start
					location := fmt.Sprintf("%s:%d:%d", pos.File, pos.Line, pos.Column)

					t.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message: fmt.Sprintf("duplicate case label '%s' (already defined at %s)",
							enum.GetVariant(val), location),
						Range: t.ctx.Source.Span(t.ctx.FileName, labelRange.Pos(), labelRange.End()),
					})
				} else {
					seen[val] = labelRange.Low
				}
			}

		}

		for _, statement := range c.StmtSeq {
			statement.Accept(t)
		}
	}
}

func (t *TypeChecker) checkTypeCase(stmt *ast.CaseStmt, caseType types.Type) {
	seen := map[string]ast.Node{}
	for _, c := range stmt.Cases {
		var (
			nilLabel       ast.Expression
			nilLabelExists bool
		)

		if len(c.CaseLabelList) != 1 {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "type CASE branch must have exactly one type label",
				Range:    t.ctx.Source.Span(t.ctx.FileName, c.StartOffset, c.EndOffset),
			})
			continue
		}
		c.CaseLabelList[0].Accept(t)

		label := c.CaseLabelList[0]
		labelType := label.Low.Type()

		extOf := types.IsExtensionOf(types.Underlying(labelType), types.Underlying(caseType))
		ptrExtOf := types.IsPointerExtensionOf(types.Underlying(labelType), types.Underlying(caseType))
		if !extOf && !ptrExtOf && !types.IsNil(types.Underlying(labelType)) {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message: fmt.Sprintf("CASE label '%v' is not an extension of '%v'",
					label, caseType),
				Range: t.ctx.Source.Span(t.ctx.FileName, label.Pos(), label.End()),
			})
		}

		if types.IsNil(labelType) {
			nilLabelExists = true
			nilLabel = label.Low.(*ast.Nil)
		}

		if !types.IsPointer(types.Underlying(caseType)) && nilLabelExists {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("cannot have a NIL CASE label for a non-pointer CASE expression '%v'", caseType),
				Range:    t.ctx.Source.Span(t.ctx.FileName, nilLabel.Pos(), nilLabel.End()),
			})
		}

		if name := labelType.String(); seen[name] != nil {
			t.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("duplicate CASE label '%v'", label.Low),
				Range:    t.ctx.Source.Span(t.ctx.FileName, label.Pos(), label.End()),
			})
		} else {
			seen[name] = label
		}

		dsg := stmt.Expr.(*ast.Designator)

		t.ctx.TypeOverrides[dsg.Symbol.Name()] = labelType

		for _, statement := range c.StmtSeq {
			statement.Accept(t)
		}

		delete(t.ctx.TypeOverrides, dsg.Symbol.Name())
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

func (t *TypeChecker) EvalConstUint32(expr ast.Expression) (uint32, error) {
	val := ast.EvalConstExpr(expr)
	switch v := val.(type) {
	case int:
		if v < 0 {
			return 0, fmt.Errorf("value must be unsigned integer, got %d", v)
		}
		return uint32(v), nil
	case uint32:
		return v, nil
	case uint64:
		if v > math.MaxUint32 {
			return 0, fmt.Errorf("value exceeds uint32 max: %d", v)
		}
		return uint32(v), nil
	case int64:
		if v < 0 || v > math.MaxUint32 {
			return 0, fmt.Errorf("value outside of [0..uint32] range: %d", v)
		}
		return uint32(v), nil
	default:
		return 0, fmt.Errorf("expected constant unsigned integer, got %T", val)
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

func (t *TypeChecker) checkTypeBoundRedefinition(proc *ast.ProcedureDecl) {
	if proc.Kind != ast.TypeBoundProcedureKind || proc.Head.Rcv == nil {
		return // not a type-bound procedure
	}

	recv := proc.Head.Rcv
	rcvType := types.Underlying(recv.Name.SemaType)
	rcvBase := types.BaseRecord(rcvType)

	// Look for overridden method in base types
	name := proc.Head.Name.Name
	for base := rcvBase; base != nil; base = types.BaseRecord(base) {
		if field := base.GetField(name); field != nil {
			// check that field is a method.
			pOrig, fieldIsProcedure := field.Type.(*types.ProcedureType)
			pRedef, procIsProcedure := proc.Head.Name.SemaType.(*types.ProcedureType)
			if !fieldIsProcedure || !procIsProcedure {
				continue
			}

			// Check parameter list match
			if !types.FormalParamsListMatch(pOrig.Params, pRedef.Params) {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("parameter mismatch between of super and redefinition of '%s'", name),
					Range:    t.ctx.Source.Span(t.ctx.FileName, proc.Head.Pos(), proc.Head.End()),
				})
			}

			// Check export rules
			if field.IsExported && recv.Name.Symbol.Props() == ast.Exported && proc.Head.Name.Symbol.Props() != ast.Exported {
				t.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  fmt.Sprintf("redefinition of exported procedure '%s' must also be exported", name),
					Range:    t.ctx.Source.Span(t.ctx.FileName, proc.Head.Pos(), proc.Head.Pos()),
				})
			}
			return
		}
	}
}
