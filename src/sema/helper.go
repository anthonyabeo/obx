package sema

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// isFuncProc reports whether decl requires a RETURN value on every control
// path. This is true for FunctionProcedureKind procedures, and also for
// TypeBoundProcedureKind procedures that declare a return type.
func isFuncProc(decl *ast.ProcedureDecl) bool {
	switch decl.Kind {
	case ast.FunctionProcedureKind:
		return true
	case ast.TypeBoundProcedureKind:
		return decl.Head.FP != nil && decl.Head.FP.RetType != nil
	default:
		return false
	}
}

// returnsOnAllPaths reports whether every control path through stmts is
// guaranteed to reach a RETURN statement.
func returnsOnAllPaths(stmts []ast.Statement) bool {
	for _, stmt := range stmts {
		if stmtReturns(stmt) {
			return true
		}
	}
	return false
}

// stmtReturns reports whether stmt guarantees a RETURN on every control path
// it contains.
func stmtReturns(stmt ast.Statement) bool {
	switch s := stmt.(type) {
	case *ast.ReturnStmt:
		return true

	case *ast.IfStmt:
		// All branches must be present and return:
		//   – the THEN path
		//   – every ELSIF branch
		//   – an ELSE branch (absent ELSE ⟹ not all paths are covered)
		if len(s.ElsePath) == 0 {
			return false
		}
		if !returnsOnAllPaths(s.ThenPath) {
			return false
		}
		for _, elif := range s.ElseIfBranches {
			if !returnsOnAllPaths(elif.ThenPath) {
				return false
			}
		}
		return returnsOnAllPaths(s.ElsePath)

	case *ast.CaseStmt:
		// Every case arm and the ELSE branch must return.
		// Without an ELSE the discriminant value space is not fully covered.
		if !returnsOnAllPaths(s.Else) {
			return false
		}
		for _, c := range s.Cases {
			if !returnsOnAllPaths(c.StmtSeq) {
				return false
			}
		}
		return true

	default:
		// Loops, assignments, calls, etc. do not guarantee a return.
		return false
	}
}

func (f *FlowChecker) newLoopLabel(loopKind string) string {
	l := fmt.Sprintf("%s_loop_%d", loopKind, f.loopIDCounter)
	f.loopIDCounter++

	return l
}

func findAssignmentTo(dst string, mod ast.CompilationUnit) *ast.AssignmentStmt {
	var found *ast.AssignmentStmt

	ast.Walk(func(n ast.Node) bool {
		if assign, ok := n.(*ast.AssignmentStmt); ok {
			if dsg, ok := assign.LValue.(*ast.Designator); ok {
				if dsg.String() == dst {
					found = assign
					return false // Stop traversal
				}
			}
		}
		return true // Continue traversal
	}, mod)

	return found
}

func findProcedureByName(name string, unit ast.CompilationUnit) *ast.ProcedureDecl {
	var found *ast.ProcedureDecl

	ast.Walk(func(n ast.Node) bool {
		if decl, ok := n.(*ast.ProcedureDecl); ok {
			if decl.Head.Name.Name == name {
				found = decl
				return false // Stop traversal
			}
		}
		return true // Continue traversal
	}, unit)

	return found
}

func smallestTypeFor(value int64) types.Type {
	switch {
	case value >= 0 && value <= 255:
		return types.ByteType
	case value >= -128 && value <= 127:
		return types.Int8Type
	case value >= -32768 && value <= 32767:
		return types.Int16Type
	case value >= -2147483648 && value <= 2147483647:
		return types.Int32Type
	default:
		return types.Int64Type
	}
}

func IsCallable(dsg *ast.Designator) bool {
	if dsg.QIdent == nil || dsg.QIdent.Symbol == nil {
		return false
	}

	switch dsg.Symbol.Kind() {
	case ast.ProcedureSymbolKind:
		return true
	case ast.VariableSymbolKind, ast.ConstantSymbolKind, ast.FieldSymbolKind, ast.ParamSymbolKind:
		switch ty := dsg.Symbol.AstType().(type) {
		case *ast.ProcedureType:
			return true
		case *ast.NamedType:
			_, ok := ty.Symbol.AstType().(*ast.ProcedureType)
			return ok
		case *ast.PointerType:
			_, ok := ty.Base.(*ast.ProcedureType)
			return ok
		default:
			return false
		}
	default:
		return false
	}
}

func (n *NamesResolver) underlying(ty ast.Type) ast.Type {
	for {
		switch t := ty.(type) {
		case *ast.NamedType:
			var sym ast.Symbol
			if t.Name.Prefix != "" {
				scope := n.ctx.Env.ModuleScope(t.Name.Prefix)
				if scope == nil {
					return nil
				}
				sym = scope.Lookup(t.Name.Name)
			} else {
				sym = n.ctx.Env.Lookup(t.Name.Name)
			}
			if sym == nil {
				return nil
			}

			ty = sym.AstType()
		default:
			return t
		}
	}
}

func IsValidCaseDiscriminant(expr ast.Expression) bool {
	dsg, exprIsDesignator := expr.(*ast.Designator)
	if !exprIsDesignator {
		return false
	}

	semaType := types.Underlying(dsg.SemaType)
	if types.IsInteger(semaType) || types.IsEnum(semaType) || semaType == types.CharType {
		return true
	}

	// VAR param of record type
	if ast.IsVarParam(dsg) {
		if types.IsPointerToRecord(semaType) {
			return true
		}

		vt := types.Underlying(semaType)
		return types.IsRecord(vt)
	}
	return false
}

func IsValidGuardExpr(expr ast.Expression) bool {
	dsg, exprIsDesignator := expr.(*ast.Designator)
	if !exprIsDesignator {
		return false
	}

	// VAR param of record type
	if ast.IsVarParam(dsg) {
		if types.IsRecord(dsg.SemaType) || types.IsPointerToRecord(dsg.SemaType) {
			return true
		}

		vt := types.Underlying(dsg.SemaType)
		return types.IsRecord(vt)
	}
	return false
}

// AlignTo rounds `offset` up to the nearest multiple of `alignment`.
// alignment must be a power of two.
func AlignTo(offset, alignment int) int {
	if alignment <= 0 {
		panic("alignment must be > 0")
	}
	mask := alignment - 1
	return (offset + mask) &^ mask
}

// collectOuterProcScopes returns the set of LexicalScopes that belong to
// procedures enclosing decl in the lexical scope chain.  Starting from
// decl.Env.Parent() it walks up the parent chain, collecting every scope
// that is NOT the module scope (i.e. whose parent is ast.Global) and NOT
// ast.Global itself.  These are the procedure-owned scopes from which a
// type-bound procedure is forbidden from reading variables or parameters.
func collectOuterProcScopes(decl *ast.ProcedureDecl) map[*ast.LexicalScope]bool {
	result := make(map[*ast.LexicalScope]bool)
	if decl.Env == nil {
		return result
	}
	for s := decl.Env.Parent(); s != nil && s != ast.Global; s = s.Parent() {
		// The module scope has ast.Global as its direct parent; skip it.
		if s.Parent() != ast.Global {
			result[s] = true
		}
	}
	return result
}
