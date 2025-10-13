package sema

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/types"
)

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
			sym := n.ctx.Env.Lookup(t.Name.Name)
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

// alignTo rounds `offset` up to the nearest multiple of `alignment`.
// alignment must be a power of two.
func alignTo(offset, alignment int) int {
	if alignment <= 0 {
		panic("alignment must be > 0")
	}
	mask := alignment - 1
	return (offset + mask) &^ mask
}
