package sema

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/types"
)

var loopIDCounter int

func newLoopLabel(loopKind string) string {
	l := fmt.Sprintf("%s.loop.%d", loopKind, loopIDCounter)
	loopIDCounter++

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
