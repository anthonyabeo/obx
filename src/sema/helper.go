package sema

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/ast"
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
