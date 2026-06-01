package desugar

import "github.com/anthonyabeo/obx/src/syntax/ast"

type Module struct {
	Name    string
	IsEntry bool
	DLLName string   // non-empty when this is a DEFINITION module with [dll "X"]
	Imports []*ast.Import
	Decls   []Decl
	Init    *Function
}

type Program struct {
	Modules []*Module
}
