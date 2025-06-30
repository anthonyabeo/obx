package report

import (
	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/types"
)

type Context struct {
	FileName string
	FilePath string
	Content  []byte

	Source   *SourceManager
	Reporter Reporter
	TabWidth int

	Env  *ast.Environment            // the current environment. Defaults to that of the current scope
	Envs map[string]*ast.Environment // environments of compilation units

	// use to disambiguate between type-guards and procedure calls during parsing
	Names     *adt.Stack[string]
	ExprLists *adt.Stack[[]ast.Expression]

	SymbolOverrides map[string]ast.Symbol // temporary binding of name to new type
	TypeOverrides   map[string]types.Type // temporary types
}
