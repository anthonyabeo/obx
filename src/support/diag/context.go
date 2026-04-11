package diag

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type Context struct {
	FileName string
	FilePath string
	Content  []byte

	Source   *source.Manager
	Reporter Reporter

	Env *ast.Environment // manages scopes (lexical, record)

	SymbolOverrides map[string]ast.Symbol // temporary binding of name to new type
	TypeOverrides   map[string]types.Type // temporary types

	TargetMachineWordSize uint64 // in bytes
}
