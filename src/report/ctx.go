package report

import "github.com/anthonyabeo/obx/src/syntax/ast"

type Context struct {
	FileName string
	FilePath string
	Content  []byte

	Source   *SourceManager
	Reporter Reporter
	TabWidth int

	Env  *ast.Environment            // the current environment. Defaults to that of the current scope
	Envs map[string]*ast.Environment // environments of compilation units
}
