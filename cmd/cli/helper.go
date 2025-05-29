package cli

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

func loadModule(ctx *report.Context, obx *ast.OberonX, visited map[string]bool, modulePath, moduleName string, PathMap map[string]string) error {
	if visited[moduleName] {
		return nil
	}
	visited[moduleName] = true

	path := PathMap[moduleName]
	content, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("could not read module %q at %q: %w", moduleName, path, err)
	}

	// Update the context for this file
	ctx.FileName = filepath.Base(path)
	ctx.FilePath = path
	ctx.Content = content
	ctx.Env = ast.NewEnvironment(ast.GlobalEnviron, moduleName)
	ctx.Envs[moduleName] = ctx.Env

	p := parser.NewParser(ctx)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
	}

	obx.Units[moduleName] = unit

	for _, imp := range unit.Imports() {
		if err := loadModule(ctx, obx, visited, modulePath, imp.Name, PathMap); err != nil {
			return err
		}
	}

	return nil
}
