package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/diag"
	"github.com/anthonyabeo/obx/src/diag/emit"
	"github.com/anthonyabeo/obx/src/modgraph"
	"github.com/anthonyabeo/obx/src/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

// resolveModules discovers every .obx file under path, scans their headers,
// builds the import graph, and returns them in topological (dependency) order.
func resolveModules(path string) ([]modgraph.Header, error) {
	files, err := modgraph.DiscoverModuleFiles(path)
	if err != nil {
		return nil, fmt.Errorf("discover: %w", err)
	}

	var headers []modgraph.Header
	for _, file := range files {
		mods, err := modgraph.ScanModuleHeaders(file)
		if err != nil {
			return nil, fmt.Errorf("scan %s: %w", file, err)
		}
		headers = append(headers, mods...)
	}

	graph, err := modgraph.BuildImportGraph(path, headers)
	if err != nil {
		return nil, fmt.Errorf("import graph: %w", err)
	}

	sorted, err := modgraph.TopoSort(graph)
	if err != nil {
		return nil, fmt.Errorf("topo sort: %w", err)
	}

	return sorted, nil
}

// newContext builds a diag.Context wired to a fresh SourceManager and a
// BufferedReporter that writes to stderr. tabWidth is a display-only setting
// that controls tab expansion in rendered diagnostic snippets.
func newContext(tabWidth, maxErrors int) (*diag.Context, *source.Manager) {
	srcMgr := source.NewSourceManager()
	reporter := diag.NewBufferedReporter(srcMgr, maxErrors, emit.StdoutSink{
		Source:   srcMgr,
		Writer:   os.Stderr,
		TabWidth: tabWidth,
	})
	ctx := &diag.Context{
		Source:                srcMgr,
		Reporter:              reporter,
		Env:                   ast.NewEnv(),
		Names:                 adt.NewStack[string](),
		ExprLists:             adt.NewStack[[]ast.Expression](),
		TargetMachineWordSize: 8,
	}
	return ctx, srcMgr
}

// parseModules parses every header (in sorted order) into obx.
// Returns false and flushes diagnostics if any parse errors are found.
func parseModules(sorted []modgraph.Header, ctx *diag.Context, obx *ast.OberonX) bool {
	for _, header := range sorted {
		data, err := os.ReadFile(header.File)
		if err != nil {
			log.Fatalf("read %s: %v", header.File, err)
		}

		ctx.FileName = filepath.Base(header.File)
		ctx.FilePath = header.File
		ctx.Content = data[header.StartPos:header.EndPos]

		p := parser.NewParser(ctx)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			return false
		}

		obx.AddUnit(unit)
	}
	return true
}
