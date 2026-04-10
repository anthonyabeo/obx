package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/anthonyabeo/obx/src/adt"
	"github.com/anthonyabeo/obx/src/diag"
	"github.com/anthonyabeo/obx/src/diag/formatter"
	"github.com/anthonyabeo/obx/src/modgraph"
	"github.com/anthonyabeo/obx/src/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

// resolveModules discovers every .obx file under the given source roots,
// derives their ModuleKeys from the filesystem layout, builds the import
// graph, and returns the headers in topological (dependency-first) order
// together with the graph (needed for entry-point filtering).
func resolveModules(roots ...string) ([]modgraph.Header, *modgraph.ImportGraph, error) {
	r := modgraph.NewResolver(roots...)

	headers, err := r.DiscoverAll()
	if err != nil {
		return nil, nil, fmt.Errorf("discover: %w", err)
	}

	graph, err := modgraph.BuildImportGraph(headers)
	if err != nil {
		return nil, nil, fmt.Errorf("import graph: %w", err)
	}

	sorted, err := modgraph.TopoSort(graph)
	if err != nil {
		return nil, nil, fmt.Errorf("topo sort: %w", err)
	}

	return sorted, graph, nil
}

// reachableFrom filters sorted to the entry module and its transitive
// dependencies, preserving topological order. Returns sorted unchanged if
// entry is empty or not found in the graph.
func reachableFrom(sorted []modgraph.Header, graph *modgraph.ImportGraph, entry string) []modgraph.Header {
	if entry == "" {
		return sorted
	}
	if _, ok := graph.Headers[entry]; !ok {
		return sorted
	}

	reachable := make(map[string]bool)
	var walk func(string)
	walk = func(key string) {
		if reachable[key] {
			return
		}
		reachable[key] = true
		for _, dep := range graph.Adj[key] {
			walk(dep)
		}
	}
	walk(entry)

	out := make([]modgraph.Header, 0, len(reachable))
	for _, h := range sorted {
		if reachable[h.Key.String()] {
			out = append(out, h)
		}
	}
	return out
}

// newContext builds a diag.Context wired to a fresh SourceManager and a
// BufferedReporter that writes to stderr. tabWidth controls tab expansion in
// rendered diagnostic snippets.
func newContext(tabWidth, maxErrors int) (*diag.Context, *source.Manager) {
	srcMgr := source.NewSourceManager()
	reporter := diag.NewBufferedReporter(srcMgr, maxErrors,
		diag.Stderr(formatter.NewTextFormatter(srcMgr, tabWidth)),
	)
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
