package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

// resolveModules discovers every .obx file under the given source roots,
// derives their ModuleKeys from the filesystem layout, builds the import
// graph, and returns the headers in topological (dependency-first) order
// together with the graph (needed for entry-point filtering).
func resolveModules(roots ...string) ([]project.Header, *project.ImportGraph, error) {
	r := project.NewResolver(roots...)

	headers, err := r.DiscoverAll()
	if err != nil {
		return nil, nil, fmt.Errorf("discover: %w", err)
	}

	graph, err := project.BuildImportGraph(headers)
	if err != nil {
		return nil, nil, fmt.Errorf("import graph: %w", err)
	}

	sorted, err := project.TopoSort(graph)
	if err != nil {
		return nil, nil, fmt.Errorf("topo sort: %w", err)
	}

	return sorted, graph, nil
}

// reachableFrom filters sorted to the entry module and its transitive
// dependencies, preserving topological order. Returns sorted unchanged if
// entry is empty or not found in the graph.
func reachableFrom(sorted []project.Header, graph *project.ImportGraph, entry string) []project.Header {
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

	out := make([]project.Header, 0, len(reachable))
	for _, h := range sorted {
		if reachable[h.Key.String()] {
			out = append(out, h)
		}
	}
	return out
}

// newContext builds a compiler.Context wired to a fresh SourceManager and a
// BufferedReporter that writes to stderr.
// The text formatter uses its built-in tab-width default of 4 spaces.
func newContext(maxErrors int) (*compiler.Context, *source.Manager) {
	srcMgr := source.NewSourceManager()
	reporter := diag.NewBufferedReporter(srcMgr, maxErrors,
		diag.Stderr(formatter.NewTextFormatter(srcMgr, 0)),
	)
	ctx := compiler.New("", srcMgr, reporter, ast.NewEnv(), 8)
	return ctx, srcMgr
}

// parseModules parses every header (in sorted order) into obx.
// Returns false and flushes diagnostics if any parse errors are found.
func parseModules(sorted []project.Header, ctx *compiler.Context, obx *ast.OberonX) bool {
	for _, header := range sorted {
		data, err := os.ReadFile(header.File)
		if err != nil {
			log.Fatalf("read %s: %v", header.File, err)
		}

		fileName := filepath.Base(header.File)
		content := data[header.StartPos:header.EndPos]

		p := parser.NewParser(ctx, fileName, content)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			return false
		}

		obx.AddUnit(unit)
	}
	return true
}

// applyDirectives parses each entry in defines (format "NAME" or "NAME=VALUE")
// and registers the corresponding compile-time directive constant on ctx via
// ctx.SetDirective.
//
// Supported value formats:
//   - Bare name with no "="  → bool true  (e.g. --define RISCV64)
//   - NAME=true / NAME=false → bool
//   - NAME=<integer>         → int64  (e.g. --define Version=3)
//   - NAME=<float>           → float64 (e.g. --define Pi=3.14)
//
// Returns an error for malformed entries so callers can report and exit.
func applyDirectives(ctx *compiler.Context, defines []string) error {
	for _, def := range defines {
		name, valStr, hasEq := strings.Cut(def, "=")
		name = strings.TrimSpace(name)
		if name == "" {
			return fmt.Errorf("--define: empty directive name in %q", def)
		}
		if !hasEq {
			// Bare name: a boolean presence flag, e.g. -d RISCV64
			ctx.SetDirective(name, true)
			continue
		}
		valStr = strings.TrimSpace(valStr)
		// Try bool first (handles "true"/"false"/"1"/"0" etc.)
		if b, err := strconv.ParseBool(valStr); err == nil {
			ctx.SetDirective(name, b)
			continue
		}
		// Try integer
		if i, err := strconv.ParseInt(valStr, 10, 64); err == nil {
			ctx.SetDirective(name, i)
			continue
		}
		// Try float
		if f, err := strconv.ParseFloat(valStr, 64); err == nil {
			ctx.SetDirective(name, f)
			continue
		}
		return fmt.Errorf("--define: value %q for %q is not a recognised type (use bool, integer, or float)", valStr, name)
	}
	return nil
}

