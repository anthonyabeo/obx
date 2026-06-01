package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	zlog "github.com/rs/zerolog/log"

	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/directive"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

type bootstrapOptions struct {
	Command   string
	Target    string
	Defines   []string
	MaxErrors int
	Roots     []string
	Path      string
	Entry     string
}

type bootstrapState struct {
	Ctx        *compiler.Context
	Machine    target.Target
	ProjectDir string
	Manifest   project.Manifest
	Roots      []string
	Entry      string
	StdlibRoot string // resolved stdlib root; empty when no stdlib was found
}

// resolveModules discovers every .obx file under the given source roots,
// derives their ModuleKeys from the filesystem layout, builds the import
// graph, and returns the headers in topological (dependency-first) order
// together with the graph (needed for entry-point filtering).
func resolveModules(ctx *compiler.Context, roots ...string) ([]project.Header, *project.ImportGraph, error) {
	r := project.NewResolver(roots...)

	//headers, err := r.DiscoverAll()
	headers, err := r.DiscoverAllWithResolver(directive.ResolverFromContext(ctx))
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

// bootstrapFrontEnd resolves the target, creates the compiler context, applies
// directives, and resolves project roots / manifest state for the command.
func bootstrapFrontEnd(opts bootstrapOptions) (*bootstrapState, error) {
	mach, err := target.Lookup(opts.Target)
	if err != nil {
		return nil, err
	}

	ctx, _ := newContext(opts.MaxErrors)
	injectPlatformDirectives(ctx, mach.Name())
	if err := applyDirectives(ctx, opts.Defines); err != nil {
		return nil, err
	}

	state := &bootstrapState{
		Ctx:        ctx,
		Machine:    mach,
		ProjectDir: ".",
		Manifest:   project.Manifest{},
		Roots:      append([]string(nil), opts.Roots...),
		Entry:      opts.Entry,
	}

	if opts.Path != "" {
		state.Roots = []string{opts.Path}
		return state, nil
	}

	if len(opts.Roots) == 0 {
		dir, err := project.FindProjectRoot()
		if err != nil {
			return nil, fmt.Errorf("%s: no --root given and %w", opts.Command, err)
		}
		state.ProjectDir = dir
		manifest, err := project.LoadManifest(dir)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", opts.Command, err)
		}
		state.Manifest = manifest
		state.Roots = manifest.Roots
		if state.Entry == "" {
			state.Entry = manifest.Entry
		}
	} else {
		if dir, err := project.FindProjectRoot(); err == nil {
			state.ProjectDir = dir
			manifest, err := project.LoadManifest(dir)
			if err != nil {
				zlog.Warn().Err(err).Str("dir", dir).Msg(opts.Command + ": ignoring invalid obx.mod while explicit roots are set")
			} else {
				state.Manifest = manifest
				if state.Entry == "" {
					state.Entry = manifest.Entry
				}
			}
		}
	}

	if sr := project.ResolveStdlibRoot(state.Manifest); sr != "" {
		state.StdlibRoot = sr
		state.Roots = append([]string{sr}, state.Roots...)
	}

	return state, nil
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

// injectPlatformDirectives sets compile-time boolean directives that stdlib
// wrapper modules use inside <* IF WINDOWS THEN *> / <* IF LINUX THEN *>
// blocks to select the correct FFI binding layer.
//
// Mapping:
//
//	rv64imafd                          → POSIX=true, LINUX=true
//	arm64-apple-macos / aarch64-*      → POSIX=true, DARWIN=true
//	x86_64-pc-windows / *windows*      → WINDOWS=true
//
// The platform directives are injected before user-supplied --define values
// so that explicit --define WINDOWS=false can still override the defaults.
func injectPlatformDirectives(ctx *compiler.Context, targetName string) {
	lower := strings.ToLower(targetName)
	switch {
	case strings.Contains(lower, "windows"):
		ctx.SetDirective("WINDOWS", true)
		ctx.SetDirective("POSIX", false)
		ctx.SetDirective("LINUX", false)
		ctx.SetDirective("DARWIN", false)
	case strings.Contains(lower, "darwin") ||
		strings.Contains(lower, "macos") ||
		strings.HasPrefix(lower, "arm64-apple"):
		ctx.SetDirective("POSIX", true)
		ctx.SetDirective("DARWIN", true)
		ctx.SetDirective("LINUX", false)
		ctx.SetDirective("WINDOWS", false)
	default:
		// rv64imafd and any other POSIX/Linux target.
		ctx.SetDirective("POSIX", true)
		ctx.SetDirective("LINUX", true)
		ctx.SetDirective("DARWIN", false)
		ctx.SetDirective("WINDOWS", false)
	}
}
