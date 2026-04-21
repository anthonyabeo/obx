// Package compiler defines the Context type threaded through every compiler
// phase beyond the scanner.  It embeds the thin *diag.Context (diagnostics)
// and adds language-specific state (environment) and machine-specific state
// (target word size).
package compiler

import (
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// TargetConfig holds machine-specific parameters needed by the IR builder and
// built-in lowering.
type TargetConfig struct {
	// WordSize is the pointer / word size in bytes for the target machine
	// (e.g. 8 for 64-bit targets).
	WordSize uint64
}

// Context is the single object threaded through every compiler phase.
//
// The embedded *diag.Context promotes FileName, Source, and Reporter so all
// ctx.Reporter.Report(…) and ctx.Source.Span(…) call sites work without
// changes.
//
// Content is intentionally absent: callers pass it directly to
// parser.NewParser so the shared context is never mutated per-file.
// FilePath is intentionally absent: it was never read by any compiler phase.
// SymbolOverrides / TypeOverrides are intentionally absent: they are transient
// per-walk scratch space that belongs inside NamesResolver / TypeChecker.
type Context struct {
	*diag.Context // FileName, Source, Reporter (promoted)

	// Env manages lexical and record scopes for the compilation unit.
	Env *ast.Environment

	// Target holds target-machine parameters used during IR lowering.
	Target TargetConfig

	// Directives holds compiler-supplied compile-time named constants that
	// are visible inside <* IF … *> / <* ASSERT … *> source directives.
	// Keys are case-sensitive. Values must be bool, int64, or float64.
	// Looked up before ctx.Env so the build system can shadow language CONSTs.
	Directives map[string]any
}

// New constructs a Context with a freshly allocated diag.Context wired to the
// supplied source manager and reporter.
func New(
	fileName string,
	src *source.Manager,
	reporter diag.Reporter,
	env *ast.Environment,
	wordSize uint64,
) *Context {
	return &Context{
		Context:    &diag.Context{FileName: fileName, Source: src, Reporter: reporter},
		Env:        env,
		Target:     TargetConfig{WordSize: wordSize},
		Directives: make(map[string]any),
	}
}

// SetDirective registers a compile-time directive constant.
// val must be bool, int64, or float64.
func (c *Context) SetDirective(name string, val any) {
	c.Directives[name] = val
}
