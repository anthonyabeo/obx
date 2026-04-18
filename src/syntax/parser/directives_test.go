package parser

import (
	"testing"

	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// newDarwinCtx creates a compiler.Context configured for macOS / POSIX target.
// WINDOWS=false means <* IF WINDOWS THEN *> blocks are skipped and the POSIX
// <* ELSE *> branch is selected.
func newDarwinCtx(t *testing.T) *compiler.Context {
	t.Helper()
	mgr := source.NewSourceManager()
	rep := diag.NewBufferedReporter(mgr, 100,
		diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
	ctx := compiler.New("stdlib_test", mgr, rep, ast.NewEnv(), 8)
	ctx.SetDirective("POSIX", true)
	ctx.SetDirective("DARWIN", true)
	ctx.SetDirective("LINUX", false)
	ctx.SetDirective("WINDOWS", false)
	return ctx
}

func TestDarwin(t *testing.T) {
	src := []byte(`
module Main
  import IO
<* IF WINDOWS THEN *>
  import S := win32.Stdio
<* ELSE *>
  import S := posix.Stdio
<* END *>

begin
 IO.WriteInt(42)
end Main
`)
	ctx := newDarwinCtx(t)
	obx := ast.NewOberonX()
	p := NewParser(ctx, "directives_test.obx", src)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("parse errors in directives_test.obx")
	}
	obx.AddUnit(unit)
}
