package parser

import (
	"testing"

	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// newCtx constructs a compiler.Context seeded with the supplied directive
// name→bool map. Keys are case-sensitive and mapped directly to
// ctx.Directives via SetDirective.
func newCtx(t *testing.T, dirs map[string]bool) *compiler.Context {
	t.Helper()
	mgr := source.NewSourceManager()
	rep := diag.NewBufferedReporter(mgr, 100,
		diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
	ctx := compiler.New("directives_test", mgr, rep, ast.NewEnv(), 8)
	for k, v := range dirs {
		ctx.SetDirective(k, v)
	}
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
	ctx := newCtx(t, map[string]bool{"DARWIN": true, "POSIX": true, "WINDOWS": false})
	obx := ast.NewOberonX()
	p := NewParser(ctx, "directives_test.obx", src)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("parse errors in directives_test.obx")
	}
	obx.AddUnit(unit)
}

func TestWindowsDirective(t *testing.T) {
	src := []byte(`module Main
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

	ctx := newCtx(t, map[string]bool{"WINDOWS": true, "POSIX": false})
	p := NewParser(ctx, "directives_windows.obx", src)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("parse errors in directives_windows.obx")
	}

	main := unit.(*ast.Module)
	if len(main.ImportList) < 2 {
		t.Fatalf("expected at least 2 imports, got %d", len(main.ImportList))
	}

	// check that the conditional import selected win32.Stdio
	imp := main.ImportList[1]
	if imp.String() != "S := win32.Stdio" {
		t.Fatalf("expected import 'S := win32.Stdio', got '%s'", imp.String())
	}
}

func TestIfElsifElseDirective(t *testing.T) {
	src := []byte(`module M
<* IF DARWIN THEN *>
  import A := posix.Stdio
<* ELSIF LINUX THEN *>
  import A := linux.Stdio
<* ELSE *>
  import A := fallback.Stdio
<* END *>
end M
`)

	// DARWIN true -> picks posix
	ctx := newCtx(t, map[string]bool{"DARWIN": true, "LINUX": false})
	p := NewParser(ctx, "d1.obx", src)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("parse errors in d1.obx")
	}
	main := unit.(*ast.Module)
	if len(main.ImportList) != 1 {
		t.Fatalf("expected 1 import, got %d", len(main.ImportList))
	}
	if main.ImportList[0].String() != "A := posix.Stdio" {
		t.Fatalf("unexpected import: %s", main.ImportList[0].String())
	}

	// LINUX true -> picks linux
	ctx2 := newCtx(t, map[string]bool{"DARWIN": false, "LINUX": true})
	p2 := NewParser(ctx2, "d2.obx", src)
	unit2 := p2.Parse()
	if ctx2.Reporter.ErrorCount() > 0 {
		ctx2.Reporter.Flush()
		t.Fatal("parse errors in d2.obx")
	}
	main2 := unit2.(*ast.Module)
	if main2.ImportList[0].String() != "A := linux.Stdio" {
		t.Fatalf("unexpected import: %s", main2.ImportList[0].String())
	}

	// neither true -> picks fallback
	ctx3 := newCtx(t, map[string]bool{"DARWIN": false, "LINUX": false})
	p3 := NewParser(ctx3, "d3.obx", src)
	unit3 := p3.Parse()
	if ctx3.Reporter.ErrorCount() > 0 {
		ctx3.Reporter.Flush()
		t.Fatal("parse errors in d3.obx")
	}
	main3 := unit3.(*ast.Module)
	if main3.ImportList[0].String() != "A := fallback.Stdio" {
		t.Fatalf("unexpected import: %s", main3.ImportList[0].String())
	}
}

func TestNestedDirectives(t *testing.T) {
	src := []byte(`module N
<* IF OUTER THEN *>
  import X := outermod
  <* IF INNER THEN *>
	import Y := innermod
  <* ELSE *>
	import Y := innerfallback
  <* END *>
<* ELSE *>
  import X := other
<* END *>
end N
`)

	ctx := newCtx(t, map[string]bool{"OUTER": true, "INNER": true})
	p := NewParser(ctx, "n1.obx", src)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatal("parse errors in n1.obx")
	}
	m := unit.(*ast.Module)
	// expect two imports: X and Y
	if len(m.ImportList) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(m.ImportList))
	}
	if m.ImportList[0].String() != "X := outermod" || m.ImportList[1].String() != "Y := innermod" {
		t.Fatalf("unexpected imports: %v", []string{m.ImportList[0].String(), m.ImportList[1].String()})
	}
}
