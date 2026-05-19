package cache_test

// roundtrip_test.go — verifies that SaveBundle / LoadBundle correctly
// roundtrip every kind of minir entity.
//
// This test catches the previous bug where WriteModule wrote raw minir bytes
// (opcodes 0x20–0x23) that DecodeBundle never matched (expected 0x10–0x13),
// silently producing empty modules.

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/anthonyabeo/obx/src/ir/minir"
	"github.com/anthonyabeo/obx/src/support/cache"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

func TestSaveLoadBundle_RoundTrip(t *testing.T) {
	// Build a module with one of each entity type.
	m := &minir.Module{Name: "TestMod"}

	gv := &minir.GlobalVar{
		Name:    "TestMod.x",
		Ty:      minir.I32(),
		Linkage: minir.ExternalLinkage,
	}
	m.Globals = append(m.Globals, gv)

	gc := &minir.GlobalConst{
		Name:    "TestMod.MaxVal",
		Ty:      minir.I32(),
		Linkage: minir.InternalLinkage,
		Init:    minir.ConstInt("42", 42, minir.I32()),
	}
	m.Constants = append(m.Constants, gc)

	ef := &minir.ExternalFunc{
		Name:    "puts",
		Linkage: minir.ExternalLinkage,
		Sig: &minir.FunctionType{
			Params: []minir.Type{minir.Ptr(minir.I32())},
			Result: minir.I32(),
		},
		Attrs: &minir.ExternalAttrs{
			CName:   "puts",
			DLLName: "libc",
		},
	}
	m.Externals = append(m.Externals, ef)

	// Build a minimal scope.
	scope := ast.NewLexicalScope(nil, "TestMod")

	// Write the bundle to a temp file.
	dir := t.TempDir()
	path := filepath.Join(dir, "TestMod.obxi")
	srcContent := []byte("fake source content for hash")
	if err := cache.SaveBundle(path, "TestMod", srcContent, scope, m); err != nil {
		t.Fatalf("SaveBundle: %v", err)
	}

	// Read it back.
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}

	bundle, err := cache.DecodeBundle(data, srcContent)
	if err != nil {
		t.Fatalf("DecodeBundle: %v", err)
	}

	if bundle.ModuleName != "TestMod" {
		t.Errorf("ModuleName = %q, want %q", bundle.ModuleName, "TestMod")
	}
	if bundle.Module == nil {
		t.Fatal("Module is nil after decode")
	}

	if got := len(bundle.Module.Globals); got != 1 {
		t.Errorf("Globals: got %d, want 1", got)
	} else if bundle.Module.Globals[0].Name != "TestMod.x" {
		t.Errorf("Global name = %q, want %q", bundle.Module.Globals[0].Name, "TestMod.x")
	}
	if _, ok := bundle.Module.SymTab.Lookup("TestMod$x"); !ok {
		t.Errorf("module SymTab missing canonical alias %q", "TestMod$x")
	}

	if got := len(bundle.Module.Constants); got != 1 {
		t.Errorf("Constants: got %d, want 1", got)
	} else if bundle.Module.Constants[0].Name != "TestMod.MaxVal" {
		t.Errorf("Const name = %q, want %q", bundle.Module.Constants[0].Name, "TestMod.MaxVal")
	}

	if got := len(bundle.Module.Externals); got != 1 {
		t.Errorf("Externals: got %d, want 1", got)
	} else if bundle.Module.Externals[0].Name != "puts" {
		t.Errorf("External name = %q, want %q", bundle.Module.Externals[0].Name, "puts")
	}
}

// TestDefToBundle_ExternalFuncs ensures DefToBundle produces non-empty
// Externals for a module declared with [dll "..."].
func TestDefToBundle_ExternalFuncs(t *testing.T) {
	src := []byte(`module Stdio [dll "libc"]
proc printf*(fmt: integer): integer [varargs]
proc puts*(s: integer): integer
end Stdio`)

	b, err := cache.DefToBundle(src)
	if err != nil {
		t.Fatalf("DefToBundle: %v", err)
	}
	if b.Module == nil {
		t.Fatal("Module is nil")
	}
	if len(b.Module.Externals) == 0 {
		t.Errorf("Externals is empty; expected at least 2 external procs")
	}
}

func TestDefToBundle_GlobalNamesUseDollar(t *testing.T) {
	src := []byte(`module Foo
var StdOut*: integer
end Foo`)

	b, err := cache.DefToBundle(src)
	if err != nil {
		t.Fatalf("DefToBundle: %v", err)
	}
	if b.Module == nil {
		t.Fatal("Module is nil")
	}
	if got := len(b.Module.Globals); got != 1 {
		t.Fatalf("Globals: got %d, want 1", got)
	}
	if got := b.Module.Globals[0].Name; got != "Foo$StdOut" {
		t.Fatalf("Global name = %q, want %q", got, "Foo$StdOut")
	}
	if _, ok := b.Module.SymTab.Lookup("Foo$StdOut"); !ok {
		t.Fatalf("module SymTab missing %q", "Foo$StdOut")
	}
}
