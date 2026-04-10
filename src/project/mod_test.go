package project

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// ── helpers ──────────────────────────────────────────────────────────────────

// writeFile creates a file inside dir and returns its absolute path.
func writeFile(t *testing.T, dir, name, content string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
	return path
}

// ── ModuleKey ────────────────────────────────────────────────────────────────

func TestModuleKey_Bare(t *testing.T) {
	k := NewKey("Math")
	if k.Name() != "Math" {
		t.Errorf("Name: want %q, got %q", "Math", k.Name())
	}
	if k.String() != "Math" {
		t.Errorf("String: want %q, got %q", "Math", k.String())
	}
	if len(k.DirSegs()) != 0 {
		t.Errorf("DirSegs: want empty, got %v", k.DirSegs())
	}
}

func TestModuleKey_Pathed(t *testing.T) {
	k := NewKey("Collections", "Drawing")
	if k.Name() != "Drawing" {
		t.Errorf("Name: want %q, got %q", "Drawing", k.Name())
	}
	if k.String() != "Collections.Drawing" {
		t.Errorf("String: want %q, got %q", "Collections.Drawing", k.String())
	}
	if got := k.DirSegs(); len(got) != 1 || got[0] != "Collections" {
		t.Errorf("DirSegs: want [Collections], got %v", got)
	}
}

func TestModuleKey_ToFilePath(t *testing.T) {
	k := NewKey("Collections", "Drawing")
	got := k.ToFilePath("/src")
	want := filepath.Join("/src", "Collections", "Drawing") + ".obx"
	if got != want {
		t.Errorf("ToFilePath: want %q, got %q", want, got)
	}
}

func TestKeyFromFile(t *testing.T) {
	root := "/src"
	file := filepath.Join(root, "Collections", "Drawing.obx")
	k, err := KeyFromFile(root, file)
	if err != nil {
		t.Fatalf("KeyFromFile: %v", err)
	}
	if k.String() != "Collections.Drawing" {
		t.Errorf("want %q, got %q", "Collections.Drawing", k.String())
	}
}

// ── ScanHeader ───────────────────────────────────────────────────────────────

func TestScanHeader_NoImports(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, "Math.obx", `
module Math
  var Pi: real
end Math
`)
	hdr, err := ScanHeader(root, filepath.Join(root, "Math.obx"))
	if err != nil {
		t.Fatalf("ScanHeader: %v", err)
	}
	if hdr.Key.String() != "Math" {
		t.Errorf("key: want %q, got %q", "Math", hdr.Key)
	}
	if len(hdr.Imports) != 0 {
		t.Errorf("imports: want 0, got %d", len(hdr.Imports))
	}
}

func TestScanHeader_WithImports(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, "Util.obx", `
module Util
  import Math
  var Tau: real
end Util
`)
	hdr, err := ScanHeader(root, filepath.Join(root, "Util.obx"))
	if err != nil {
		t.Fatalf("ScanHeader: %v", err)
	}
	if len(hdr.Imports) != 1 || hdr.Imports[0].Key.String() != "Math" {
		t.Errorf("imports: want [Math], got %v", hdr.Imports)
	}
}

func TestScanHeader_PathedImport(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, "Main.obx", `
module Main
  import D := Collections.Drawing
end Main
`)
	hdr, err := ScanHeader(root, filepath.Join(root, "Main.obx"))
	if err != nil {
		t.Fatalf("ScanHeader: %v", err)
	}
	if len(hdr.Imports) != 1 {
		t.Fatalf("want 1 import, got %d", len(hdr.Imports))
	}
	imp := hdr.Imports[0]
	if imp.Alias != "D" {
		t.Errorf("alias: want %q, got %q", "D", imp.Alias)
	}
	if imp.Key.String() != "Collections.Drawing" {
		t.Errorf("key: want %q, got %q", "Collections.Drawing", imp.Key)
	}
}

func TestScanHeader_CaseInsensitiveKeywords(t *testing.T) {
	root := t.TempDir()
	// All-uppercase keywords (classic Oberon style)
	writeFile(t, root, "Foo.obx", `
MODULE Foo;
  IMPORT Bar;
END Foo.
`)
	hdr, err := ScanHeader(root, filepath.Join(root, "Foo.obx"))
	if err != nil {
		t.Fatalf("ScanHeader: %v", err)
	}
	if hdr.Key.String() != "Foo" {
		t.Errorf("key: want %q, got %q", "Foo", hdr.Key)
	}
	if len(hdr.Imports) != 1 || hdr.Imports[0].Key.String() != "Bar" {
		t.Errorf("imports: want [Bar], got %v", hdr.Imports)
	}
}

func TestScanHeader_MismatchedName(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, "Foo.obx", `module Bar end Bar`)
	_, err := ScanHeader(root, filepath.Join(root, "Foo.obx"))
	if err == nil {
		t.Fatal("expected error for name mismatch, got nil")
	}
}

func TestScanHeader_BlockComment(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, "Math.obx", `
(* This is a block comment *)
module Math
  (* nested (* comment *) *)
  var Pi: real
end Math
`)
	if _, err := ScanHeader(root, filepath.Join(root, "Math.obx")); err != nil {
		t.Fatalf("ScanHeader with block comment: %v", err)
	}
}

// ── DiscoverAndScan ───────────────────────────────────────────────────────────

func TestDiscoverAndScan(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, "Math.obx", "module Math end Math")
	writeFile(t, root, "utils/Util.obx", "module Util\nimport Math\nend Util")

	hdrs, err := DiscoverAndScan(root)
	if err != nil {
		t.Fatalf("DiscoverAndScan: %v", err)
	}
	if len(hdrs) != 2 {
		t.Fatalf("want 2 headers, got %d", len(hdrs))
	}

	keys := make(map[string]bool)
	for _, h := range hdrs {
		keys[h.Key.String()] = true
	}
	if !keys["Math"] || !keys["utils.Util"] {
		t.Errorf("unexpected keys: %v", keys)
	}
}

// ── BuildImportGraph ──────────────────────────────────────────────────────────

func TestBuildImportGraph_NoCycle(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, "Math.obx", "module Math end Math")
	writeFile(t, root, "Util.obx", "module Util\nimport Math\nend Util")
	writeFile(t, root, "Main.obx", "module Main\nimport Util, Math\nend Main")

	hdrs, _ := DiscoverAndScan(root)
	graph, err := BuildImportGraph(hdrs)
	if err != nil {
		t.Fatalf("BuildImportGraph: %v", err)
	}

	sorted, err := TopoSort(graph)
	if err != nil {
		t.Fatalf("TopoSort: %v", err)
	}

	order := make([]string, len(sorted))
	for i, h := range sorted {
		order[i] = h.Key.String()
	}

	mathIdx := indexOf(order, "Math")
	utilIdx := indexOf(order, "Util")
	mainIdx := indexOf(order, "Main")
	if mathIdx > utilIdx || utilIdx > mainIdx {
		t.Errorf("wrong order: %v", order)
	}
}

func TestBuildImportGraph_SelfImport(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, "A.obx", "module A\nimport A\nend A")

	hdrs, _ := DiscoverAndScan(root)
	_, err := BuildImportGraph(hdrs)
	if err == nil {
		t.Fatal("expected self-import error, got nil")
	}
}

func TestBuildImportGraph_MissingDep(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, "A.obx", "module A\nimport B\nend A")

	hdrs, _ := DiscoverAndScan(root)
	_, err := BuildImportGraph(hdrs)
	if err == nil {
		t.Fatal("expected missing-module error, got nil")
	}
}

// ── TopoSort ─────────────────────────────────────────────────────────────────

func TestTopoSort_Cycle(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, "A.obx", "module A\nimport B\nend A")
	writeFile(t, root, "B.obx", "module B\nimport C\nend B")
	writeFile(t, root, "C.obx", "module C\nimport A\nend C")

	hdrs, _ := DiscoverAndScan(root)
	graph, err := BuildImportGraph(hdrs)
	if err != nil {
		t.Fatalf("BuildImportGraph: %v", err)
	}

	_, err = TopoSort(graph)
	if err == nil {
		t.Fatal("expected cycle error, got nil")
	}
	if !strings.Contains(err.Error(), "cycle") {
		t.Errorf("error should mention cycle: %v", err)
	}
	fmt.Println("detected cycle:", err)
}

func TestTopoSort_Manual(t *testing.T) {
	graph := &ImportGraph{
		Headers: map[string]Header{
			"A": {Key: NewKey("A"), File: "a.obx"},
			"B": {Key: NewKey("B"), File: "b.obx"},
			"C": {Key: NewKey("C"), File: "c.obx"},
		},
		Adj: map[string][]string{
			"A": {"B"},
			"B": {"C"},
			"C": {},
		},
	}

	order, err := TopoSort(graph)
	if err != nil {
		t.Fatalf("TopoSort: %v", err)
	}

	got := make([]string, len(order))
	for i, h := range order {
		got[i] = h.Key.String()
	}
	want := []string{"C", "B", "A"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Errorf("order: want %v, got %v", want, got)
	}
}

// ── Resolver ─────────────────────────────────────────────────────────────────

func TestResolver_Resolve(t *testing.T) {
	root := t.TempDir()
	writeFile(t, root, filepath.Join("Collections", "Drawing.obx"),
		"module Drawing end Drawing")

	r := NewResolver(root)
	path, err := r.Resolve(NewKey("Collections", "Drawing"))
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	want := filepath.Join(root, "Collections", "Drawing.obx")
	if path != want {
		t.Errorf("want %q, got %q", want, path)
	}
}

func TestResolver_NotFound(t *testing.T) {
	root := t.TempDir()
	r := NewResolver(root)
	_, err := r.Resolve(NewKey("Missing"))
	if err == nil {
		t.Fatal("expected error for missing module, got nil")
	}
}

func TestResolver_MultiRoot(t *testing.T) {
	root1 := t.TempDir()
	root2 := t.TempDir()
	writeFile(t, root2, "Stdlib.obx", "module Stdlib end Stdlib")

	r := NewResolver(root1, root2)
	path, err := r.Resolve(NewKey("Stdlib"))
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if !strings.HasSuffix(path, "Stdlib.obx") {
		t.Errorf("unexpected path: %q", path)
	}
}

// ── helpers ───────────────────────────────────────────────────────────────────

func indexOf(slice []string, s string) int {
	for i, v := range slice {
		if v == s {
			return i
		}
	}
	return -1
}
