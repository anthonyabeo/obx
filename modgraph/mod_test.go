package modgraph

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestScanModuleHeaders_MultipleModules(t *testing.T) {
	content := `
MODULE Math;
   VAR Pi: REAL;
END Math.

MODULE Util;
   IMPORT Math;
   VAR Tau: REAL;
END Util.

MODULE Main;
   IMPORT Util, Math;
   VAR Result: REAL;
   BEGIN
       Result := Util.Tau + Math.Pi;
   END Main.
`
	tmp := t.TempDir()
	file := filepath.Join(tmp, "multi_module.obx")
	if err := os.WriteFile(file, []byte(content), 0644); err != nil {
		t.Fatalf("write failed: %v", err)
	}

	headers, err := ScanModuleHeaders(file)
	if err != nil {
		t.Fatalf("scanModuleHeaders failed: %v", err)
	}

	if len(headers) != 3 {
		t.Fatalf("expected 3 headers, got %d", len(headers))
	}

	if headers[0].Name != "Math" || headers[1].Name != "Util" || headers[2].Name != "Main" {
		t.Errorf("unexpected modgraph names: %+v", headers)
	}

	if len(headers[1].Imports) != 1 || headers[1].Imports[0].Name != "Math" {
		t.Errorf("Util should import Math")
	}

	if len(headers[2].Imports) != 2 {
		t.Errorf("Main should import Util and Math")
	}
}

func TestTopoSortModulesNoCycle(t *testing.T) {
	content := `
MODULE Math;
   VAR Pi: REAL;
END Math.

MODULE Util;
   IMPORT Math;
   VAR Tau: REAL;
END Util.

MODULE Main;
   IMPORT Util, Math;
   VAR Result: REAL;
   BEGIN
       Result := Util.Tau + Math.Pi;
   END Main.
`

	tmp := t.TempDir()
	file := filepath.Join(tmp, "multi_module.obx")
	if err := os.WriteFile(file, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}

	headers, err := ScanModuleHeaders(file)
	if err != nil {
		t.Fatalf("scanModuleHeaders: %v", err)
	}

	// Step 2: Build import graph
	graph, err := BuildImportGraph(tmp, headers)
	if err != nil {
		t.Fatalf("BuildImportGraph failed: %v", err)
	}

	// Step 3: Topo sort
	sorted, err := TopoSort(graph)
	if err != nil {
		t.Fatalf("TopoSort failed: %v", err)
	}

	// ensure topological order: Math -> Util -> Main
	gotOrder := []string{}
	for _, h := range sorted {
		gotOrder = append(gotOrder, h.Name)
	}

	wantOrder := []string{"Math", "Util", "Main"}
	for i, want := range wantOrder {
		if gotOrder[i] != want {
			t.Errorf("order mismatch at %d: want %q, got %q", i, want, gotOrder[i])
		}
	}
}

func TestTopoSortModules(t *testing.T) {
	source := `MODULE C;
END C.

MODULE B;
   IMPORT C;
END B.

MODULE A;
   IMPORT B, C;
END A.`

	// Setup: Create a temporary directory
	tmpDir := t.TempDir()
	moduleFile := filepath.Join(tmpDir, "modules.obx")

	if err := os.WriteFile(moduleFile, []byte(source), 0644); err != nil {
		t.Fatalf("failed to write modgraph source: %v", err)
	}

	// Step 1: Scan headers
	headers, err := ScanModuleHeaders(moduleFile)
	if err != nil {
		t.Fatalf("ScanModuleHeaders failed: %v", err)
	}

	// Step 2: Build import graph
	graph, err := BuildImportGraph(tmpDir, headers)
	if err != nil {
		t.Fatalf("BuildImportGraph failed: %v", err)
	}

	// Step 3: Topo sort
	sorted, err := TopoSort(graph)
	if err != nil {
		t.Fatalf("TopoSort failed: %v", err)
	}

	// Step 4: Check order
	gotOrder := []string{}
	for _, h := range sorted {
		gotOrder = append(gotOrder, h.Name)
	}

	wantOrder := []string{"C", "B", "A"}

	for i, want := range wantOrder {
		if gotOrder[i] != want {
			t.Errorf("order mismatch at %d: want %q, got %q", i, want, gotOrder[i])
		}
	}
}

func TestTopoSortModulesWithCycle(t *testing.T) {
	content := `
MODULE A;
   IMPORT B;
END A.

MODULE B;
   IMPORT C;
END B.

MODULE C;
   IMPORT A;
END C.
`

	tmp := t.TempDir()
	file := filepath.Join(tmp, "cycle.obx")
	if err := os.WriteFile(file, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}

	// Step 1: Scan headers
	headers, err := ScanModuleHeaders(file)
	if err != nil {
		t.Fatalf("ScanModuleHeaders failed: %v", err)
	}

	// Step 2: Build import graph
	graph, err := BuildImportGraph(tmp, headers)
	if err != nil {
		t.Fatalf("BuildImportGraph failed: %v", err)
	}

	// Step 3: Topo sort
	_, err = TopoSort(graph)
	if err == nil {
		t.Fatal("expected cycle error, got nil")
	}
}

func TestImportCycleDetection(t *testing.T) {
	tmpDir := t.TempDir()
	moduleFile := filepath.Join(tmpDir, "modules.obx")

	source := `MODULE A;
   IMPORT B;
END A.

MODULE B;
   IMPORT C;
END B.

MODULE C;
   IMPORT A;
END C.`

	if err := os.WriteFile(moduleFile, []byte(source), 0644); err != nil {
		t.Fatalf("failed to write modgraph source: %v", err)
	}

	headers, err := ScanModuleHeaders(moduleFile)
	if err != nil {
		t.Fatalf("ScanModuleHeaders failed: %v", err)
	}

	graph, err := BuildImportGraph(tmpDir, headers)
	if err != nil {
		t.Fatalf("BuildImportGraph failed: %v", err)
	}

	_, err = TopoSort(graph)
	if err == nil {
		t.Fatal("expected an error due to import cycle, got nil")
	}

	t.Logf("detected expected cycle error: %v", err)
}

func TestSelfImportDetection(t *testing.T) {
	tmpDir := t.TempDir()
	moduleFile := filepath.Join(tmpDir, "modules.obx")

	source := `MODULE A;
   IMPORT A;
END A.`

	if err := os.WriteFile(moduleFile, []byte(source), 0644); err != nil {
		t.Fatalf("failed to write modgraph source: %v", err)
	}

	headers, err := ScanModuleHeaders(moduleFile)
	if err != nil {
		t.Fatalf("ScanModuleHeaders failed: %v", err)
	}

	_, err = BuildImportGraph(tmpDir, headers)
	if err == nil {
		t.Fatal("expected an error due to self-import, got nil")
	}

	t.Logf("detected expected self-import error: %v", err)
}

func TestTopoSortValid(t *testing.T) {
	graph := &ImportGraph{
		Headers: map[ModuleID]Header{
			1: {ID: 1, Path: "", Name: "A", File: "a.mod"},
			2: {ID: 2, Path: "", Name: "B", File: "b.mod"},
			3: {ID: 3, Path: "", Name: "C", File: "c.mod"},
		},
		Adj: map[ModuleID][]ModuleID{
			1: {2},
			2: {3},
			3: {},
		},
	}

	order, err := TopoSort(graph)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var got []string
	for _, h := range order {
		got = append(got, h.Name)
	}
	want := []string{"C", "B", "A"}

	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Errorf("unexpected topo order: got %v, want %v", got, want)
	}
}

func TestTopoSortCycle(t *testing.T) {
	graph := &ImportGraph{
		Headers: map[ModuleID]Header{
			1: {ID: 1, Path: "", Name: "Main", File: "main.mod"},
			2: {ID: 2, Path: "", Name: "A", File: "a.mod"},
			3: {ID: 3, Path: "", Name: "B", File: "b.mod"},
		},
		Adj: map[ModuleID][]ModuleID{
			1: {2},
			2: {3},
			3: {1}, // cycle here
		},
	}

	_, err := TopoSort(graph)
	if err == nil {
		t.Fatal("expected cycle error, got nil")
	}

	expected := []string{
		"Main (main.mod)",
		"A (a.mod)",
		"B (b.mod)",
		"Main (main.mod)",
	}
	for _, part := range expected {
		if !strings.Contains(err.Error(), part) {
			t.Errorf("expected error to include %q, got %q", part, err.Error())
		}
	}
	fmt.Println("Cycle:", err.Error())
}

func TestTopoSortSelfImport(t *testing.T) {
	graph := &ImportGraph{
		Headers: map[ModuleID]Header{
			1: {ID: 1, Path: "", Name: "Self", File: "self.mod"},
		},
		Adj: map[ModuleID][]ModuleID{
			1: {1}, // self import
		},
	}

	_, err := TopoSort(graph)
	if err == nil {
		t.Fatal("expected self-cycle error, got nil")
	}
	if !strings.Contains(err.Error(), "Self (self.mod)") {
		t.Errorf("expected error to mention Self (self.mod), got %q", err.Error())
	}
	fmt.Println("Self-cycle:", err.Error())
}
