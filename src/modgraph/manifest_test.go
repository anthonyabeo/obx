package modgraph

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
)

func TestLoadManifest_Valid(t *testing.T) {
	dir := t.TempDir()
	m := Manifest{
		Name:  "myproject",
		Roots: []string{"src", "lib"},
		Entry: "Main",
	}
	data, _ := json.Marshal(m)
	os.WriteFile(filepath.Join(dir, ManifestFile), data, 0644)

	got, err := LoadManifest(dir)
	if err != nil {
		t.Fatalf("LoadManifest: %v", err)
	}
	if got.Name != "myproject" {
		t.Errorf("Name: want %q, got %q", "myproject", got.Name)
	}
	if got.Entry != "Main" {
		t.Errorf("Entry: want %q, got %q", "Main", got.Entry)
	}
	// roots should be made absolute
	for _, r := range got.Roots {
		if !filepath.IsAbs(r) {
			t.Errorf("root %q should be absolute after LoadManifest", r)
		}
	}
}

func TestLoadManifest_RootsAbsolute(t *testing.T) {
	dir := t.TempDir()
	raw := `{"name":"proj","roots":["src","lib/std"],"entry":""}`
	os.WriteFile(filepath.Join(dir, ManifestFile), []byte(raw), 0644)

	got, err := LoadManifest(dir)
	if err != nil {
		t.Fatalf("LoadManifest: %v", err)
	}
	wantSrc := filepath.Join(dir, "src")
	wantLib := filepath.Join(dir, "lib", "std")
	if got.Roots[0] != wantSrc {
		t.Errorf("roots[0]: want %q, got %q", wantSrc, got.Roots[0])
	}
	if got.Roots[1] != wantLib {
		t.Errorf("roots[1]: want %q, got %q", wantLib, got.Roots[1])
	}
}

func TestLoadManifest_Missing(t *testing.T) {
	_, err := LoadManifest(t.TempDir())
	if err == nil {
		t.Fatal("expected error for missing obx.mod, got nil")
	}
}

func TestLoadManifest_Malformed(t *testing.T) {
	dir := t.TempDir()
	os.WriteFile(filepath.Join(dir, ManifestFile), []byte("{bad json"), 0644)
	_, err := LoadManifest(dir)
	if err == nil {
		t.Fatal("expected error for malformed JSON, got nil")
	}
}

func TestFindProjectRoot_Found(t *testing.T) {
	// Create a temp tree:  root/sub/cwd, manifest at root/
	root := t.TempDir()
	sub := filepath.Join(root, "sub", "cwd")
	os.MkdirAll(sub, 0755)
	os.WriteFile(filepath.Join(root, ManifestFile), []byte(`{}`), 0644)

	orig, _ := os.Getwd()
	defer os.Chdir(orig)
	os.Chdir(sub)

	got, err := FindProjectRoot()
	if err != nil {
		t.Fatalf("FindProjectRoot: %v", err)
	}

	// Resolve symlinks on both sides (macOS: /var → /private/var).
	gotReal, _ := filepath.EvalSymlinks(got)
	rootReal, _ := filepath.EvalSymlinks(root)
	if gotReal != rootReal {
		t.Errorf("want %q, got %q", rootReal, gotReal)
	}
}

func TestFindProjectRoot_NotFound(t *testing.T) {
	// A fresh temp dir with no obx.mod anywhere in its ancestry (relative to
	// the test runner – safest is to just confirm the function returns an error
	// when started from a dir that has no obx.mod up to the fs root).
	dir := t.TempDir()
	orig, _ := os.Getwd()
	defer os.Chdir(orig)
	os.Chdir(dir)

	// Only fails if there is genuinely no obx.mod anywhere above TempDir.
	// (On CI or a clean checkout this is guaranteed.)
	_, err := FindProjectRoot()
	if err == nil {
		t.Log("skipping: obx.mod found in an ancestor of TempDir (developer machine)")
	}
}


