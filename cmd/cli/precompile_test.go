package cli

// precompile_test.go — integration tests for "obx precompile-stdlib".
//
// These tests require OBX_STDLIB to be set to the stdlib root directory.
// Skip gracefully when the variable is absent (e.g. in lightweight unit-test
// runs) so `go test ./...` never hard-fails without a configured stdlib.
//
// Run with:
//
//	OBX_STDLIB=/path/to/stdlib go test ./cmd/cli/... -run TestPrecompile -v

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/anthonyabeo/obx/src/support/cache"
)

// TestPrecompileStdlib runs "obx precompile-stdlib" against the real stdlib
// root and then verifies that every expected .obxi bundle is present at the
// correct path inside stdlib/cache/.
func TestPrecompileStdlib(t *testing.T) {
	stdlibRoot := os.Getenv("OBX_STDLIB")
	if stdlibRoot == "" {
		t.Skip("OBX_STDLIB not set; skipping stdlib precompile integration test")
	}
	if _, err := os.Stat(stdlibRoot); err != nil {
		t.Skipf("OBX_STDLIB=%s does not exist: %v", stdlibRoot, err)
	}

	// runPrecompile reads OBX_STDLIB, so make sure it points at our stdlib.
	t.Setenv("OBX_STDLIB", stdlibRoot)

	// Run the precompile pipeline.
	runPrecompile(nil, nil)

	cacheDir := filepath.Join(stdlibRoot, "cache")

	// ── Root stdlib modules ───────────────────────────────────────────────
	rootModules := []string{
		"IO", "Files", "OS", "Strings", "Math", "Mem", "Time", "Fmt",
	}
	for _, name := range rootModules {
		path := filepath.Join(cacheDir, name+".obxi")
		if _, err := os.Stat(path); err != nil {
			t.Errorf("expected root stdlib bundle missing: %s", path)
		} else {
			t.Logf("ok  %s", path)
		}
	}

	// ── POSIX FFI bundles ─────────────────────────────────────────────────
	posixModules := []string{
		"Stdio", "LibM", "Stdlib", "StringH", "TimeH", "Unistd",
	}
	for _, name := range posixModules {
		path := filepath.Join(cacheDir, "posix", name+".obxi")
		if _, err := os.Stat(path); err != nil {
			t.Errorf("expected posix FFI bundle missing: %s", path)
		} else {
			t.Logf("ok  %s", path)
		}
	}

	// ── Win32 FFI bundles ─────────────────────────────────────────────────
	win32Modules := []string{
		"Stdio", "LibM", "Stdlib", "StringH", "TimeH", "WinAPI",
	}
	for _, name := range win32Modules {
		path := filepath.Join(cacheDir, "win32", name+".obxi")
		if _, err := os.Stat(path); err != nil {
			t.Errorf("expected win32 FFI bundle missing: %s", path)
		} else {
			t.Logf("ok  %s", path)
		}
	}
}

// TestPrecompileIdem verifies that running "obx precompile-stdlib" twice
// produces bundles that decode without error on the second run (semantic
// idempotency).  Byte-for-byte identity is NOT guaranteed because Go map
// iteration makes symbol serialisation order non-deterministic; what matters
// is that the resulting scope and MIR are equivalent.
func TestPrecompileIdem(t *testing.T) {
	stdlibRoot := os.Getenv("OBX_STDLIB")
	if stdlibRoot == "" {
		t.Skip("OBX_STDLIB not set; skipping precompile idempotency test")
	}
	if _, err := os.Stat(stdlibRoot); err != nil {
		t.Skipf("OBX_STDLIB=%s does not exist: %v", stdlibRoot, err)
	}

	t.Setenv("OBX_STDLIB", stdlibRoot)

	// First run.
	runPrecompile(nil, nil)

	cacheDir := filepath.Join(stdlibRoot, "cache")
	probeNames := []string{"IO", "Files", "OS", "Math"}

	sizes := make(map[string]int64)
	for _, name := range probeNames {
		path := filepath.Join(cacheDir, name+".obxi")
		info, err := os.Stat(path)
		if err != nil {
			t.Fatalf("stat %s after first run: %v", path, err)
		}
		sizes[name] = info.Size()
	}

	// Second run.
	runPrecompile(nil, nil)

	// Verify bundles are the same size and decode cleanly.
	for _, name := range probeNames {
		path := filepath.Join(cacheDir, name+".obxi")
		info, err := os.Stat(path)
		if err != nil {
			t.Fatalf("stat %s after second run: %v", path, err)
		}
		if sizes[name] != info.Size() {
			t.Errorf("%s: bundle size changed between runs (%d → %d)", name, sizes[name], info.Size())
		}
		// Decode without source content (skip hash check) to verify structural validity.
		data, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		if _, err := decodeBundleBytes(data); err != nil {
			t.Errorf("%s: bundle decode failed after second run: %v", name, err)
		} else {
			t.Logf("ok  %s (%d bytes, decodable)", path, info.Size())
		}
	}
}

// decodeBundleBytes is a thin wrapper, so the test does not depend on the
// cache package's exported API changing — it just confirms the file parses.
func decodeBundleBytes(data []byte) (interface{}, error) {
	return cache.DecodeBundle(data, nil)
}
