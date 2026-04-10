package project

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
)

// ManifestFile is the name of the project manifest that obx looks for.
const ManifestFile = "obx.mod"

// Manifest holds the contents of an obx.mod project file.
//
// Example obx.mod:
//
//	{
//	  "name":  "myproject",
//	  "roots": ["src", "lib"],
//	  "entry": "Main"
//	}
//
// roots are relative to the directory containing obx.mod and are converted
// to absolute paths by LoadManifest.
type Manifest struct {
	Name  string   `json:"name"`
	Roots []string `json:"roots"` // source root directories
	Entry string   `json:"entry"` // default entry module for build (optional)
}

// LoadManifest reads and parses the obx.mod file in dir.
// Relative roots are resolved relative to dir.
func LoadManifest(dir string) (Manifest, error) {
	path := filepath.Join(dir, ManifestFile)
	data, err := os.ReadFile(path)
	if err != nil {
		return Manifest{}, fmt.Errorf("read %s: %w", path, err)
	}

	var m Manifest
	if err := json.Unmarshal(data, &m); err != nil {
		return Manifest{}, fmt.Errorf("parse %s: %w", path, err)
	}

	for i, r := range m.Roots {
		if !filepath.IsAbs(r) {
			m.Roots[i] = filepath.Join(dir, r)
		}
	}

	return m, nil
}

// FindProjectRoot walks up from the working directory looking for an obx.mod
// file and returns the directory that contains it.
func FindProjectRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, ManifestFile)); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", fmt.Errorf("%s not found in any parent directory", ManifestFile)
}

// WriteManifest serialises m as indented JSON and writes it to
// filepath.Join(dir, ManifestFile).  Roots should be relative to dir.
func WriteManifest(dir string, m Manifest) error {
	data, err := json.MarshalIndent(m, "", "  ")
	if err != nil {
		return fmt.Errorf("marshal manifest: %w", err)
	}
	path := filepath.Join(dir, ManifestFile)
	if err := os.WriteFile(path, append(data, '\n'), 0644); err != nil {
		return fmt.Errorf("write %s: %w", path, err)
	}
	return nil
}
