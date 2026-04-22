package project

import (
	"fmt"
	"path/filepath"
	"strings"
)

// ModuleKey uniquely identifies a module by its dot-separated import path.
//
// For "IMPORT Math"                → Segs: ["Math"]
// For "IMPORT Collections.Drawing" → Segs: ["Collections", "Drawing"]
//
// All but the last segment are directory components; the last segment is
// both the filename stem (Drawing.obx) and the declared MODULE name.
type ModuleKey struct {
	segs []string
}

// NewKey builds a ModuleKey from one or more path segments.
func NewKey(segs ...string) ModuleKey {
	cp := make([]string, len(segs))
	copy(cp, segs)
	return ModuleKey{segs: cp}
}

// KeyFromSegs builds a ModuleKey from a slice of segments
// (as stored in ast.Import.ImportPath).
func KeyFromSegs(segs []string) ModuleKey {
	return NewKey(segs...)
}

// KeyFromFile derives a ModuleKey from an absolute .obx file path and the
// root directory it lives under.
//
//	root="/src"  file="/src/Collections/Drawing.obx"
//	→  NewKey("Collections", "Drawing")
func KeyFromFile(root, file string) (ModuleKey, error) {
	rel, err := filepath.Rel(root, file)
	if err != nil {
		return ModuleKey{}, fmt.Errorf("cannot make %q relative to root %q: %w", file, root, err)
	}
	rel = strings.TrimSuffix(rel, ".obx")
	parts := strings.Split(filepath.ToSlash(rel), "/")
	for _, p := range parts {
		if p == "" || p == "." || p == ".." {
			return ModuleKey{}, fmt.Errorf("invalid path segment in %q", rel)
		}
	}
	return NewKey(parts...), nil
}

// Name returns the module name — the last segment.
func (k ModuleKey) Name() string {
	if len(k.segs) == 0 {
		return ""
	}
	return k.segs[len(k.segs)-1]
}

// DirSegs returns the directory path segments — all but the last.
func (k ModuleKey) DirSegs() []string {
	if len(k.segs) <= 1 {
		return nil
	}
	cp := make([]string, len(k.segs)-1)
	copy(cp, k.segs[:len(k.segs)-1])
	return cp
}

// Segs returns all path segments as a new slice.
func (k ModuleKey) Segs() []string {
	cp := make([]string, len(k.segs))
	copy(cp, k.segs)
	return cp
}

// String returns the dot-joined import path, e.g. "Collections.Drawing".
func (k ModuleKey) String() string {
	return strings.Join(k.segs, ".")
}

// Equal reports whether k and other refer to the same module.
func (k ModuleKey) Equal(other ModuleKey) bool {
	return k.String() == other.String()
}

// IsZero reports whether the key is the zero value (no segments).
func (k ModuleKey) IsZero() bool {
	return len(k.segs) == 0
}

// ToFilePath returns the filesystem path for this key under root.
//
//	NewKey("Collections", "Drawing").ToFilePath("/src") → "/src/Collections/Drawing.obx"
func (k ModuleKey) ToFilePath(root string) string {
	parts := make([]string, 0, len(k.segs)+1)
	parts = append(parts, root)
	parts = append(parts, k.segs...)
	return filepath.Join(parts...) + ".obx"
}
