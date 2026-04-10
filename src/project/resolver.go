package project

import (
	"fmt"
	"os"
)

// Resolver maps ModuleKeys to filesystem paths by searching an ordered list
// of source-tree roots.  Roots are searched in declaration order; the first
// root that contains the expected file wins.
type Resolver struct {
	Roots []string
}

// NewResolver creates a Resolver that searches the given roots.
func NewResolver(roots ...string) *Resolver {
	return &Resolver{Roots: roots}
}

// Resolve returns the absolute path of the .obx file for key, or an error if
// the file is not found under any root.
func (r *Resolver) Resolve(key ModuleKey) (string, error) {
	for _, root := range r.Roots {
		path := key.ToFilePath(root)
		if _, err := os.Stat(path); err == nil {
			return path, nil
		}
	}
	return "", fmt.Errorf("module %q not found in roots %v", key, r.Roots)
}

// DiscoverAll walks every root, scans each .obx file, and returns the
// combined set of headers.  Duplicate keys (same module found in multiple
// roots) are resolved in favour of the first root.
func (r *Resolver) DiscoverAll() ([]Header, error) {
	seen := make(map[string]bool)
	var headers []Header

	for _, root := range r.Roots {
		hdrs, err := DiscoverAndScan(root)
		if err != nil {
			return nil, fmt.Errorf("root %q: %w", root, err)
		}
		for _, h := range hdrs {
			k := h.Key.String()
			if !seen[k] {
				seen[k] = true
				headers = append(headers, h)
			}
		}
	}

	return headers, nil
}

