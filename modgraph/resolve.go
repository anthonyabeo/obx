package modgraph

import (
	"fmt"
	"path/filepath"
	"strings"
)

func BuildImportGraph(rootDir string, headers []Header) (*ImportGraph, error) {
	graph := &ImportGraph{
		Headers: make(map[ModuleID]Header),
		Adj:     make(map[ModuleID][]ModuleID),
	}

	// Step 1: Index all modules by their ModuleID
	for _, header := range headers {
		path, err := extractPathFromFile(rootDir, header.File)
		if err != nil {
			return nil, err
		}

		id := ModuleID{
			Path: path,
			Name: header.ID.Name,
		}
		graph.Headers[id] = header
	}

	// Step 2: Link each modgraph to its dependencies
	for modID, header := range graph.Headers {
		for _, imp := range header.Imports {
			var target ModuleID

			// If path is not given, inherit from current modgraph's path (relative import)
			if imp.ID.Path == "" {
				target = ModuleID{Path: modID.Path, Name: imp.ID.Name}
			} else {
				target = ModuleID{Path: imp.ID.Path, Name: imp.ID.Name}
			}

			if modID == target {
				return nil, fmt.Errorf("modgraph %s cannot import itself", modID.String())
			}

			// Validate that the imported modgraph actually exists
			if _, ok := graph.Headers[target]; !ok {
				return nil, fmt.Errorf("imported modgraph %s not found (imported in %s)", target.String(), modID.String())
			}

			graph.Adj[modID] = append(graph.Adj[modID], target)
		}
	}

	return graph, nil
}

// root is the base directory containing all modules.
func extractPathFromFile(root, absFilePath string) (string, error) {
	// Ensure the file has a .mod extension
	if !strings.HasSuffix(absFilePath, ".obx") {
		return "", fmt.Errorf("invalid modgraph file (expected .mod): %s", absFilePath)
	}

	// Make paths absolute and clean
	root = filepath.Clean(root)
	absFilePath = filepath.Clean(absFilePath)

	// Make absFilePath relative to root
	relPath, err := filepath.Rel(root, absFilePath)
	if err != nil {
		return "", fmt.Errorf("file %q is not under root %q: %w", absFilePath, root, err)
	}

	// Strip the .mod suffix
	relPath = strings.TrimSuffix(relPath, ".obx")

	// Normalize to slash-separated import path (regardless of OS)
	importPath := filepath.ToSlash(relPath)

	return importPath, nil
}
