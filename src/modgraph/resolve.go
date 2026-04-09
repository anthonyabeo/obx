package modgraph

import (
	"fmt"
)

func BuildImportGraph(rootDir string, headers []Header) (*ImportGraph, error) {
	graph := &ImportGraph{
		Headers: make(map[ModuleID]Header),
		Adj:     make(map[ModuleID][]ModuleID),
	}

	// Step 1: Index modules by their full ID (Path + Name)
	for _, header := range headers {
		graph.Headers[header.ID] = header
	}

	// Step 2: Build adjacency list from imports
	for modID, header := range graph.Headers {
		for _, imp := range header.Imports {
			if imp.ID == 0 {
				for moduleID, h := range graph.Headers {
					if h.Name == imp.Name {
						imp.ID = moduleID
						break
					}
				}
			}

			// Self-import check
			if modID == imp.ID {
				return nil, fmt.Errorf("module '%s' cannot import itself (in file %s)", header.String(), header.File)
			}

			// Check existence
			if _, ok := graph.Headers[imp.ID]; !ok {
				return nil, fmt.Errorf("module '%s' imports '%s', which was not found",
					header.String(), graph.Headers[imp.ID].String())
			}

			graph.Adj[modID] = append(graph.Adj[modID], imp.ID)
		}
	}

	return graph, nil
}
