package project

import "fmt"

// BuildImportGraph constructs a dependency graph from a pre-scanned slice of
// headers.  Each import is resolved against the header set by ModuleKey; an
// error is returned for missing modules, self-imports, and duplicate keys.
func BuildImportGraph(headers []Header) (*ImportGraph, error) {
	graph := &ImportGraph{
		Headers: make(map[string]Header),
		Adj:     make(map[string][]string),
	}

	// Index every header by its key string.
	for _, h := range headers {
		k := h.Key.String()
		if _, dup := graph.Headers[k]; dup {
			return nil, fmt.Errorf("duplicate module %q (files: %s and %s)",
				k, graph.Headers[k].File, h.File)
		}
		graph.Headers[k] = h
	}

	// Build the adjacency list.
	for k, h := range graph.Headers {
		for _, imp := range h.Imports {
			depKey := imp.Key.String()

			if depKey == k {
				return nil, fmt.Errorf("module '%s' cannot import itself (in file %s)", k, h.File)
			}

			if _, ok := graph.Headers[depKey]; !ok {
				return nil, fmt.Errorf("module %q (in %s) imports %q, which was not found",
					k, h.File, depKey)
			}

			graph.Adj[k] = append(graph.Adj[k], depKey)
		}
	}

	return graph, nil
}
