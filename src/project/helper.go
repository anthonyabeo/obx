package project

import "fmt"

// ReachableFrom filters the topologically-sorted header list `sorted` to the
// given `entry` module and its transitive dependencies, preserving the order.
// The `entry` parameter must be the full module key string (ModuleKey.String()).
// If `entry` is empty the original `sorted` slice is returned. If `entry` is
// non-empty but not found in `graph.Headers`, an error is returned.
func ReachableFrom(sorted []Header, graph *ImportGraph, entry string) ([]Header, error) {
	if entry == "" {
		return sorted, nil
	}
	if graph == nil || graph.Headers == nil {
		return nil, fmt.Errorf("import graph is nil")
	}
	if _, ok := graph.Headers[entry]; !ok {
		return nil, fmt.Errorf("entry module %q not found", entry)
	}

	reachable := make(map[string]bool)
	var walk func(string)
	walk = func(key string) {
		if reachable[key] {
			return
		}
		reachable[key] = true
		for _, dep := range graph.Adj[key] {
			walk(dep)
		}
	}
	walk(entry)

	out := make([]Header, 0, len(reachable))
	for _, h := range sorted {
		if reachable[h.Key.String()] {
			out = append(out, h)
		}
	}
	return out, nil
}
