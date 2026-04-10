package modgraph

import (
	"fmt"
	"strings"
)

// TopoSort returns the headers of graph in topological order (dependencies
// before dependents).  It returns an error if a cycle is detected.
func TopoSort(graph *ImportGraph) ([]Header, error) {
	visited := make(map[string]bool)
	temp := make(map[string]bool)
	var order []Header
	var stack []string // DFS path for cycle reporting

	var visit func(key string) error
	visit = func(key string) error {
		if temp[key] {
			// Build a human-readable cycle description.
			var cycle []string
			for i := len(stack) - 1; i >= 0; i-- {
				mod := stack[i]
				h := graph.Headers[mod]
				cycle = append([]string{fmt.Sprintf("%s (%s)", h.Key, h.File)}, cycle...)
				if mod == key {
					break
				}
			}
			h := graph.Headers[key]
			cycle = append(cycle, fmt.Sprintf("%s (%s)", h.Key, h.File))
			return fmt.Errorf("import cycle detected:\n  %s", strings.Join(cycle, "\n  -> "))
		}
		if visited[key] {
			return nil
		}

		temp[key] = true
		stack = append(stack, key)
		for _, dep := range graph.Adj[key] {
			if err := visit(dep); err != nil {
				return err
			}
		}
		stack = stack[:len(stack)-1]
		temp[key] = false
		visited[key] = true

		order = append(order, graph.Headers[key])
		return nil
	}

	for key := range graph.Headers {
		if !visited[key] {
			if err := visit(key); err != nil {
				return nil, err
			}
		}
	}

	return order, nil
}
