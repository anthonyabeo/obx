package modgraph

import (
	"fmt"
	"strings"
)

func TopoSort(graph *ImportGraph) ([]Header, error) {
	visited := make(map[ModuleID]bool)
	temp := make(map[ModuleID]bool)
	var order []Header
	var stack []ModuleID // Tracks DFS path

	var visit func(ModuleID) error
	visit = func(id ModuleID) error {
		if temp[id] {
			// Build cycle description with source file info
			var cycle []string
			//start := -1
			for i := len(stack) - 1; i >= 0; i-- {
				mod := stack[i]
				header := graph.Headers[mod]
				cycle = append([]string{fmt.Sprintf("%s (%s)", mod.String(), header.File)}, cycle...)
				if mod == id {
					//start = i
					break
				}
			}
			// Add the closing edge
			header := graph.Headers[id]
			cycle = append(cycle, fmt.Sprintf("%s (%s)", id.String(), header.File))
			return fmt.Errorf("import cycle detected:\n  %s", strings.Join(cycle, "\n  -> "))
		}
		if visited[id] {
			return nil
		}

		temp[id] = true
		stack = append(stack, id) // push
		for _, dep := range graph.Adj[id] {
			if err := visit(dep); err != nil {
				return err
			}
		}
		stack = stack[:len(stack)-1] // pop
		temp[id] = false
		visited[id] = true

		order = append(order, graph.Headers[id])
		return nil
	}

	for id := range graph.Headers {
		if !visited[id] {
			if err := visit(id); err != nil {
				return nil, err
			}
		}
	}

	return order, nil
}
