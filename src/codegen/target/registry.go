package target

import (
	"fmt"
	"sort"
	"strings"
)

var registry = map[string]func() Machine{}

// Register makes a target available by name. Intended to be called from
// each target package's init() function.
func Register(name string, factory func() Machine) {
	registry[name] = factory
}

// Lookup returns a freshly constructed Machine for the given target name.
// Returns an error that lists all registered targets when name is unknown.
func Lookup(name string) (Machine, error) {
	f, ok := registry[name]
	if !ok {
		return nil, fmt.Errorf("unknown target %q; available: %s",
			name, strings.Join(Available(), ", "))
	}
	return f(), nil
}

// Available returns the names of all registered targets in sorted order.
func Available() []string {
	names := make([]string, 0, len(registry))
	for k := range registry {
		names = append(names, k)
	}
	sort.Strings(names)
	return names
}
