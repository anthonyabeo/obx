package minir

import (
	"fmt"
	"sort"
)

// SymbolTable is a flat name → Value map used at both module scope (globals)
// and function scope (local SSA temporaries).  It preserves insertion order
// and provides sorted access via Names() for deterministic output.
type SymbolTable struct {
	table map[string]Value
	order []string // insertion-order record; used by Names()
}

// Define inserts the binding name → v.
// Returns a non-nil error when name is already present in the table.
func (s *SymbolTable) Define(name string, v Value) error {
	if s.table == nil {
		s.table = make(map[string]Value)
	}
	if _, exists := s.table[name]; exists {
		return fmt.Errorf("symbol %q already defined", name)
	}
	s.table[name] = v
	s.order = append(s.order, name)
	return nil
}

// Lookup returns the Value bound to name, or (nil, false) if not present.
func (s *SymbolTable) Lookup(name string) (Value, bool) {
	if s.table == nil {
		return nil, false
	}
	v, ok := s.table[name]
	return v, ok
}

// Names returns all defined names in sorted (alphabetical) order.
func (s *SymbolTable) Names() []string {
	if len(s.table) == 0 {
		return nil
	}
	names := make([]string, len(s.table))
	copy(names, s.order)
	sort.Strings(names)
	return names
}

// Len returns the number of symbols defined in the table.
func (s *SymbolTable) Len() int { return len(s.table) }

