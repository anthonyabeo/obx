package ast

import "fmt"

// Oberon is the top-level representation of an Oberon+ program
// It is modelled as directed-acyclic graph where the nodes in the graph
// are compilation units (Module and Definition) and the edges between
// them represent their import dependency.
type Oberon struct {
	program map[string]Unit
}

func NewOberon() *Oberon {
	return &Oberon{program: make(map[string]Unit)}
}

func (obx *Oberon) Module(name string) (*Module, error) {
	m, ok := obx.program[name]
	if !ok {
		return nil, fmt.Errorf("no module named '%s' found", name)
	}

	mod, ok := m.(*Module)
	if !ok {
		return nil, fmt.Errorf("'%s' is not a module", m.Name())
	}

	return mod, nil
}

func (obx *Oberon) Definition(name string) (*Definition, error) {
	m, ok := obx.program[name]
	if !ok {
		return nil, fmt.Errorf("no definition named '%s' found", name)
	}

	def, ok := m.(*Definition)
	if !ok {
		return nil, fmt.Errorf("'%s' is not a definition", m.Name())
	}

	return def, nil
}

func (obx *Oberon) AddUnit(name string, unit Unit) error {
	if u, ok := obx.program[name]; ok {
		if u.Kind() == unit.Kind() {
			return fmt.Errorf("unit with name '%s' already defined", name)
		}
	}

	obx.program[name] = unit

	return nil
}

func (obx *Oberon) Program() map[string]Unit {
	return obx.program
}
