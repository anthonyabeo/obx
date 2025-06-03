package ast

// OberonX is the top-level representation of an OberonX+ program
// It is modelled as directed-acyclic graph where the nodes in the graph
// are compilation units (Module and Definition) and the edges between
// them represent their import dependency.
type OberonX struct {
	//Units map[string]CompilationUnit
	Units []CompilationUnit
}

func NewOberonX() *OberonX {
	return &OberonX{Units: make([]CompilationUnit, 0)}
}

func (obx *OberonX) AddUnit(node CompilationUnit) {
	obx.Units = append(obx.Units, node)
}

func (obx *OberonX) Lookup(name string) (CompilationUnit, bool) {
	for _, unit := range obx.Units {
		if unit.Name() == name {
			return unit, true
		}
	}

	return nil, false
}
