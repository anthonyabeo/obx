package ast

// OberonX is the top-level representation of an OberonX+ program
// It is modelled as directed-acyclic graph where the nodes in the graph
// are compilation units (Module and Definition) and the edges between
// them represent their import dependency.
type OberonX struct {
	Units map[string]CompilationUnit
}

func NewOberonX() *OberonX {
	return &OberonX{Units: make(map[string]CompilationUnit)}
}

func (obx *OberonX) AddUnit(name string, node CompilationUnit) {
	if _, exist := obx.Units[name]; exist {
		return
	}

	obx.Units[name] = node
}

func (obx *OberonX) Lookup(name string) (CompilationUnit, bool) {
	unit, ok := obx.Units[name]
	return unit, ok
}

//func (obx *OberonX) AddEdge(src, dst string) {
//	if _, ok := obx.Units[src]; !ok {
//		return
//	}
//	if _, ok := obx.Units[dst]; !ok {
//		return
//	}
//
//	obx.Units[src].addEdge(dst, obx.Units[dst])
//}
//
//func (obx *OberonX) Neighbors(src string) []string {
//	var result []string
//
//	for _, edge := range obx.Units[src].Edges() {
//		result = append(result, edge.Name())
//	}
//
//	return result
//}
