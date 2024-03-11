package ast

// Oberon is the top-level representation of an Oberon+ program
// It is modelled as directed-acyclic graph where the nodes in the graph
// are compilation units (Module and Definition) and the edges between
// them represent their import dependency.
type Oberon struct {
	units  map[string]Unit
	TopOrd []string
}

func NewOberon() *Oberon {
	return &Oberon{units: make(map[string]Unit)}
}

func (obx *Oberon) AddUnit(name string, node Unit) {
	if _, exist := obx.units[name]; exist {
		return
	}

	obx.units[name] = node
}

func (obx *Oberon) AddEdge(src, dst string) {
	if _, ok := obx.units[src]; !ok {
		return
	}
	if _, ok := obx.units[dst]; !ok {
		return
	}

	obx.units[src].addEdge(dst, obx.units[dst])
}

func (obx *Oberon) Neighbors(src string) []string {
	var result []string

	for _, edge := range obx.units[src].Edges() {
		result = append(result, edge.Name())
	}

	return result
}

func (obx *Oberon) Units() map[string]Unit {
	return obx.units
}
