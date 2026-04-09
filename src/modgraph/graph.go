package modgraph

type ModuleID int

var id ModuleID

func nextModID() ModuleID {
	id++
	return id
}

type Header struct {
	ID       ModuleID
	Path     string // logical import path (may be empty)
	Name     string // name of the module
	File     string // name of the file containing the module e.g. math.obx
	StartPos int
	EndPos   int
	Imports  []Import
}

func (id Header) String() string {
	if id.Path != "" {
		return id.Path + "." + id.Name
	}
	return id.Name
}

type ImportGraph struct {
	Headers map[ModuleID]Header
	Adj     map[ModuleID][]ModuleID
}

type Import struct {
	Alias string // optional alias
	Path  string // logical import path (may be empty)
	Name  string // name of the module
	ID    ModuleID
}
