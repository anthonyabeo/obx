package modgraph

type ModuleID struct {
	Path string // logical import path (may be empty)
	Name string // name of the modgraph
}

func (id ModuleID) String() string {
	if id.Path != "" {
		return id.Path + "." + id.Name
	}
	return id.Name
}

type Header struct {
	ID      ModuleID
	File    string
	Imports []Import
}

type ImportGraph struct {
	Headers map[ModuleID]Header
	Adj     map[ModuleID][]ModuleID
}

type Import struct {
	Alias string // optional alias
	ID    ModuleID
}
