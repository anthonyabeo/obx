package mir

// Program ...
type Program struct {
	Modules []*Module
}

// Module is the high level compilation unit
type Module struct {
	Name       string
	IsEntry    bool
	Imports    []Import
	Globals    []Decl
	Procedures []*Procedure
	Init       *Procedure
}

type Import struct {
	Path  string // e.g. "Math"
	Alias string // e.g. "M" for "IMPORT M := Math"
}
