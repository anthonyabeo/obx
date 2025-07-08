package hir

type Module struct {
	Name       string
	Imports    []Import
	Globals    []Decl
	Procedures []*Procedure
	Init       *Procedure
}

type Import struct {
	Path        string
	Alias       string
	StartOffset int
	EndOffset   int
}
