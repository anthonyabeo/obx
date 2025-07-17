package hir

type Module struct {
	Name       string
	Imports    []Import
	Globals    []Decl
	Procedures []*ProcedureDecl
	Init       *ProcedureDecl
}

type Import struct {
	Path  string
	Alias string
}
