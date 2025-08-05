package hir

type Module struct {
	Name    string
	IsEntry bool
	Imports []Import
	Decls   []Decl
	Init    *ProcedureDecl
}

type Import struct {
	Path  string
	Alias string
}

type Program struct {
	Modules []*Module
}
