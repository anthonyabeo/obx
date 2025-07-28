package hir

type Module struct {
	Name    string
	Imports []Import
	Decls   []Decl
	//Procedures []*ProcedureDecl
	Init *ProcedureDecl
}

type Import struct {
	Path  string
	Alias string
}

type Program struct {
	Modules []*Module
}
