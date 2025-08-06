package hir

type Module struct {
	Name    string
	IsEntry bool
	Decls   []Decl
	Init    *Function
}

type Program struct {
	Modules []*Module
}
