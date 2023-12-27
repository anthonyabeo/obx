package ast

type Module struct {
	BeginName, EndName *Ident
	ImportList         []*Import
	DeclSeq            []Declaration
	StmtSeq            []Statement
}
