package ast

type Definition struct {
	BeginName, EndName *Ident
	//ImportList []*Import
	DeclSeq []Declaration
}
