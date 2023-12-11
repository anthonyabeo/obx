package ast

import "fmt"

type VarDecl struct {
	IdentList []*Ident
	Type      Expression
}

func (v *VarDecl) decl() {}

func (v *VarDecl) String() string {
	return fmt.Sprintf("")
}
