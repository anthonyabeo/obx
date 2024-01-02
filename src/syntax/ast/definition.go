package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type Definition struct {
	Def        *token.Position
	BeginName  *Ident
	EndName    *Ident
	ImportList []*Import
	DeclSeq    []Declaration
}
