package types

import (
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type Signature struct {
	Rcv    *ast.Receiver
	FP     *ast.FormalParams
	Params []DesugParam
}

func (t *Signature) NumParams() int {
	return len(t.Params)
}

func (t *Signature) DeSugarParams() {
	for _, FP := range t.FP.Params {
		for _, name := range FP.Names {
			t.Params = append(t.Params, DesugParam{Name: name, Type: FP.Type})
		}
	}
}

func (t *Signature) Underlying() Type { return t }
func (t *Signature) String() string   { return "" }

// DesugParam
// ----------------
type DesugParam struct {
	Name *ast.Ident
	Type ast.Expression
}
