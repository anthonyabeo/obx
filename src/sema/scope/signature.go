package types

import "github.com/anthonyabeo/obx/src/syntax/ast"

type Signature struct {
	rcv    *ast.Receiver
	fp     *ast.FormalParams
	Params []DesugParam
}

func NewSignature(rcv *ast.Receiver, params *ast.FormalParams) (sig *Signature) {
	sig = &Signature{rcv: rcv, fp: params}
	sig.DeSugarParams()
	return
}

func (t *Signature) ReturnType() ast.Type {
	return t.fp.RetType
}

func (t *Signature) NumParams() int {
	return len(t.Params)
}

func (t *Signature) DeSugarParams() {
	for _, FP := range t.fp.Params {
		for _, name := range FP.Names {
			t.Params = append(t.Params, DesugParam{Name: name, Type: FP.Type})
		}
	}
}
func (t *Signature) Underlying() Type { return t }
func (t *Signature) String() string   { return "" }
func (t *Signature) Width() int       { return 8 }

// DesugParam
// ----------------
type DesugParam struct {
	Name string
	Type ast.Type
}
