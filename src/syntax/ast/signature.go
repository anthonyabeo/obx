package ast

import "github.com/anthonyabeo/obx/src/sema/types"

type Signature struct {
	rcv    *Receiver
	fp     *FormalParams
	Params []DesugParam
}

func NewSignature(rcv *Receiver, params *FormalParams) (sig *Signature) {
	sig = &Signature{rcv: rcv, fp: params}
	sig.DeSugarParams()
	return
}

func (t *Signature) ReturnType() Type {
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
func (t *Signature) Underlying() types.Type { return t }
func (t *Signature) String() string         { return "" }
func (t *Signature) Width() int             { panic("not implemented") }

// DesugParam
// ----------------
type DesugParam struct {
	Name *Ident
	Type Type
}
