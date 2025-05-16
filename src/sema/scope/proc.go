package types

import (
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type ProcedureType struct {
	Proc   *token.Position
	fp     *ast.FormalParams
	Params []*DesugParam
}

func (p *ProcedureType) DeSugarParams() {
	for _, FP := range p.fp.Params {
		for _, name := range FP.Names {
			p.Params = append(p.Params, &DesugParam{Name: name, Type: FP.Type})
		}
	}
}

func (p *ProcedureType) String() string   { return p.fp.String() }
func (p *ProcedureType) Underlying() Type { return p }
func (p *ProcedureType) Width() int       { return 8 }
