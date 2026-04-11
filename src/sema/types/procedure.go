package types

import (
	"fmt"
	"strings"
)

type ProcedureType struct {
	Params          []*FormalParam // parameter types only; use signature binding for names/kinds
	Result          Type           // nil if PROCEDURE is a command (no result)
	IsTypeBoundType bool
}

func NewProcedureType(param []*FormalParam, res Type) *ProcedureType {
	return &ProcedureType{Params: param, Result: res}
}

func (p *ProcedureType) String() string {
	var params []string
	for _, fp := range p.Params {
		var part string
		if fp.Kind != "" && fp.Kind != "value" && fp.Kind != "VALUE" {
			part = fp.Kind + " "
		}
		part += fp.Type.String()
		params = append(params, part)
	}
	sig := fmt.Sprintf("PROCEDURE(%s)", strings.Join(params, ", "))
	if p.Result != nil {
		sig += ": " + p.Result.String()
	}
	return sig
}

func (p *ProcedureType) Alignment() int { return 8 }

func (p *ProcedureType) Width() int { return 8 }

func (p *ProcedureType) Equals(other Type) bool {
	o, ok := other.(*ProcedureType)
	if !ok || len(p.Params) != len(o.Params) {
		return false
	}
	for i := range p.Params {
		if p.Params[i].Kind != o.Params[i].Kind {
			return false
		}
		if !EqualType(p.Params[i].Type, o.Params[i].Type) {
			return false
		}
	}
	return ResultTypeMatch(p.Result, o.Result)
}

type FormalParam struct {
	Kind string // VAR, IN, OUT, value
	Name string
	Type Type
}
