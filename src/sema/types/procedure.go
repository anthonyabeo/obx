package types

type ProcedureType struct {
	Params          []*FormalParam // parameter types only; use signature binding for names/kinds
	Result          Type           // nil if PROCEDURE is a command (no result)
	IsTypeBoundType bool
}

func NewProcedureType(param []*FormalParam, res Type) *ProcedureType {
	return &ProcedureType{Params: param, Result: res}
}

func (p *ProcedureType) String() string {
	//var params []string
	//for _, t := range p.Params {
	//	params = append(params, t.String())
	//}
	//var results []string
	//for _, t := range p.Results {
	//	results = append(results, t.String())
	//}
	//
	//sig := fmt.Sprintf("PROCEDURE(%s)", strings.Join(params, ", "))
	//if len(results) > 0 {
	//	sig += " : " + strings.Join(results, ", ")
	//}
	//return sig
	return ""
}

func (p *ProcedureType) Alignment() int { return 8 }

func (p *ProcedureType) Width() int {
	return 8
}

func (p *ProcedureType) Equals(other Type) bool {
	//o, ok := other.(*ProcedureType)
	//if !ok || len(p.Params) != len(o.Params) || len(p.Results) != len(o.Results) {
	//	return false
	//}
	//for i := range p.Params {
	//	if !p.Params[i].Equals(o.Params[i]) {
	//		return false
	//	}
	//}
	//for i := range p.Results {
	//	if !p.Results[i].Equals(o.Results[i]) {
	//		return false
	//	}
	//}
	return true
}

type FormalParam struct {
	Kind string // VAR, IN, value
	Name string
	Type Type
}
