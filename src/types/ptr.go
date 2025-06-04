package types

import "fmt"

type PointerType struct {
	Base Type // should usually be a RecordType
}

func (p *PointerType) String() string {
	return fmt.Sprintf("POINTER TO %s", p.Base.String())
}

func (p *PointerType) Equals(other Type) bool {
	o, ok := other.(*PointerType)
	if !ok {
		return false
	}
	return p.Base.Equals(o.Base)
}
