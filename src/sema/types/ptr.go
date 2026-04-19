package types

import "fmt"

type PointerType struct {
	Base Type // should usually be a RecordType
}

func (p *PointerType) String() string {
	return fmt.Sprintf("POINTER TO %s", p.Base.String())
}

func (p *PointerType) Width() int { return 8 }

func (p *PointerType) Alignment() int { return 8 }

func (p *PointerType) Equals(other Type) bool {
	o, ok := other.(*PointerType)
	if !ok {
		return false
	}
	return p.Base.Equals(o.Base)
}

type CPointer struct {
	Base Type
}

func (p *CPointer) String() string {
	return fmt.Sprintf("CPointer(%s)", p.Base.String())
}

func (p *CPointer) Width() int { return 8 }

func (p *CPointer) Alignment() int { return 8 }

func (p *CPointer) Equals(other Type) bool {
	o, ok := other.(*CPointer)
	if !ok {
		return false
	}
	return p.Base.Equals(o.Base)
}
