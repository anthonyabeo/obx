package types

import "fmt"

type ArrayType struct {
	Length int
	Base   Type
}

func (a *ArrayType) String() string {
	return fmt.Sprintf("ARRAY %d OF %s", a.Length, a.Base.String())
}

func (a *ArrayType) Equals(other Type) bool {
	o, ok := other.(*ArrayType)
	if !ok {
		return false
	}
	return a.Length == o.Length && a.Base.Equals(o.Base)
}

func (a *ArrayType) IsOpen() bool {
	return a.Length == -1
}
