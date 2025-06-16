package types

import "fmt"

type ArrayType struct {
	Length int64
	Base   Type
}

func (a *ArrayType) String() string {
	if a.Length == -1 {
		return fmt.Sprintf("ARRAY OF %s", a.Base.String())
	}

	return fmt.Sprintf("ARRAY %d OF %s", a.Length, a.Base.String())

}

func (a *ArrayType) Width() int {
	if a.Length == -1 {
		return -1 // Open array, width is not defined
	}

	baseWidth := a.Base.Width()
	if baseWidth == -1 {
		return -1 // If base type has undefined width, return -1
	}
	return int(a.Length) * baseWidth
}

func (a *ArrayType) Alignment() int {
	panic("Not implemented")
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
