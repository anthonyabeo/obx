package types

import "fmt"

type ArrayType struct {
	Length int64
	Elem   Type
}

func (a *ArrayType) String() string {
	if a.Length == -1 {
		return fmt.Sprintf("ARRAY OF %s", a.Elem.String())
	}

	return fmt.Sprintf("ARRAY %d OF %s", a.Length, a.Elem.String())

}

func (a *ArrayType) Width() int {
	if a.Length == -1 {
		return -1 // Open array, width is not defined
	}

	baseWidth := a.Elem.Width()
	if baseWidth == -1 {
		return -1 // If base type has undefined width, return -1
	}
	return int(a.Length) * baseWidth
}

func (a *ArrayType) Dimensions() int {
	base, ok := a.Elem.(*ArrayType)
	if ok {
		return 1 + base.Dimensions()
	}

	return 1 // Base type is not an array, so this is the only dimension
}

func (a *ArrayType) Alignment() int {
	panic("Not implemented")
}

func (a *ArrayType) Equals(other Type) bool {
	o, ok := other.(*ArrayType)
	if !ok {
		return false
	}
	return a.Length == o.Length && a.Elem.Equals(o.Elem)
}

func (a *ArrayType) IsOpen() bool {
	return a.Length == -1
}
