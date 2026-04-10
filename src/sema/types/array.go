package types

import (
	"fmt"
)

type ArrayType struct {
	Length int
	Elem   Type
}

func (a *ArrayType) String() string {
	if a.Length == -1 {
		return fmt.Sprintf("array of %s", a.Elem.String())
	}
	return fmt.Sprintf("array %d of %s", a.Length, a.Elem.String())
}

func (a *ArrayType) Width() int {
	if a.Length == -1 {
		return -1 // Open arrays have undefined width
	}

	baseWidth := a.Elem.Width()
	if baseWidth == -1 {
		return -1 // If base type has undefined width, return -1
	}

	totalWidth := baseWidth * a.Length
	return totalWidth
}

func (a *ArrayType) Alignment() int {
	if a.Length == -1 {
		return 1 // Open arrays have alignment of 1
	}

	baseAlignment := a.Elem.Alignment()
	if baseAlignment == -1 {
		return -1 // If base type has undefined alignment, return -1
	}

	return baseAlignment
}

func (a *ArrayType) Dimensions() []int {
	dims := make([]int, 0)
	if !a.IsOpen() {
		dims = append(dims, a.Length)
	} else {
		dims = append(dims, -1) // Indicating an open array
	}

	if sub, ok := a.Elem.(*ArrayType); ok {
		dims = append(dims, sub.Dimensions()...)
	}

	return dims
}

func (a *ArrayType) Equals(other Type) bool {
	if other == nil {
		return false
	}

	if a == other {
		return true
	}

	if otherArray, ok := other.(*ArrayType); ok {
		if a.Length != otherArray.Length || !a.Elem.Equals(otherArray.Elem) {
			return false
		}
		return true
	}

	return false
}

func (a *ArrayType) IsOpen() bool { return a.Length == -1 }
