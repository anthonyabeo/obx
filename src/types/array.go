package types

import (
	"fmt"
	"strings"
)

type ArrayType struct {
	Length     int64
	Dimensions []int64
	Elem       Type
	ElemSize   int
}

func (a *ArrayType) String() string {
	if a.IsOpen() {
		return fmt.Sprintf("ARRAY OF %s", a.Elem.String())
	}

	var dims []string
	for _, dim := range a.Dimensions {
		dims = append(dims, fmt.Sprintf("%d", dim))
	}

	return fmt.Sprintf("ARRAY [%s] OF %s", strings.Join(dims, ", "), a.Elem.String())

}

func (a *ArrayType) Width() int {
	if a.IsOpen() {
		return -1 // Open array, width is not defined
	}

	baseWidth := a.Elem.Width()
	if baseWidth == -1 {
		return -1 // If base type has undefined width, return -1
	}

	// Use all dimensions to calculate total width
	total := int64(1)
	for _, dim := range a.Dimensions {
		if dim == -1 {
			return -1 // Open dimension, width is not defined
		}
		total *= dim
	}
	return int(total) * baseWidth
}

func (a *ArrayType) Alignment() int {
	if a.Dimensions[0] == -1 {
		return 1 // Open arrays typically have an alignment of 1
	}

	baseAlignment := a.Elem.Alignment()
	if baseAlignment == -1 {
		return -1 // If base type has undefined alignment, return -1
	}
	return baseAlignment
}

func (a *ArrayType) Equals(other Type) bool {
	o, ok := other.(*ArrayType)
	if !ok {
		return false
	}
	return fmt.Sprintf("%v", a.Dimensions) == fmt.Sprintf("%v", o.Dimensions) && a.Elem.Equals(o.Elem)
}

func (a *ArrayType) IsOpen() bool {
	return len(a.Dimensions) == 1 && a.Dimensions[0] == -1
}
