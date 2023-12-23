package types

import "github.com/anthonyabeo/obx/src/syntax/ast"

// An Array represents an array type.
type Array struct {
	len  ast.Expression
	elem Type
}

// NewArray returns a new array type for the given element type and length.
// A negative length indicates an unknown length.
func NewArray(elem Type, len ast.Expression) *Array { return &Array{len: len, elem: elem} }

// Len returns the length of array a.
// A negative result indicates an unknown length.
func (a *Array) Len() ast.Expression { return a.len }

// Elem returns element type of array a.
func (a *Array) Elem() Type { return a.elem }

func (a *Array) Underlying() Type { return a }

func (a *Array) String() string { return "" }
