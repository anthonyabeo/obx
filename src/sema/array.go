package sema

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// An Array represents an array type.
type Array struct {
	len  ast.Expression
	elem types.Type
}

func NewArray(elem types.Type, len ast.Expression) *Array { return &Array{len: len, elem: elem} }

func (a *Array) Len() ast.Expression { return a.len }

func (a *Array) Elem() types.Type { return a.elem }

func (a *Array) Underlying() types.Type { return a }

func (a *Array) String() string { return "" }
