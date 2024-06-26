package sema

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// An Array represents an array type.
type Array struct {
	LenList *ast.LenList
	ElemTy  types.Type
}

func NewArray(elem types.Type, len *ast.LenList) *Array {
	return &Array{LenList: len, ElemTy: elem}
}

func (a *Array) Underlying() types.Type { return a }

func (a *Array) String() string {
	var ll []string
	for _, l := range a.LenList.List {
		ll = append(ll, l.String())
	}

	return fmt.Sprintf("[%s]%s", strings.Join(ll, ", "), a.ElemTy)
}

func (a *Array) Width() int {
	w := a.ElemTy.Width()

	for i := len(a.LenList.List) - 1; i >= 0; i-- {
		idx, _ := strconv.Atoi(a.LenList.List[i].(*ast.BasicLit).Val)
		w *= idx
	}

	return w
}

func (a *Array) IsOpen() bool {
	return a.LenList == nil
}
