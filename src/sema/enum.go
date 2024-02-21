package sema

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/sema/types"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type Enum struct {
	variants []*ast.Ident
}

func NewEnumType(variants []*ast.Ident) *Enum {
	return &Enum{variants}
}

func (e *Enum) Underlying() types.Type { return e }

func (e *Enum) String() string {
	var list []string
	for _, c := range e.variants {
		list = append(list, c.Name)
	}

	return fmt.Sprintf("(%v)", strings.Join(list, ", "))
}

func (e *Enum) Width() int { panic("not implemented") }
