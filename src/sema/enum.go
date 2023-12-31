package sema

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type Enum struct {
	consts []*ast.Ident
}

func NewEnumType(consts []*ast.Ident) *Enum {
	return &Enum{consts: consts}
}

func (e *Enum) Underlying() types.Type { return e }

func (e *Enum) String() string {
	var list []string
	for _, c := range e.consts {
		list = append(list, c.Name)
	}

	return fmt.Sprintf("(%v)", strings.Join(list, ", "))
}
