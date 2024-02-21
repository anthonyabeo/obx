package sema

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/sema/types"
)

type PtrType struct {
	UTy types.Type
}

func (p PtrType) String() string         { return fmt.Sprintf("^%s", p.UTy) }
func (p PtrType) Underlying() types.Type { return p.UTy }
func (p PtrType) Width() int             { return 8 }
