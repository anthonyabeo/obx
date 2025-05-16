package types

import (
	"fmt"
)

type PtrType struct {
	UTy Type
}

func (p PtrType) String() string   { return fmt.Sprintf("^%s", p.UTy) }
func (p PtrType) Underlying() Type { return p.UTy }
func (p PtrType) Width() int       { return 8 }
