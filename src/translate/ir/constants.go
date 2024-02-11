package ir

import (
	"fmt"
	"strconv"
)

// Constant ...
// ----------------
type Constant interface {
	Value
}

// ConstantInt ...
// --------------------
type ConstantInt struct {
	value  uint64
	signed bool
	ty     Type
	name   string
}

func (c ConstantInt) String() string      { return fmt.Sprintf("%s %d", c.Type(), c.value) }
func (c ConstantInt) Type() Type          { return c.ty }
func (c ConstantInt) Name() string        { return c.name }
func (c ConstantInt) SetName(name string) { c.name = name }
func (c ConstantInt) HasName() bool       { return c.name != "" }

func NewConstantInt(ty Type, value uint64, signed bool, name string) *ConstantInt {
	if name == "" {
		name = strconv.Itoa(int(value))
	}

	return &ConstantInt{value, signed, ty, name}
}
