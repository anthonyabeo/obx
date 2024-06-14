package ir

import (
	"container/list"
	"fmt"
	"math"
	"strconv"
)

// Constant ...
// ----------------
type Constant interface {
	User
}

// ConstantInt ...
// --------------------
type ConstantInt struct {
	value  uint64
	signed bool
	ty     Type
	name   string

	useList *list.List
}

func (c ConstantInt) SetOperand(int, Value)   {}
func (c ConstantInt) AddUse(v Value)          { c.useList.PushBack(v) }
func (c ConstantInt) NumOperands() int        { return c.useList.Len() }
func (c ConstantInt) Operand(int) Value       { panic("[internal] int constant has no operands") }
func (c ConstantInt) OperandList() *list.List { return c.useList }
func (c ConstantInt) NumUses() int            { return c.useList.Len() }
func (c ConstantInt) String() string          { return fmt.Sprintf("%s %d", c.Type(), c.value) }
func (c ConstantInt) Type() Type              { return c.ty }
func (c ConstantInt) Name() string            { return c.name }
func (c ConstantInt) SetName(name string)     { c.name = name }
func (c ConstantInt) HasName() bool           { return c.name != "" }

func NewConstantInt(ty Type, value uint64, signed bool, name string) *ConstantInt {
	if name == "" {
		name = strconv.Itoa(int(value))
	}

	uses := list.New()
	uses.Init()

	return &ConstantInt{value, signed, ty, name, uses}
}

func GetNullValue(ty Type) Value {
	switch {
	case ty.IsIntegerTy():
		return NewConstantInt(ty, 0, false, "")
	default:
		panic(fmt.Sprintf("[internal] invalid IR type '%s'", ty))
	}
}

func GetAllOnesValue(ty Type) Value {
	switch {
	case ty.IsIntegerTy():
		bw := ty.(IntegerType).BitWidth()
		allOnes := uint64(math.Pow(2, float64(bw)) - 1.0)

		return NewConstantInt(ty, allOnes, false, "")
	default:
		panic(fmt.Sprintf("[internal] invalid IR type '%s'", ty))
	}
}
