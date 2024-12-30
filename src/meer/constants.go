package meer

import "fmt"

// BoolConst
// -------------
type BoolConst struct {
	value bool
}

func (b *BoolConst) String() string {
	return fmt.Sprintf("%s", b.value)
}

// IntegerConst
// -------------
type IntegerConst struct {
	Value  uint64
	signed bool
}

func (b *IntegerConst) expr() {}
func (b *IntegerConst) String() string {
	return fmt.Sprintf("%d", b.Value)
}

// FloatConst
// -------------
type FloatConst struct {
	value  float64
	signed bool
}

func (b *FloatConst) String() string {
	return fmt.Sprintf("%f", b.value)
}
