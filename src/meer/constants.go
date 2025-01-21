package meer

import "fmt"

// BoolConst
// -------------
type BoolConst struct {
	value bool
	Ty    Type
}

func (b *BoolConst) expr()                    {}
func (b *BoolConst) Type() Type               { return b.Ty }
func (b *BoolConst) SetType(t Type)           { b.Ty = t }
func (b *BoolConst) String() string           { return fmt.Sprintf("%t", b.value) }
func (b *BoolConst) NumOperands() int         { panic("not implemented") }
func (b *BoolConst) Operand(i int) Expression { panic("not implemented") }

// IntegerConst
// -------------
type IntegerConst struct {
	Value  uint64
	signed bool
	Ty     Type
}

func CreateIntegerConst(ty Type, value uint64, signed bool) *IntegerConst {
	return &IntegerConst{
		Value:  value,
		signed: signed,
		Ty:     ty,
	}
}

func (i *IntegerConst) expr()                  {}
func (i *IntegerConst) SetType(t Type)         { i.Ty = t }
func (i *IntegerConst) String() string         { return fmt.Sprintf("%d", i.Value) }
func (i *IntegerConst) Type() Type             { return i.Ty }
func (i *IntegerConst) NumOperands() int       { panic("not implemented") }
func (i *IntegerConst) Operand(int) Expression { panic("not implemented") }

// FloatConst
// -------------
type FloatConst struct {
	value  float64
	signed bool
	Ty     Type
}

func (f *FloatConst) expr()                    {}
func (f *FloatConst) Type() Type               { return f.Ty }
func (f *FloatConst) SetType(t Type)           { f.Ty = t }
func (f *FloatConst) String() string           { return fmt.Sprintf("%f", f.value) }
func (f *FloatConst) NumOperands() int         { panic("not implemented") }
func (f *FloatConst) Operand(i int) Expression { panic("not implemented") }
