package lir

import "fmt"

type (
	IntegerConst struct {
		Value  uint64
		Signed bool
	}

	RealConst struct {
		Value float64
	}
)

func (IntegerConst) isOperand() {}
func (RealConst) isOperand()    {}

func (IntegerConst) isConst() {}
func (RealConst) isConst()    {}

func (i IntegerConst) String() string { return fmt.Sprintf("$%d", i.Value) }
func (r RealConst) String() string    { return fmt.Sprintf("$%f", r) }
