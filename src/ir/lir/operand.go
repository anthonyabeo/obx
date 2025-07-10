package lir

import "fmt"

type (
	Register struct {
		Name string // e.g., "t1", "r3", etc.
		Type Type
	}

	Addr struct {
		Base Operand
	}

	Label struct {
		Name string
	}

	PhiArm struct {
		Pred Label   // predecessor block
		Val  Operand // value from that block
	}

	Var struct {
		Name string
	}
)

func (Label) isOperand()    {}
func (Addr) isOperand()     {}
func (Register) isOperand() {}
func (PhiArm) isOperand()   {}
func (Var) isOperand()      {}

func (l Label) String() string    { return "%" + l.Name }
func (a Addr) String() string     { return a.Base.String() }
func (r Register) String() string { return r.Name }
func (a PhiArm) String() string {
	return fmt.Sprintf("[%s: %s]", a.Pred, a.Val)
}
func (v Var) String() string { return "%" + v.Name }
