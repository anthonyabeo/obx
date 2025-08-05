package lir

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/mir"
)

type (
	// Register represents a temporary variable in the LIR. These correspond to
	// registers in the target architecture.
	Register struct {
		Name string // e.g., "t1", "r3", etc.
		Type mir.Type
	}

	// Mem represents a memory location, starting at address 'BaseAddr'.
	// It can be used to represent global variables, stack variables,
	// array/record locations, etc.
	Mem struct {
		BaseAddr Operand
	}

	IntegerConst struct {
		Value  uint64
		Signed bool
		Bits   uint
		Typ    mir.Type
	}

	// RealConst represents a constant floating-point value.
	RealConst struct {
		Value float64
		Bits  uint
		Typ   mir.Type
	}
)

func (*Label) isOperand()       {}
func (*Mem) isOperand()         {}
func (*Register) isOperand()    {}
func (IntegerConst) isOperand() {}
func (RealConst) isOperand()    {}

func (o *Label) Children() []Node     { return []Node{} }
func (o *Mem) Children() []Node       { return []Node{o.BaseAddr} }
func (o *Register) Children() []Node  { return []Node{} }
func (IntegerConst) Children() []Node { return []Node{} }
func (RealConst) Children() []Node    { return []Node{} }

// func (o *Label) String() string       { return "%" + o.Name }
func (o *Mem) String() string         { return fmt.Sprintf("[%s]", o.BaseAddr.String()) }
func (o *Register) String() string    { return "$" + o.Name }
func (o IntegerConst) String() string { return fmt.Sprintf("$%d", o.Value) }
func (o RealConst) String() string    { return fmt.Sprintf("$%f", o.Value) }
