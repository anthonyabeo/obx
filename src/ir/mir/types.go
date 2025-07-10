package mir

import "fmt"

type Type interface {
	isMIRType()
	String() string
}

type (
	IntegerType struct {
		Bits   int
		Signed bool
	}
)

func (*IntegerType) isMIRType()       {}
func (i *IntegerType) String() string { return fmt.Sprintf("i%d", i.Bits) }
