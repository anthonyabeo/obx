package types

import "fmt"

type Type interface {
	fmt.Stringer
	Underlying() Type
}
