package types

import "fmt"

type Type interface {
	fmt.Stringer
	Underlying() Type
	Width() int // number of bytes (8-bit) this type requires
}
