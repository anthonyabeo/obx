package types

import "fmt"

type Type interface {
	fmt.Stringer
	Underlying() Type
	Width() int // number of bytes (8-bit) this type requires
}

func NewGenericType(name string, tyConst Type) *Generic {
	return &Generic{
		Name:    name,
		TyConst: tyConst,
	}
}

type Generic struct {
	Name    string
	TyConst Type
}

func (g Generic) String() string   { return g.Name }
func (g Generic) Underlying() Type { return g }
func (g Generic) Width() int       { return 8 }
