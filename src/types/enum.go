package types

import "strings"

type EnumType struct {
	Variants map[string]int
}

func NewEnumWithVariants(variants ...string) *EnumType {
	elems := make(map[string]int)
	for i, v := range variants {
		elems[v] = i
	}

	return &EnumType{Variants: elems}
}

func (e EnumType) String() string {
	var parts []string
	for name := range e.Variants {
		parts = append(parts, name)
	}
	return "ENUM " + strings.Join(parts, ", ") + " END"
}

func (e EnumType) Width() int {
	//TODO implement me
	panic("implement me")
}

func (e EnumType) Alignment() int {
	//TODO implement me
	panic("implement me")
}

func (e EnumType) Equals(t Type) bool {
	//TODO implement me
	panic("implement me")
}
