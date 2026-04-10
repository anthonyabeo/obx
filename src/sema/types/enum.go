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

func (e EnumType) GetVariant(pos int) string {
	var variant string
	for name, ord := range e.Variants {
		if ord == pos {
			variant = name
		}
	}

	return variant
}

func (e EnumType) String() string {
	var parts []string
	for name := range e.Variants {
		parts = append(parts, name)
	}
	return "ENUM " + strings.Join(parts, ", ") + " END"
}

func (e EnumType) Width() int {
	return len(e.Variants) * 4
}

func (e EnumType) Alignment() int { return 4 }

func (e EnumType) Equals(t Type) bool {
	//TODO implement me
	panic("implement me")
}
