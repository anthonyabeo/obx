package types

type EnumType struct {
	Variants map[int]string
}

func NewEnumWithVariants(variants ...string) *EnumType {
	elems := make(map[int]string)
	for i, v := range variants {
		elems[i] = v
	}

	return &EnumType{Variants: elems}
}

func (e EnumType) String() string {
	//TODO implement me
	panic("implement me")
}

func (e EnumType) Equals(t Type) bool {
	//TODO implement me
	panic("implement me")
}
