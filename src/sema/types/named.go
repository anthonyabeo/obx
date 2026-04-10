package types

type NamedType struct {
	Name string
	Def  Type // underlying type (e.g. RecordType)
}

func (n *NamedType) String() string {
	return n.Name
}

func (n *NamedType) Width() int { return n.Def.Width() }

func (n *NamedType) Alignment() int { return n.Def.Alignment() }

func (n *NamedType) Equals(other Type) bool {
	if o, ok := other.(*NamedType); ok {
		return n.Name == o.Name // compare by name
	}
	return false
}
