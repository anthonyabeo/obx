package mir

import "fmt"

// Linkage describes the visibility / linkage model for module-level symbols.
type Linkage int

const (
	InternalLinkage Linkage = iota
	ExternalLinkage
	PrivateLinkage
)

func (l Linkage) String() string {
	switch l {
	case ExternalLinkage:
		return "external"
	case PrivateLinkage:
		return "private"
	default:
		return "internal"
	}
}

// Type is a lightweight backend type descriptor carried by operands and
// module-level declarations. It is intentionally target-neutral.
type Type struct {
	Name   string
	Size   int
	Signed bool
	Elem   *Type
	Len    int
}

func NewScalarType(name string, size int) *Type {
	return &Type{Name: name, Size: size}
}

func NewPointerType(elem *Type, size int) *Type {
	return &Type{Name: "ptr", Size: size, Elem: elem}
}

func NewArrayType(elem *Type, length int) *Type {
	return &Type{Name: "array", Elem: elem, Len: length}
}

func (t *Type) String() string {
	if t == nil {
		return "void"
	}
	if t.Len > 0 && t.Elem != nil {
		return fmt.Sprintf("[%d x %s]", t.Len, t.Elem)
	}
	if t.Elem != nil {
		return fmt.Sprintf("*%s", t.Elem)
	}
	if t.Name != "" {
		return t.Name
	}
	if t.Size > 0 {
		return fmt.Sprintf("i%d", t.Size*8)
	}
	return "unknown"
}
