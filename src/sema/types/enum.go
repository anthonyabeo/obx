package types

import (
	"fmt"
	"strings"
)

type Enum struct {
	variants map[string]int
}

func NewEnumType(variants map[string]int) *Enum {
	return &Enum{variants}
}

func (e *Enum) Underlying() Type { return e }

func (e *Enum) String() string {
	var list []string
	for name := range e.variants {
		list = append(list, name)
	}

	return fmt.Sprintf("(%v)", strings.Join(list, ", "))
}

func (e *Enum) Width() int { return 4 }

func (e *Enum) SameAs(other *Enum) bool {
	for name := range e.variants {
		if _, ok := other.variants[name]; !ok {
			return false
		}
	}

	return true
}

func (e *Enum) IsValidOrd(i int) bool {
	for _, ord := range e.variants {
		if ord == i {
			return true
		}
	}

	return false
}
