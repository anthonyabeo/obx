package types

import "fmt"

type StringType struct {
	Length int
}

func NewStringType(len int) *StringType {
	return &StringType{Length: len}
}

func (s StringType) String() string {
	return fmt.Sprintf("StringType(%d)", s.Length)
}

func (s StringType) Width() int {
	panic("Not implemented")
}

func (s StringType) Alignment() int {
	panic("Not implemented")
}

func (s StringType) Equals(t Type) bool {
	if other, ok := t.(*StringType); ok {
		return s.Length == other.Length
	}
	return false
}
