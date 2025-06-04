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

func (s StringType) Equals(t Type) bool {
	//TODO implement me
	panic("implement me")
}
