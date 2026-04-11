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

// Width returns the byte size of the string literal in memory.
// A string of Length n is stored as n CHAR values (Latin-1, 1 byte each)
// plus one null-terminator byte.
func (s StringType) Width() int {
	return (s.Length + 1) * CharType.Width()
}

// Alignment follows the element type (CHAR = 1).
func (s StringType) Alignment() int {
	return CharType.Alignment()
}

func (s StringType) Equals(t Type) bool {
	if other, ok := t.(*StringType); ok {
		return s.Length == other.Length
	}
	return false
}
