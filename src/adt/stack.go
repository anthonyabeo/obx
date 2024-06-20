package adt

const Capacity = 8

type Stack[T any] struct {
	Items []T
	t     int
}

func NewStack[T any]() *Stack[T] { return &Stack[T]{Items: make([]T, Capacity)} }

func (s *Stack[T]) Empty() bool { return s.t == -1 }
func (s *Stack[T]) Size() int   { return s.t + 1 }

func (s *Stack[T]) Top() T {
	var data T

	if s.Empty() {
		return data
	}

	data = s.Items[s.t]
	return data
}

func (s *Stack[T]) Pop() T {
	var data T
	if s.Empty() {
		return data
	}

	data = s.Items[s.t]
	s.t--

	return data
}

func (s *Stack[T]) Push(item T) {
	s.t++
	s.Items[s.t] = item
}
