package adt

import "fmt"

type Set[T comparable] interface {
	Union(Set[T]) Set[T]
	Intersect(Set[T]) Set[T]
	Diff(Set[T]) Set[T]
	Size() int
	Add(...T)
	Remove(...T) Set[T]
	Contains(T) bool
	Clone() Set[T]
	Empty() bool
	Elems() []T
	Pop() T
	Equal(Set[T]) bool
}

// HashSet
// -------------------------------------------------------------
type HashSet[T comparable] struct {
	elems map[T]bool
}

func NewHashSet[T comparable]() *HashSet[T] {
	return &HashSet[T]{elems: make(map[T]bool)}
}

func (h *HashSet[T]) Elems() []T {
	var elems []T
	for elem := range h.elems {
		elems = append(elems, elem)
	}

	return elems
}

func (h *HashSet[T]) Union(other Set[T]) Set[T] {
	set := NewHashSet[T]()
	for elem := range h.elems {
		set.Add(elem)
	}
	for _, elem := range other.Elems() {
		set.Add(elem)
	}

	return set
}

func (h *HashSet[T]) Intersect(s Set[T]) Set[T] {
	in := NewHashSet[T]()
	if h.Size() > s.Size() {
		s, ok := s.(*HashSet[T])
		if !ok {
			panic(fmt.Sprintf("expected an argument of type 'HashSet', got '%T'", s))
		}

		h, s = s, h
	}
	for bb, exist := range h.elems {
		if _, exists := h.elems[bb]; exists {
			in.elems[bb] = exist
		}
	}

	return in
}

func (h *HashSet[T]) Diff(other Set[T]) Set[T] {
	diff := NewHashSet[T]()
	for elem := range h.elems {
		if !other.Contains(elem) {
			diff.elems[elem] = true
		}
	}

	return diff
}

func (h *HashSet[T]) Size() int { return len(h.elems) }

func (h *HashSet[T]) Add(s ...T) {
	for _, blk := range s {
		if _, exists := h.elems[blk]; !exists {
			h.elems[blk] = true
		}
	}
}

func (h *HashSet[T]) Remove(s ...T) Set[T] {
	for _, value := range s {
		delete(h.elems, value)
	}

	return h
}

func (h *HashSet[T]) Empty() bool { return len(h.elems) == 0 }

func (h *HashSet[T]) Contains(t T) bool { return h.elems[t] }

func (h *HashSet[T]) Clone() Set[T] {
	target := NewHashSet[T]()

	for key, value := range h.elems {
		target.elems[key] = value
	}

	return target
}

func (h *HashSet[T]) Pop() T {
	var t T
	for key := range h.elems {
		t = key
		delete(h.elems, key)
		break
	}

	return t
}

func (h *HashSet[T]) Equal(other Set[T]) bool {
	if h.Size() != other.Size() {
		return false
	}

	for name := range h.elems {
		if !other.Contains(name) {
			return false
		}
	}

	return true
}
