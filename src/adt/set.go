package adt

import (
	"fmt"
	"golang.org/x/exp/constraints"
	"math"
)

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
	for bb := range h.elems {
		if s.Contains(bb) {
			in.elems[bb] = true
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

// BitVector
// -----------------------------------------------
type BitVector[T constraints.Unsigned] struct {
	set T
}

func (b BitVector[T]) Union(s Set[T]) Set[T] {
	bits, ok := s.(BitVector[T])
	if !ok {
		panic(fmt.Sprintf("expected an argument of type 'BitVector', got '%T'", s))
	}

	return BitVector[T]{set: b.set | bits.set}
}

func (b BitVector[T]) Intersect(s Set[T]) Set[T] {
	bits, ok := s.(BitVector[T])
	if !ok {
		panic(fmt.Sprintf("expected an argument of type 'BitVector', got '%T'", s))
	}

	return BitVector[T]{set: b.set & bits.set}
}

func (b BitVector[T]) Diff(s Set[T]) Set[T] {
	bits, ok := s.(BitVector[T])
	if !ok {
		panic(fmt.Sprintf("expected an argument of type 'BitVector', got '%T'", s))
	}

	return BitVector[T]{set: b.set & ^bits.set}
}

func (b BitVector[T]) Size() int {
	b.set = b.set - ((b.set >> 1) & 0x55555555)
	b.set = (b.set & 0x33333333) + ((b.set >> 2) & 0x33333333)
	b.set = (b.set + (b.set >> 4)) & 0x0F0F0F0F
	b.set = b.set + (b.set >> 8)
	b.set = b.set + (b.set >> 16)

	return int(b.set & 0x0000003F)
}

func (b BitVector[T]) Add(t ...T) {
	for _, k := range t {
		b.set = b.set | (1 << k)
	}
}

func (b BitVector[T]) Remove(t ...T) Set[T] {
	for _, k := range t {
		b.set = b.set & ^(1 << k)
	}

	return b
}

func (b BitVector[T]) Contains(t T) bool {
	return b.set&(1<<(t-1)) != 0
}

func (b BitVector[T]) Clone() Set[T] {
	return BitVector[T]{set: b.set}
}

func (b BitVector[T]) Empty() bool {
	return b.set == 0
}

func (b BitVector[T]) Elems() (elems []T) {
	temp := b.set
	for temp != 0 {
		t := T(math.Log2(float64(temp & (^temp + 1))))
		temp = temp & (temp - 1)
		elems = append(elems, t)
	}

	return
}

func (b BitVector[T]) Pop() T {
	t := T(math.Log2(float64(b.set & (^b.set + 1))))
	b.set = b.set & (b.set - 1)

	return t
}

func (b BitVector[T]) Equal(s Set[T]) bool {
	bits, ok := s.(BitVector[T])
	if !ok {
		panic(fmt.Sprintf("expected an argument of type 'BitVector', got '%T'", s))
	}

	return b.set == bits.set
}
