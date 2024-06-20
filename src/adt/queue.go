package adt

type Queue[T any] struct {
	f    int
	sz   int
	data []T
}

func NewQueue[T any]() *Queue[T] { return &Queue[T]{data: make([]T, Capacity)} }
func (q *Queue[T]) Size() int    { return q.sz }
func (q *Queue[T]) Empty() bool  { return q.sz == 0 }
func (q *Queue[T]) First() T {
	var first T
	if q.Empty() {
		return first
	}

	first = q.data[q.f]
	return first
}
func (q *Queue[T]) Enqueue(elem T) {
	avail := (q.f + q.sz) % len(q.data)
	q.data[avail] = elem
	q.sz++
}

func (q *Queue[T]) Dequeue() T {
	var elem T
	if q.Empty() {
		return elem
	}

	elem = q.data[q.f]
	q.f = (q.f + 1) % len(q.data)
	q.sz--

	return elem
}
