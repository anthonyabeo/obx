package adt

type Queue[T any] struct {
	data []T
}

func NewQueueFrom[T any](data []T) *Queue[T] {
	return &Queue[T]{data: data}
}

func NewQueue[T any]() *Queue[T] { return &Queue[T]{data: make([]T, 0)} }

func (q *Queue[T]) Size() int { return len(q.data) }

func (q *Queue[T]) Empty() bool { return len(q.data) == 0 }

func (q *Queue[T]) First() T {
	var first T
	if q.Empty() {
		return first
	}

	first = q.data[0]
	return first
}

func (q *Queue[T]) Enqueue(elem T) {
	q.data = append(q.data, elem)
}

func (q *Queue[T]) Dequeue() T {
	elem := q.data[0]
	q.data = q.data[1:]

	return elem
}
