package ir

func TrvQueue(entry *BasicBlock) (list []*BasicBlock) {
	visited := make(map[string]bool, 0)

	q := make([]*BasicBlock, 0)
	q = append(q, entry)
	list = append(list, entry)

	visited[entry.name] = true

	// while the queue is not empty
	for len(q) != 0 {
		// dequeue the first item
		blk := q[0]
		q = q[1:]

		// for each of its successors, if it's not visited, do something with it.
		// then append it to the queue
		for _, succ := range blk.succ {
			if !visited[succ.name] {
				q = append(q, succ)
				list = append(list, succ)
				visited[succ.name] = true
			}
		}
	}

	return
}
