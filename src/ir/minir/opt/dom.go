// Package miniropt provides optimisation passes that operate on the minir IR.
//
// It is intentionally separate from the minir package (IR definitions) for the
// same reason src/opt is separate from src/ir/obxir: keeping transformations
// out of the IR definition package avoids import cycles as the pass set grows
// and makes the responsibility of each package unambiguous.
package miniropt

import (
	"sort"

	"github.com/anthonyabeo/obx/src/ir/minir"
)

// ── Dominator utilities ───────────────────────────────────────────────────────
//
// We use the simple, efficient Cooper–Harvey–Kennedy algorithm
// ("A Simple, Fast Dominance Algorithm", 2001) that works in reverse
// post-order (RPO) and converges in a small, bounded number of iterations
// on typical CFGs.

// rpo returns blocks in reverse post-order starting from fn.Entry.
// Unreachable blocks are omitted.
func rpo(fn *minir.Function) []*minir.Block {
	ids := fn.ReversePostOrder() // already available on minir.Function
	out := make([]*minir.Block, 0, len(ids))
	for _, id := range ids {
		if b, ok := fn.Blocks[id]; ok {
			out = append(out, b)
		}
	}
	return out
}

// buildIDom computes the immediate dominator of every reachable block using
// the Cooper et al. RPO-based intersect.
//
// Convention: idom[entry] == entry (used as a sentinel so the DF loop
// terminates cleanly); callers that need "entry has no idom" should compare
// against fn.Entry themselves.
func buildIDom(fn *minir.Function) map[*minir.Block]*minir.Block {
	if fn.Entry == nil {
		return nil
	}

	order := rpo(fn)
	rpoNum := make(map[*minir.Block]int, len(order))
	for i, b := range order {
		rpoNum[b] = i
	}

	idom := make(map[*minir.Block]*minir.Block, len(order))
	idom[fn.Entry] = fn.Entry // sentinel

	// intersect: walk up idom chains from b1 and b2 until they meet.
	var intersect func(b1, b2 *minir.Block) *minir.Block
	intersect = func(b1, b2 *minir.Block) *minir.Block {
		for b1 != b2 {
			for rpoNum[b1] > rpoNum[b2] {
				b1 = idom[b1]
			}
			for rpoNum[b2] > rpoNum[b1] {
				b2 = idom[b2]
			}
		}
		return b1
	}

	changed := true
	for changed {
		changed = false
		for _, b := range order {
			if b == fn.Entry {
				continue
			}
			preds := b.SortedPreds()
			if len(preds) == 0 {
				continue
			}

			// Pick the first already-processed predecessor as the initial idom.
			var newIdom *minir.Block
			for _, p := range preds {
				if idom[p] != nil {
					newIdom = p
					break
				}
			}
			if newIdom == nil {
				continue
			}

			for _, p := range preds {
				if p == newIdom {
					continue
				}
				if idom[p] != nil {
					newIdom = intersect(newIdom, p)
				}
			}

			if idom[b] != newIdom {
				idom[b] = newIdom
				changed = true
			}
		}
	}

	return idom
}

// buildDomTree returns the dominator tree as parent → sorted children.
// Entry appears in the map with an empty children list (it has no parent).
func buildDomTree(idom map[*minir.Block]*minir.Block) map[*minir.Block][]*minir.Block {
	tree := make(map[*minir.Block][]*minir.Block, len(idom))
	for b, parent := range idom {
		if tree[b] == nil {
			tree[b] = nil // ensure every block has an entry
		}
		if parent != nil && parent != b { // skip the entry sentinel
			tree[parent] = append(tree[parent], b)
		}
	}
	for _, children := range tree {
		sort.Slice(children, func(i, j int) bool {
			return children[i].ID < children[j].ID
		})
	}
	return tree
}

// buildDF computes the dominance frontier (DF) of each block.
// A block y is in DF[x] if x dominates a predecessor of y but x does not
// strictly dominate y.
func buildDF(fn *minir.Function, idom map[*minir.Block]*minir.Block) map[*minir.Block][]*minir.Block {
	df := make(map[*minir.Block][]*minir.Block, len(fn.Blocks))

	for _, b := range fn.Blocks {
		preds := b.SortedPreds()
		if len(preds) < 2 {
			continue // only join blocks contribute to DF
		}
		domOfB := idom[b] // == sentinel entry when b == entry, else true idom
		for _, p := range preds {
			runner := p
			// Walk up the idom chain from p until we reach idom(b).
			// idom[entry] == entry so the loop terminates when runner reaches
			// the common dominator.
			for runner != domOfB && runner != nil {
				if !containsBlock(df[runner], b) {
					df[runner] = append(df[runner], b)
				}
				parent := idom[runner]
				if parent == runner { // sentinel: entry's idom is itself
					break
				}
				runner = parent
			}
		}
	}

	return df
}

// containsBlock is an O(n) membership test for small frontier sets.
func containsBlock(slice []*minir.Block, b *minir.Block) bool {
	for _, s := range slice {
		if s == b {
			return true
		}
	}
	return false
}
