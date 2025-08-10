package opt

import "github.com/anthonyabeo/obx/src/ir/mir"

func filterBlocks(blocks map[int]*mir.Block, reachable map[int]bool) map[int]*mir.Block {
	out := make(map[int]*mir.Block)
	for id, b := range blocks {
		if reachable[id] {
			out[id] = b
		}
	}
	return out
}
