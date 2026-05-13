package regalloc

import (
	"fmt"
	"sort"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type regAllocResult struct {
	mapVRegToPReg map[string]string
	spillSlots    map[string]int
	savedRegs     []string
	scratchRegs   []string
}

type graphNode struct {
	name      string
	neighbors map[string]bool
	weight    int
	phiDest   bool
	crossCall bool
}

func colorGraph(fn *mir.Function, fa *functionAnalysis, colors, scratch []string, abi target.ABI) (*regAllocResult, error) {
	_ = fn
	nodes := make(map[string]*graphNode)
	for _, ba := range fa.blocks {
		for _, item := range ba.items {
			for _, name := range append(append([]string(nil), item.defs...), item.uses...) {
				if _, ok := nodes[name]; !ok {
					nodes[name] = &graphNode{name: name, neighbors: make(map[string]bool)}
				}
			}
			if call, ok := item.instr.(*mir.CallInstr); ok {
				for name := range item.liveOut {
					if node := nodes[name]; node != nil {
						node.crossCall = true
					}
				}
				_ = call
			}
		}
		for _, phi := range ba.phis {
			if phi != nil && phi.Dst != nil && phi.Dst.Kind == mir.VirtualReg {
				if node := nodes[phi.Dst.Name]; node != nil {
					node.phiDest = true
				}
			}
		}
	}

	for _, blk := range fa.blocks {
		bodyLive := cloneSet(blk.blockOut)
		for i := len(blk.items) - 1; i >= 0; i-- {
			item := blk.items[i]
			if _, isTerm := item.instr.(mir.Terminator); isTerm || !isPhi(item.instr) {
				for _, d := range item.defs {
					if node := nodes[d]; node != nil {
						node.weight += len(item.liveOut)
						for name := range item.liveOut {
							if name == d {
								continue
							}
							if other := nodes[name]; other != nil {
								node.neighbors[name] = true
								other.neighbors[d] = true
							}
						}
					}
				}
				for _, u := range item.uses {
					if node := nodes[u]; node != nil {
						node.weight++
					}
				}
				bodyLive = item.liveIn
			}
		}

		phiNames := make([]string, 0, len(blk.phis))
		for _, phi := range blk.phis {
			if phi == nil || phi.Dst == nil || phi.Dst.Kind != mir.VirtualReg {
				continue
			}
			phiNames = append(phiNames, phi.Dst.Name)
			if node := nodes[phi.Dst.Name]; node != nil {
				for name := range bodyLive {
					if name == phi.Dst.Name {
						continue
					}
					if other := nodes[name]; other != nil {
						node.neighbors[name] = true
						other.neighbors[phi.Dst.Name] = true
					}
				}
			}
		}
		for i := 0; i < len(phiNames); i++ {
			for j := i + 1; j < len(phiNames); j++ {
				a, b := phiNames[i], phiNames[j]
				if na, ok := nodes[a]; ok {
					na.neighbors[b] = true
				}
				if nb, ok := nodes[b]; ok {
					nb.neighbors[a] = true
				}
			}
		}
	}

	for _, name := range scratch {
		delete(nodes, name)
	}

	stack := make([]string, 0, len(nodes))
	remaining := make(map[string]bool, len(nodes))
	degree := make(map[string]int, len(nodes))
	for name, node := range nodes {
		remaining[name] = true
		degree[name] = len(node.neighbors)
	}

	order := make([]string, 0, len(nodes))
	for name := range nodes {
		order = append(order, name)
	}
	sort.Strings(order)

	allowedCount := func(name string) int {
		if nodes[name].crossCall {
			return len(calleeSavedOnly(colors, abi))
		}
		return len(colors)
	}

	for len(remaining) > 0 {
		name := pickSimplifyNode(order, remaining, degree, allowedCount)
		if name == "" {
			name = pickSpillNode(order, remaining, nodes, degree)
		}
		if name == "" {
			break
		}
		stack = append(stack, name)
		delete(remaining, name)
		for neigh := range nodes[name].neighbors {
			if remaining[neigh] {
				degree[neigh]--
			}
		}
	}

	res := &regAllocResult{
		mapVRegToPReg: make(map[string]string),
		spillSlots:    make(map[string]int),
		savedRegs:     make([]string, 0),
		scratchRegs:   append([]string(nil), scratch...),
	}

	usedSaved := make(map[string]bool)
	for i := len(stack) - 1; i >= 0; i-- {
		name := stack[i]
		node := nodes[name]
		if node == nil {
			continue
		}

		available := colors
		if node.crossCall {
			available = calleeSavedOnly(colors, abi)
			if len(available) == 0 {
				available = colors
			}
		}

		used := make(map[string]bool)
		for neigh := range node.neighbors {
			if preg, ok := res.mapVRegToPReg[neigh]; ok {
				used[preg] = true
			}
		}

		chosen := ""
		for _, preg := range available {
			if !used[preg] {
				chosen = preg
				break
			}
		}

		if chosen == "" {
			if node.phiDest {
				return nil, fmt.Errorf("register allocation: phi destination %q could not be colored", name)
			}
			res.spillSlots[name] = len(res.spillSlots)
			continue
		}

		res.mapVRegToPReg[name] = chosen
		if abi.CalleeSaved != nil && contains(abi.CalleeSaved, chosen) && !usedSaved[chosen] {
			usedSaved[chosen] = true
			res.savedRegs = append(res.savedRegs, chosen)
		}
	}

	for _, name := range scratch {
		res.mapVRegToPReg[name] = name
	}
	return res, nil
}

func buildFrameLayout(alloc *regAllocResult, abi target.ABI) *mir.FrameLayout {
	frame := mir.NewFrameLayout()
	if alloc == nil {
		return frame
	}

	frame.SavedRegs = append(frame.SavedRegs, alloc.savedRegs...)
	spillNames := make([]string, 0, len(alloc.spillSlots))
	for name := range alloc.spillSlots {
		spillNames = append(spillNames, name)
	}
	sort.Strings(spillNames)

	wordSize := abi.WordSize
	if wordSize <= 0 {
		wordSize = 8
	}
	offset := 0
	for _, name := range spillNames {
		offset -= wordSize
		frame.SpillSlots[name] = offset
	}
	frame.TotalSize = alignUp((-offset)+len(frame.SavedRegs)*wordSize, abi.Align)
	return frame
}

func alignUp(n, align int) int {
	if align <= 0 {
		return n
	}
	return (n + align - 1) / align * align
}
