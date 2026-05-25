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
	// Build pre-coloring for function parameters mapped to ABI argument registers.
	// Integer and floating-point parameters use independent index counters so
	// that mixed signatures (e.g., func(int, float, int)) map correctly.
	precolored := make(map[string]string)
	intIdx, floatIdx := 0, 0
	for _, param := range fn.Params {
		if param == nil {
			continue
		}
		if target.IsFloatType(param.Type) {
			if reg, ok := abi.FloatArgReg(floatIdx); ok {
				precolored[param.Name] = reg
			}
			// Params beyond FloatArgRegs are stack-passed; no pre-coloring needed.
			floatIdx++
		} else {
			if reg, ok := abi.ArgReg(intIdx); ok {
				precolored[param.Name] = reg
			}
			// Params beyond IntArgRegs are stack-passed; no pre-coloring needed.
			intIdx++
		}
	}

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

	// Remove pre-colored parameter nodes from the simplification work-list.
	// They are already assigned to their ABI registers and must not be re-colored.
	// Adjust neighbor degrees as if the pre-colored node had been simplified.
	for name := range precolored {
		if !remaining[name] {
			continue
		}
		delete(remaining, name)
		if node, ok := nodes[name]; ok {
			for neigh := range node.neighbors {
				if remaining[neigh] {
					degree[neigh]--
				}
			}
		}
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

	// Seed the result with pre-colored parameters so that neighbor color
	// selection naturally avoids these registers.
	for name, preg := range precolored {
		res.mapVRegToPReg[name] = preg
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

// collectSurvivingAllocas gathers all AllocaInstr in the function.
// Returns a map of alloca destination register names to their sizes.
func collectSurvivingAllocas(fn *mir.Function) map[string]int {
	allocas := make(map[string]int)
	if fn == nil {
		return allocas
	}
	for _, block := range fn.Blocks {
		if block == nil {
			continue
		}
		for _, instr := range block.Instrs {
			if alloca, ok := instr.(*mir.AllocaInstr); ok && alloca != nil && alloca.Dst != nil {
				allocas[alloca.Dst.Name] = alloca.Size
			}
		}
	}
	return allocas
}

// functionHasCalls reports whether fn contains at least one call instruction.
// A non-leaf function must save/restore the link register (bl overwrites it).
func functionHasCalls(fn *mir.Function, abi target.ABI) {
	for _, block := range fn.Blocks {
		if block == nil {
			continue
		}
		for _, instr := range block.Instrs {
			if _, ok := instr.(*mir.CallInstr); ok {
				fn.HasCalls = true
			}

			if instr, ok := instr.(*mir.MachineInstr); ok {
				switch abi.Name {
				case "AAPCS64":
					if instr.Op == "bl" || instr.Op == "blr" {
						fn.HasCalls = true
						break
					}
				case "LP64D":
					if instr.Op == "bl" {
						fn.HasCalls = true
						break
					}
				}
			}
		}
	}
	//fn.HasCalls = false
}

func buildFrameLayout(alloc *regAllocResult, fn *mir.Function, abi target.ABI) *mir.FrameLayout {
	frame := mir.NewFrameLayout()
	if alloc == nil {
		return frame
	}

	// Collect surviving allocas from the function
	allocas := collectSurvivingAllocas(fn)
	functionHasCalls(fn, abi)

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

	// Layout allocas first (at negative offsets from frame pointer)
	offset := 0
	allocaNames := make([]string, 0, len(allocas))
	for name := range allocas {
		allocaNames = append(allocaNames, name)
	}
	sort.Strings(allocaNames)

	for _, name := range allocaNames {
		size := allocas[name]
		if size <= 0 {
			size = wordSize
		}
		offset -= size
		frame.AllocaSlots[name] = offset
	}

	// Layout spill slots below allocas
	for _, name := range spillNames {
		offset -= wordSize
		frame.SpillSlots[name] = offset
	}

	// Compute total frame size: allocas + spills + saved registers
	allocaSize := 0
	for _, size := range allocas {
		if size <= 0 {
			allocaSize += wordSize
		} else {
			allocaSize += size
		}
	}
	spillSize := len(spillNames) * wordSize
	savedSize := len(frame.SavedRegs) * wordSize

	frame.TotalSize = alignUp(allocaSize+spillSize+savedSize, abi.Align)

	// non-leaf functions that emit a `bl`/`jal` clobber the link
	// register.  The ABI requires us to save lr (and fp) in the prologue; the
	// frame must be at least large enough for that pair even when there are no
	// spills or surviving allocas.
	if fn != nil && fn.HasCalls && abi.LinkRegister != "" {
		minSize := alignUp(2*wordSize, abi.Align)
		if frame.TotalSize < minSize {
			frame.TotalSize = minSize
		}
	}

	return frame
}

func alignUp(n, align int) int {
	if align <= 0 {
		return n
	}
	return (n + align - 1) / align * align
}
