package opt

import "github.com/anthonyabeo/obx/src/ir/mir"

func SSAConstruct(fn *mir.Function) {
	// Step 1: Build the control flow graph (CFG)
	BuildCFG(fn)

	// Step 2: Compute dominators
	ComputeDom(fn)

	// Step 3: Place φ-nodes for each variable
	PlacePhiNodes(fn)

	// Step 4: Rename variables in φ-nodes and instructions
	RenamePhiNodes(fn)
}

// PlacePhiNodes places φ-nodes for all variables using DF and def-sites
func PlacePhiNodes(fn *mir.Function) {
	// For each variable independently
	for v, defs := range fn.SSAInfo.DefSites {
		// Worklist initially contains all definition blocks
		worklist := make([]*mir.Block, 0, len(defs))
		inWork := make(map[*mir.Block]bool) // avoid duplicates in queue
		for b := range defs {
			worklist = append(worklist, b)
			inWork[b] = true
		}

		placed := make(map[*mir.Block]bool) // track where we've placed phi for v

		for len(worklist) > 0 {
			// pop last
			n := worklist[len(worklist)-1]
			worklist = worklist[:len(worklist)-1]
			inWork[n] = false

			for _, y := range fn.Dom.DF[n] {
				if !placed[y] && !y.HasPhi(v) {
					value, found := fn.Env.Lookup(v)
					if !found {
						continue
					}

					phi := &mir.PhiInst{Target: value, Args: make([]*mir.PHIArg, 0, len(y.Preds))}
					// Add all predecessors of y as arguments to the phi node
					for _, pred := range y.Preds {
						phi.AddArg(pred, value)
					}

					fn.SSAInfo.PhiFuncs[y.Label] = append(fn.SSAInfo.PhiFuncs[y.Label], phi)

					y.AddPhi(phi)
					placed[y] = true
					// If y doesn't define v, it becomes a def-site in the iterative algorithm
					// so we must propagate from y as well.
					definesV := false
					for d := range defs {
						if d == y {
							definesV = true
							break
						}
					}
					if !definesV && !inWork[y] {
						worklist = append(worklist, y)
						inWork[y] = true
					}
				}
			}
		}
	}
}

func RenamePhiNodes(fn *mir.Function) {
	vst := make(map[int]bool)
	rename(fn, fn.Entry, vst)

}

func rename(fn *mir.Function, block *mir.Block, vst map[int]bool) {
	if vst[block.ID] {
		return
	}
	vst[block.ID] = true

	for _, phi := range fn.SSAInfo.PhiFuncs[block.Label] {
		fn.SSAInfo.Push(phi.Target.BaseName())
		phi.ReplaceDef(fn.SSAInfo.NewValue(phi.Target))
	}

	for _, ins := range block.Instrs {
		if _, ok := ins.(*mir.PhiInst); ok {
			continue
		}

		// replace uses
		uses := ins.Uses()
		subst := map[string]mir.Value{}
		for _, u := range uses {
			subst[u.BaseName()] = fn.SSAInfo.NewValue(u)
		}
		ins.ReplaceUses(subst)

		// defs -> create new name and set def to that
		def := ins.Def()
		if def != nil {
			fn.SSAInfo.Push(def.BaseName())
			Val := fn.SSAInfo.NewValue(def)
			ins.ReplaceDef(Val)
		}
	}

	for _, s := range block.Succs {
		// for every phi in successor
		for _, ins := range s.Instrs {
			if p, ok := ins.(*mir.PhiInst); ok {
				for _, arg := range p.Args {
					if arg.Block.ID == block.ID {
						// arg.Block is the predecessor block
						// we need to rename arg.Value to the current SSA name
						value := fn.SSAInfo.NewValue(arg.Value)
						arg.Value = value
					}
				}
			}
		}
	}

	for _, b := range fn.Dom.DomTree[block] {
		rename(fn, b, vst)
	}

	for i := len(block.Instrs) - 1; i >= 0; i-- {
		// d is SSA name we created for this block; pop its base var
		if block.Instrs[i].Def() != nil {
			base := block.Instrs[i].Def().BaseName()
			fn.SSAInfo.Pop(base)
		}
	}
}
