package miniropt

import (
	"github.com/anthonyabeo/obx/src/ir/minir"
)

// Mem2Reg promotes non-escaping scalar allocas to SSA values, eliminating
// load/store pairs and inserting φ-nodes where control flow merges.
//
// Algorithm (Cytron et al., 1991, simplified):
//
//  1. Identify promotable allocas (non-escaping, scalar-typed).
//  2. Insert φ-nodes at the iterated dominance frontier (IDF) of each
//     alloca's definition sites (blocks that contain a StoreInst to it).
//  3. Walk the dominator tree, maintaining a per-alloca value stack:
//     • StoreInst  → push the stored value; mark for deletion.
//     • LoadInst   → replace load result with stack top; mark for deletion
//     (or replace with a materialiser instruction when the
//     reaching value is a constant and the result must remain
//     a *Temp, e.g. ReturnInst.Result).
//     • φ-node arm → fill with the current stack top before recursing into
//     each dominated successor.
//  4. Apply the substitution map (loadTemp → reachingValue) across the whole
//     function, rebuilding only instructions that reference a substituted temp.
//  5. Delete promoted AllocaInst and any remaining StoreInst / LoadInst that
//     were not already replaced by materialisers.
//
// SROA limitation: aggregate (RecordType, ArrayType) allocas are not promoted;
// they are left unchanged and are the intended target of a future SROA pass.
//
// Returns the number of allocas successfully promoted.
func Mem2Reg(fn *minir.Function) int {
	if fn == nil || fn.Entry == nil {
		return 0
	}

	// ── Step 0: collect promotable allocas ───────────────────────────────────
	eligible := collectPromotable(fn)
	if len(eligible) == 0 {
		return 0
	}

	// ── Step 1: build dominator information ──────────────────────────────────
	idom := buildIDom(fn)
	domTree := buildDomTree(idom)
	df := buildDF(fn, idom)

	// ── Step 2: map alloca.Dst → alloca for O(1) lookup ─────────────────────
	// dstToAlloca lets us identify, in O(1), which alloca an Addr temp belongs to.
	dstToAlloca := make(map[*minir.Temp]*minir.AllocaInst, len(eligible))
	for _, a := range eligible {
		dstToAlloca[a.Dst] = a
	}

	// ── Step 3: collect def-blocks per alloca (blocks with StoreInst to it) ──
	defBlocks := make(map[*minir.AllocaInst]map[*minir.Block]struct{}, len(eligible))
	for _, a := range eligible {
		defBlocks[a] = make(map[*minir.Block]struct{})
	}
	for _, b := range fn.Blocks {
		for _, ins := range b.Instrs {
			if s, ok := ins.(*minir.StoreInst); ok {
				if at, ok := s.Addr.(*minir.Temp); ok {
					if a, ok := dstToAlloca[at]; ok {
						defBlocks[a][b] = struct{}{}
					}
				}
			}
		}
	}

	// ── Step 4: place φ-nodes at IDF of each alloca's def-sites ─────────────
	// phiForAlloca[block][alloca] = the PhiInst we placed there.
	phiForAlloca := make(map[*minir.Block]map[*minir.AllocaInst]*minir.PhiInst)

	for _, a := range eligible {
		worklist := make([]*minir.Block, 0, len(defBlocks[a]))
		inWork := make(map[*minir.Block]bool)
		for b := range defBlocks[a] {
			worklist = append(worklist, b)
			inWork[b] = true
		}
		placed := make(map[*minir.Block]bool)

		for len(worklist) > 0 {
			b := worklist[len(worklist)-1]
			worklist = worklist[:len(worklist)-1]
			inWork[b] = false

			for _, y := range df[b] {
				if placed[y] {
					continue
				}
				// Never place φ-nodes in the exit block: it is a sentinel with
				// only a void ret, and doing so creates dead phis for every
				// variable defined on only some return paths.
				if fn.Exit != nil && y.ID == fn.Exit.ID {
					continue
				}
				// Insert a φ-node for alloca a at the top of block y.
				phi := &minir.PhiInst{
					Dst:  minir.NewAnonTemp(a.AllocType),
					Args: make([]minir.PhiArm, 0, len(y.Preds)),
				}
				for _, pred := range y.SortedPreds() {
					phi.Args = append(phi.Args, minir.PhiArm{
						BlockLabel: pred.Label,
						Val:        zeroForType(a.AllocType),
					})
				}
				// Prepend the φ-node (phis must precede non-phi instructions).
				y.Instrs = append([]minir.Instr{phi}, y.Instrs...)

				if phiForAlloca[y] == nil {
					phiForAlloca[y] = make(map[*minir.AllocaInst]*minir.PhiInst)
				}
				phiForAlloca[y][a] = phi
				placed[y] = true

				// y becomes a new def-site.
				if _, alreadyDef := defBlocks[a][y]; !alreadyDef {
					if !inWork[y] {
						worklist = append(worklist, y)
						inWork[y] = true
					}
				}
			}
		}
	}

	// ── Step 5: rename — dominator-tree walk ─────────────────────────────────
	// subst: loadResult *Temp → reaching Value (set when the load is deleted).
	// toDelete: instructions marked for removal.
	subst := make(map[*minir.Temp]minir.Value)
	toDelete := make(map[minir.Instr]bool)

	// stacks[a] is the value stack for alloca a; top is the last element.
	stacks := make(map[*minir.AllocaInst][]minir.Value, len(eligible))

	var renameBlock func(b *minir.Block)
	renameBlock = func(b *minir.Block) {
		// Snapshot stack sizes so we can restore them on exit.
		snapshots := make(map[*minir.AllocaInst]int, len(stacks))
		for a, s := range stacks {
			snapshots[a] = len(s)
		}

		// (a) φ-nodes placed at the top of this block define new values.
		if phisHere, ok := phiForAlloca[b]; ok {
			for a, phi := range phisHere {
				stacks[a] = append(stacks[a], phi.Dst)
			}
		}

		// (b) Walk instructions, handling loads and stores.
		for i, ins := range b.Instrs {
			if _, isPhi := ins.(*minir.PhiInst); isPhi {
				continue // phi nodes already handled above
			}

			switch instr := ins.(type) {
			case *minir.LoadInst:
				addrTemp, ok := instr.Addr.(*minir.Temp)
				if !ok {
					continue
				}
				a, ok := dstToAlloca[addrTemp]
				if !ok {
					continue // not a promoted alloca
				}
				reaching := topOrZero(stacks[a], a.AllocType)
				if t, ok := reaching.(*minir.Temp); ok {
					// Direct temp: record substitution and delete the load.
					subst[instr.Dst] = t
					toDelete[ins] = true
				} else {
					// Constant: replace the load with a materialiser so that the
					// load's Dst *Temp remains a valid SSA definition (needed in
					// positions like ReturnInst.Result where only *Temp is allowed).
					b.Instrs[i] = materializeConst(instr.Dst, reaching)
					// The old LoadInst pointer is no longer in Instrs; toDelete
					// refers to the original pointer which won't be found.
				}

			case *minir.StoreInst:
				addrTemp, ok := instr.Addr.(*minir.Temp)
				if !ok {
					continue
				}
				a, ok := dstToAlloca[addrTemp]
				if !ok {
					continue
				}
				stacks[a] = append(stacks[a], instr.Val)
				toDelete[ins] = true
			}
		}

		// (c) Fill φ-node arms for each promotable alloca in successors.
		for _, succ := range b.SortedSuccs() {
			phisInSucc, ok := phiForAlloca[succ]
			if !ok {
				continue
			}
			for a, phi := range phisInSucc {
				reaching := topOrZero(stacks[a], a.AllocType)
				for i, arm := range phi.Args {
					if arm.BlockLabel == b.Label {
						phi.Args[i].Val = reaching
					}
				}
			}
		}

		// (d) Recurse into dominator-tree children.
		for _, child := range domTree[b] {
			renameBlock(child)
		}

		// (e) Restore stacks to the snapshot taken at entry to this block.
		for a, savedLen := range snapshots {
			stacks[a] = stacks[a][:savedLen]
		}
	}

	renameBlock(fn.Entry)

	// ── Step 6: apply substitutions across all blocks ─────────────────────────
	applySubst(fn, subst, toDelete)

	// ── Step 7: remove promoted AllocaInst nodes ──────────────────────────────
	for _, a := range eligible {
		toDelete[a] = true
	}
	for _, b := range fn.Blocks {
		filtered := b.Instrs[:0]
		for _, ins := range b.Instrs {
			if !toDelete[ins] {
				filtered = append(filtered, ins)
			}
		}
		b.Instrs = filtered
		// Refresh b.Term (the last instruction is still the terminator).
		if n := len(b.Instrs); n > 0 {
			if t, ok := b.Instrs[n-1].(minir.Terminator); ok {
				b.Term = t
			}
		}
	}

	return len(eligible)
}

// ── helpers ───────────────────────────────────────────────────────────────────

// topOrZero returns the top value on the per-alloca stack, or a zero constant
// of the alloca's element type when the stack is empty (the variable is used
// on a path where no store dominates the load — undefined behaviour in
// well-typed source, but we produce something verifier-legal here).
func topOrZero(stack []minir.Value, ty minir.Type) minir.Value {
	if len(stack) > 0 {
		return stack[len(stack)-1]
	}
	return zeroForType(ty)
}

// zeroForType builds a zero constant appropriate for ty.
func zeroForType(ty minir.Type) minir.Value {
	switch t := ty.(type) {
	case *minir.PrimitiveType:
		switch t.String() {
		case "i1":
			return minir.NewConst("false", int64(0), ty)
		case "f32", "f64":
			return minir.NewConst("0.0", float64(0), ty)
		default:
			return minir.NewConst("0", int64(0), ty)
		}
	case *minir.PointerType:
		return minir.NewConst("null", int64(0), ty)
	default:
		return minir.NewConst("0", int64(0), ty)
	}
}

// materializeConst replaces a LoadInst with a BinaryInst that copies the
// constant value into the same destination temp.  This preserves the temp as a
// valid SSA definition so callers (e.g. ReturnInst.Result) keep a *Temp.
// The result is trivially constant-foldable in the next optimisation pass.
func materializeConst(dst *minir.Temp, v minir.Value) minir.Instr {
	ty := dst.Type()
	if pt, ok := ty.(*minir.PrimitiveType); ok && pt.String() == "i1" {
		return &minir.BinaryInst{
			Dst:   dst,
			Op:    "xor",
			Left:  v,
			Right: minir.NewConst("false", int64(0), ty),
		}
	}
	return &minir.BinaryInst{
		Dst:   dst,
		Op:    "add",
		Left:  v,
		Right: minir.NewConst("0", int64(0), ty),
	}
}

// applySubst rewrites every instruction in fn, replacing occurrences of temp
// keys in subst with their mapped values.  Instructions listed in toDelete are
// dropped.  b.Term is updated to the new last instruction when it changed.
func applySubst(
	fn *minir.Function,
	subst map[*minir.Temp]minir.Value,
	toDelete map[minir.Instr]bool,
) {
	if len(subst) == 0 && len(toDelete) == 0 {
		return
	}
	for _, b := range fn.Blocks {
		var rebuilt []minir.Instr
		changed := false
		for _, ins := range b.Instrs {
			if toDelete[ins] {
				changed = true
				continue
			}
			replaced := replaceInInstr(ins, subst)
			rebuilt = append(rebuilt, replaced)
			if replaced != ins {
				changed = true
			}
		}
		if !changed {
			continue
		}
		b.Instrs = rebuilt
		if n := len(rebuilt); n > 0 {
			if t, ok := rebuilt[n-1].(minir.Terminator); ok {
				b.Term = t
			}
		}
	}
}

// replaceInInstr returns a version of ins where every use in subst has been
// replaced with its mapped value.  Returns the original pointer when nothing
// changed (no allocation).
//
// For instruction fields typed as *Temp (ReturnInst.Result, CondBrInst.Cond,
// SwitchInst.Key) we only substitute *Temp → *Temp to avoid introducing a
// type mismatch; constant reaching values are handled upstream by
// materializeConst which ensures those temps remain valid SSA definitions.
func replaceInInstr(ins minir.Instr, subst map[*minir.Temp]minir.Value) minir.Instr {
	if len(subst) == 0 {
		return ins
	}

	sub := func(v minir.Value) minir.Value {
		if t, ok := v.(*minir.Temp); ok {
			if r, found := subst[t]; found {
				return r
			}
		}
		return v
	}

	// For positions that only accept *Temp, only allow *Temp replacements.
	subTemp := func(t *minir.Temp) *minir.Temp {
		if t == nil {
			return nil
		}
		if r, found := subst[t]; found {
			if rt, ok := r.(*minir.Temp); ok {
				return rt
			}
		}
		return t
	}

	switch i := ins.(type) {
	case *minir.BinaryInst:
		l, r := sub(i.Left), sub(i.Right)
		if l == i.Left && r == i.Right {
			return ins
		}
		return &minir.BinaryInst{Dst: i.Dst, Op: i.Op, Left: l, Right: r}

	case *minir.ICmpInst:
		l, r := sub(i.Left), sub(i.Right)
		if l == i.Left && r == i.Right {
			return ins
		}
		return &minir.ICmpInst{Dst: i.Dst, Pred: i.Pred, Left: l, Right: r}

	case *minir.FCmpInst:
		l, r := sub(i.ICmpInst.Left), sub(i.ICmpInst.Right)
		if l == i.ICmpInst.Left && r == i.ICmpInst.Right {
			return ins
		}
		return &minir.FCmpInst{ICmpInst: minir.ICmpInst{
			Dst: i.ICmpInst.Dst, Pred: i.ICmpInst.Pred, Left: l, Right: r,
		}}

	case *minir.UnaryInst:
		s := sub(i.Src)
		if s == i.Src {
			return ins
		}
		return &minir.UnaryInst{Dst: i.Dst, Op: i.Op, Src: s}

	case *minir.CastInst:
		s := sub(i.Src)
		if s == i.Src {
			return ins
		}
		return &minir.CastInst{Dst: i.Dst, Op: i.Op, Src: s}

	case *minir.LoadInst:
		// A load of a non-promoted alloca; substitute the address if needed.
		a := sub(i.Addr)
		if a == i.Addr {
			return ins
		}
		return &minir.LoadInst{Dst: i.Dst, Addr: a}

	case *minir.StoreInst:
		v, a := sub(i.Val), sub(i.Addr)
		if v == i.Val && a == i.Addr {
			return ins
		}
		return &minir.StoreInst{Val: v, Addr: a}

	case *minir.CallInst:
		changed := false
		args := make([]minir.Value, len(i.Args))
		for idx, a := range i.Args {
			args[idx] = sub(a)
			if args[idx] != a {
				changed = true
			}
		}
		if !changed {
			return ins
		}
		return &minir.CallInst{Dst: i.Dst, Callee: i.Callee, Args: args}

	case *minir.PhiInst:
		changed := false
		args := make([]minir.PhiArm, len(i.Args))
		for idx, arm := range i.Args {
			nv := sub(arm.Val)
			args[idx] = minir.PhiArm{BlockLabel: arm.BlockLabel, Val: nv}
			if nv != arm.Val {
				changed = true
			}
		}
		if !changed {
			return ins
		}
		return &minir.PhiInst{Dst: i.Dst, Args: args}

	case *minir.GEPInst:
		base := sub(i.Base)
		changed := base != i.Base
		idxs := make([]minir.Value, len(i.Indices))
		for idx, v := range i.Indices {
			idxs[idx] = sub(v)
			if idxs[idx] != v {
				changed = true
			}
		}
		if !changed {
			return ins
		}
		return &minir.GEPInst{
			Dst:      i.Dst,
			Base:     base,
			ElemType: i.ElemType,
			Offsets:  i.Offsets,
			Indices:  idxs,
		}

	case *minir.ReturnInst:
		// ReturnInst.Result is typed *Temp; only *Temp → *Temp substitutions apply.
		nr := subTemp(i.Result)
		if nr == i.Result {
			return ins
		}
		return &minir.ReturnInst{Result: nr}

	case *minir.CondBrInst:
		// CondBrInst.Cond is *Temp.
		nc := subTemp(i.Cond)
		if nc == i.Cond {
			return ins
		}
		return &minir.CondBrInst{Cond: nc, TrueLabel: i.TrueLabel, FalseLabel: i.FalseLabel}

	case *minir.SwitchInst:
		// SwitchInst.Key is *Temp.
		nk := subTemp(i.Key)
		if nk == i.Key {
			return ins
		}
		return &minir.SwitchInst{Key: nk, Default: i.Default, Arms: i.Arms}

	case *minir.HaltInst:
		if i.Code == nil {
			return ins
		}
		nc := sub(i.Code)
		if nc == i.Code {
			return ins
		}
		return &minir.HaltInst{Code: nc}

	default:
		// JumpInst, AllocaInst (already removed) — no live value operands.
		return ins
	}
}
